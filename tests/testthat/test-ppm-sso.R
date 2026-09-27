local_token_path <- function(envir = parent.frame()) {
  tmp <- withr::local_tempdir(.local_envir = envir)
  path <- file.path(tmp, ".ppm", "tokens.toml")
  fake(
    ppm_sso_write_token_to_file,
    "ppm_sso_token_path",
    function() path,
    envir
  )
  path
}

read_connections <- function(path) {
  tokens <- suppressWarnings(tstoml::ts_read_toml(path))
  tsitter::ts_tree_unserialize(
    tsitter::ts_tree_select(tokens, list("connections", TRUE))
  )
}

test_that("ppm_sso_write_token_to_file: token file does not exist", {
  path <- local_token_path()
  expect_false(file.exists(path))

  ppm_sso_write_token_to_file("https://ppm.example.com", "tkn1")

  expect_true(file.exists(path))
  conns <- read_connections(path)
  expect_equal(
    conns,
    list(list(
      address = "https://ppm.example.com",
      token = "tkn1",
      auth_type = "sso"
    ))
  )
})

test_that("ppm_sso_write_token_to_file: token file is empty", {
  path <- local_token_path()
  mkdirp(dirname(path))
  file.create(path)

  ppm_sso_write_token_to_file("https://ppm.example.com", "tkn1")

  conns <- read_connections(path)
  expect_equal(
    conns,
    list(list(
      address = "https://ppm.example.com",
      token = "tkn1",
      auth_type = "sso"
    ))
  )
})

test_that("ppm_sso_write_token_to_file: non-empty file, creating connections table", {
  path <- local_token_path()
  mkdirp(dirname(path))
  writeLines(
    c(
      "top_level = \"keep me\"",
      "",
      "[meta]",
      "version = 1"
    ),
    path
  )

  ppm_sso_write_token_to_file("https://ppm.example.com", "tkn1")

  tokens <- suppressWarnings(tstoml::ts_read_toml(path))
  conns <- tsitter::ts_tree_unserialize(
    tsitter::ts_tree_select(tokens, list("connections", TRUE))
  )
  expect_equal(
    conns,
    list(list(
      address = "https://ppm.example.com",
      token = "tkn1",
      auth_type = "sso"
    ))
  )
  expect_equal(
    tsitter::ts_tree_unserialize(tsitter::ts_tree_select(tokens, "top_level"))[[
      1
    ]],
    "keep me"
  )
  expect_equal(
    tsitter::ts_tree_unserialize(
      tsitter::ts_tree_select(tokens, list("meta", "version"))
    )[[1]],
    1L
  )
})

test_that("ppm_sso_write_token_to_file: appending to existing connections table", {
  path <- local_token_path()
  mkdirp(dirname(path))
  writeLines(
    c(
      "[[connections]]",
      "address = \"https://other.example.com\"",
      "token = \"other-tkn\"",
      "auth_type = \"sso\""
    ),
    path
  )

  ppm_sso_write_token_to_file("https://ppm.example.com", "tkn1")

  conns <- read_connections(path)
  expect_equal(
    conns,
    list(
      list(
        address = "https://other.example.com",
        token = "other-tkn",
        auth_type = "sso"
      ),
      list(
        address = "https://ppm.example.com",
        token = "tkn1",
        auth_type = "sso"
      )
    )
  )
})

test_that("ppm_sso_write_token_to_file: updating existing entry", {
  path <- local_token_path()
  mkdirp(dirname(path))
  writeLines(
    c(
      "[[connections]]",
      "address = \"https://ppm.example.com\"",
      "token = \"old-tkn\"",
      "auth_type = \"sso\""
    ),
    path
  )

  ppm_sso_write_token_to_file("https://ppm.example.com", "new-tkn")

  conns <- read_connections(path)
  expect_equal(
    conns,
    list(list(
      address = "https://ppm.example.com",
      token = "new-tkn",
      auth_type = "sso"
    ))
  )
})

test_that("ppm_sso_write_token_to_file: updating preserves extra data", {
  path <- local_token_path()
  mkdirp(dirname(path))
  writeLines(
    c(
      "top_level = \"keep me\"",
      "",
      "[[connections]]",
      "address = \"https://other.example.com\"",
      "token = \"other-tkn\"",
      "auth_type = \"sso\"",
      "extra = \"keep this too\"",
      "",
      "[[connections]]",
      "address = \"https://ppm.example.com\"",
      "token = \"old-tkn\"",
      "auth_type = \"sso\"",
      "user = \"alice\"",
      "",
      "[meta]",
      "version = 1"
    ),
    path
  )

  ppm_sso_write_token_to_file("https://ppm.example.com", "new-tkn")

  tokens <- suppressWarnings(tstoml::ts_read_toml(path))
  conns <- tsitter::ts_tree_unserialize(
    tsitter::ts_tree_select(tokens, list("connections", TRUE))
  )
  expect_equal(
    conns,
    list(
      list(
        address = "https://other.example.com",
        token = "other-tkn",
        auth_type = "sso",
        extra = "keep this too"
      ),
      list(
        address = "https://ppm.example.com",
        token = "new-tkn",
        auth_type = "sso",
        user = "alice"
      )
    )
  )
  expect_equal(
    tsitter::ts_tree_unserialize(tsitter::ts_tree_select(tokens, "top_level"))[[
      1
    ]],
    "keep me"
  )
  expect_equal(
    tsitter::ts_tree_unserialize(
      tsitter::ts_tree_select(tokens, list("meta", "version"))
    )[[1]],
    1L
  )
})

test_that("ppm_sso_device_flow works against a fake PPM app", {
  srv <- webfakes::local_app_process(ppm_sso_app())
  withr::local_options("rlib.interactive" = FALSE)
  ppm_url <- sub("/$", "", srv$url())

  token <- suppressMessages(ppm_sso_device_flow(ppm_url))

  expect_type(token, "character")
  jwt <- jwt_split(token)
  expect_equal(jwt$payload$iss, "https://ppm-sso-local.invalid/")
  expect_equal(jwt$payload$sub, "ppm-sso-local-user")
  expect_equal(jwt$payload$aud, "ppm-sso-local")
  expect_true(jwt$payload$exp > unclass(Sys.time()))
})
