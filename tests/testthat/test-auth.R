test_that("looking up auth headers for repositories works as expected", {
  withr::local_options(keyring_backend = "env")
  # No package directory in the URL.
  expect_snapshot(repo_auth_headers(
    "https://username@ppm.internal/healthz",
    use_cache = FALSE,
    set_cache = FALSE
  ))

  # The URL already contains a password.
  expect_null(
    repo_auth_headers(
      "https://username:password@ppm.internal/cran/latest/src/contrib/PACKAGES.gz",
      use_cache = FALSE,
      set_cache = FALSE
    )
  )

  # No username in the repo URL.
  expect_null(
    repo_auth_headers(
      "https://ppm.internal/cran/latest/src/contrib/PACKAGES.gz",
      use_cache = FALSE,
      set_cache = FALSE
    )
  )

  expect_null(
    repo_auth_headers(
      "https://@ppm.internal/cran/latest/src/contrib/PACKAGES.gz",
      use_cache = FALSE,
      set_cache = FALSE
    )
  )

  # Verify that the environment variable keyring backend is picked up correctly.
  withr::local_envvar(c("https://ppm.internal/cran/latest:username" = "token"))
  expect_snapshot(
    repo_auth_headers(
      "https://username@ppm.internal/cran/__linux__/jammy/latest/src/contrib/PACKAGES.gz",
      use_cache = FALSE,
      set_cache = FALSE
    )
  )

  # Verify that we fall back to checking for a hostname credential when none
  # is available for a specific repo.
  withr::local_envvar(c("https://ppm.internal:username" = "token"))
  expect_snapshot(
    repo_auth_headers(
      "https://username@ppm.internal/cran/latest/bin/linux/4.4-jammy/contrib/4.4/PACKAGES.gz",
      use_cache = FALSE,
      set_cache = FALSE
    )
  )
})

test_that("without keyring", {
  fake(repo_auth_headers, "requireNamespace", FALSE)
  withr::local_envvar(c("https://ppm.internal/cran/latest:username" = "token"))
  expect_snapshot(
    repo_auth_headers(
      "https://username@ppm.internal/cran/__linux__/jammy/latest/src/contrib/PACKAGES.gz",
      use_cache = FALSE,
      set_cache = FALSE
    )
  )
})

test_that("caching", {
  on.exit(clear_auth_cache(), add = TRUE)
  withr::local_options(keyring_backend = "env")

  clear_auth_cache()
  withr::local_envvar(c("https://ppm.internal/cran/latest:username" = "token"))
  expect_snapshot({
    suppressMessages(repo_auth_headers(
      "https://username@ppm.internal/cran/__linux__/jammy/latest/src/contrib/PACKAGES.gz"
    ))
    repo_auth_headers(
      "https://username@ppm.internal/cran/__linux__/jammy/latest/src/contrib/PACKAGES.gz"
    )
    pkgenv$credentials[["https://ppm.internal/cran/latest"]]
  })

  clear_auth_cache()
  withr::local_envvar(c(
    "https://ppm.internal:username" = "token",
    "https://ppm.internal/cran/latest:username" = NA_character_
  ))
  expect_snapshot({
    repo_auth_headers(
      "https://username@ppm.internal/cran/latest/bin/linux/4.4-jammy/contrib/4.4/PACKAGES.gz"
    )
    repo_auth_headers(
      "https://username@ppm.internal/cran/latest/bin/linux/4.4-jammy/contrib/4.4/PACKAGES.gz"
    )
    pkgenv$credentials[["https://ppm.internal"]]
  })
})

test_that("http requests with auth", {
  withr::local_options(keyring_backend = "env")

  # with password
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  url <- http$url("/basic-auth/username/token")
  url2 <- set_user_in_url(url)
  authurls <- parse_url_basic_auth(url2)
  keyring::key_set_with_value(authurls$hosturl, "username", "token")
  suppressMessages(synchronise(download_file(url2, tmp)))
  expect_snapshot(readLines(tmp, warn = FALSE))

  synchronise(download_if_newer(url2, tmp))
  expect_snapshot(readLines(tmp, warn = FALSE))

  tmp2 <- tempfile()
  on.exit(unlink(tmp2), add = TRUE)
  synchronise(download_if_newer(url2, tmp2))
  expect_snapshot(readLines(tmp2, warn = FALSE))

  # without password
  clear_auth_cache()
  keyring::key_delete(authurls$hosturl, "username")
  tmp3 <- tempfile()
  on.exit(unlink(tmp3), add = TRUE)
  expect_snapshot(
    error = TRUE,
    {
      synchronise(download_file(url2, tmp3))
    },
    transform = fix_port_number
  )
  expect_snapshot(error = TRUE, {
    synchronise(download_if_newer(url2, tmp3))
  })
})

test_that("repo with basic auth", {
  withr::local_options(
    keyring_backend = "env",
    cli.dynamic = FALSE,
    cli.ansi = FALSE
  )

  fake_cran_auth <- webfakes::local_app_process(
    cran_app(
      cran_app_pkgs,
      basic_auth = c(username = "username", password = "token")
    ),
    opts = webfakes::server_opts(num_threads = 5)
  )
  withr::local_options(
    repos = c(CRAN = set_user_in_url(fake_cran_auth$url()))
  )

  # no password
  authurls <- parse_url_basic_auth(getOption("repos")[["CRAN"]])
  clear_auth_cache()
  keyring::key_delete(authurls$hosturl, "username")
  cmc <- cranlike_metadata_cache$new(platforms = "source", bioc = FALSE)
  expect_snapshot(
    cmc$update(),
    transform = function(x) {
      x <- sub(
        "Updated metadata database: [0-9.]+ .*B in [0-9]+ files?.",
        "Updated metadata database: <size> <unit> in <num> file<s>.",
        x
      )
      x <- x[!grepl("^\\s*$", x)]
      x <- fix_port_number(x)
      x
    }
  )

  # password
  clear_auth_cache()
  keyring::key_set_with_value(authurls$hosturl, "username", "token")
  expect_snapshot(
    cmc$update(),
    transform = function(x) {
      x <- sub(" [a-f0-9]{10}~ ", " <md5sum> ", x)
      x <- sub(
        "Updated metadata database: [0-9.]+ .*B in [0-9]+ files?.",
        "Updated metadata database: <size> <unit> in <num> file<s>.",
        x
      )
      x <- x[!grepl("^\\s*$", x)]
      x <- fix_port_number(x)
      x
    }
  )
})

test_that("basic auth credentials can be extracted from various URL formats", {
  expect_snapshot({
    parse_url_basic_auth("https://user.name:pass-word123@example.com")
    parse_url_basic_auth("http://user@example.com")
    parse_url_basic_auth("https://example.com")
  })
  expect_null(parse_url_basic_auth("notaurl"))
})

test_that("repo_auth_netrc", {
  tmp <- tempfile()
  withr::local_envvar(PKG_NETRC_PATH = tmp)
  on.exit(unlink(tmp), add = TRUE)
  # for predictable transform in snapshots
  withr::local_options(cli.width = 500)

  netrc <- "machine myhost login myuser password mysecret"
  writeLines(netrc, tmp)
  expect_snapshot({
    repo_auth_netrc("myhost", "myuser")
    repo_auth_netrc("myhost", "bad")
    repo_auth_netrc("bad", "myuser")
  })

  netrc <- c("machine myhost", "login myuser", "password mysecret")
  writeLines(netrc, tmp)
  expect_snapshot(repo_auth_netrc("myhost", "myuser"))

  netrc <- c("machine myhost", "login myuser", "", "", "password mysecret")
  writeLines(netrc, tmp)
  expect_snapshot(repo_auth_netrc("myhost", "myuser"))

  netrc <- c(
    "machine another",
    "login",
    "another",
    "machine myhost",
    "login myuser",
    "",
    "",
    "password mysecret"
  )
  writeLines(netrc, tmp)
  expect_snapshot(repo_auth_netrc("myhost", "myuser"))

  netrc <- c(
    "machine another",
    "login",
    "another",
    "macdef",
    "foo",
    "bar",
    "foobar",
    "",
    "machine myhost",
    "login myuser",
    "",
    "",
    "password mysecret"
  )
  writeLines(netrc, tmp)
  expect_snapshot(repo_auth_netrc("myhost", "myuser"))

  netrc <- "default login myuser password mysecret"
  writeLines(netrc, tmp)
  expect_snapshot(repo_auth_netrc("myhost", "myuser"))

  netrc <- c("machine")
  writeLines(netrc, tmp)
  expect_snapshot(
    repo_auth_netrc("myhost", "myuser"),
    transform = fix_temp_path
  )

  netrc <- c("machine myhost login")
  writeLines(netrc, tmp)
  expect_snapshot(
    repo_auth_netrc("myhost", "myuser"),
    transform = fix_temp_path
  )

  netrc <- c("machine myhost login myuser password")
  writeLines(netrc, tmp)
  expect_snapshot(
    repo_auth_netrc("myhost", "myuser"),
    transform = fix_temp_path
  )

  netrc <- c("login myuser password mysecret")
  writeLines(netrc, tmp)
  expect_snapshot(
    repo_auth_netrc("myhost", "myuser"),
    transform = fix_temp_path
  )

  netrc <- c("machine myhost password mysecret")
  writeLines(netrc, tmp)
  expect_snapshot(
    repo_auth_netrc("myhost", "myuser"),
    transform = fix_temp_path
  )

  netrc <- c("machine myhost badtoken")
  writeLines(netrc, tmp)
  expect_snapshot(
    repo_auth_netrc("myhost", "myuser"),
    transform = fix_temp_path
  )
})

test_that("repo_auth_headers w/ netrc", {
  netrc <- tempfile()
  withr::local_envvar(PKG_NETRC_PATH = netrc)
  on.exit(unlink(netrc), add = TRUE)

  writeLines(
    c(
      "machine foo.bar.com",
      "login username",
      "password token"
    ),
    netrc
  )

  expect_snapshot({
    repo_auth_headers("http://username@foo.bar.com/path")
  })
})

test_that("http requests with auth from netrc", {
  netrc <- tempfile()
  withr::local_envvar(PKG_NETRC_PATH = netrc)
  on.exit(unlink(netrc), add = TRUE)

  # with password
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  url <- http$url("/basic-auth/username/token")
  url2 <- set_user_in_url(url)
  authurls <- parse_url_basic_auth(url2)
  writeLines(
    c(
      paste0("machine ", sub(":[0-9]+$", "", authurls$host)),
      "login username",
      "password token"
    ),
    netrc
  )

  suppressMessages(synchronise(download_file(url2, tmp)))
  expect_snapshot(readLines(tmp, warn = FALSE))

  suppressMessages(synchronise(download_if_newer(url2, tmp)))
  expect_snapshot(readLines(tmp, warn = FALSE))
})
