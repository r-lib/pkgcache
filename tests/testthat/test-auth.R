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
      allow_prompt = FALSE,
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
      allow_prompt = FALSE,
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
      allow_prompt = FALSE,
      use_cache = FALSE,
      set_cache = FALSE
    )
  )
})

test_that("caching", {
  on.exit(clear_auth_cache(), add = TRUE)

  clear_auth_cache()
  withr::local_envvar(c("https://ppm.internal/cran/latest:username" = "token"))
  expect_snapshot({
    repo_auth_headers(
      "https://username@ppm.internal/cran/__linux__/jammy/latest/src/contrib/PACKAGES.gz",
      allow_prompt = FALSE
    )
    repo_auth_headers(
      "https://username@ppm.internal/cran/__linux__/jammy/latest/src/contrib/PACKAGES.gz",
      allow_prompt = FALSE
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
      "https://username@ppm.internal/cran/latest/bin/linux/4.4-jammy/contrib/4.4/PACKAGES.gz",
      allow_prompt = FALSE
    )
    repo_auth_headers(
      "https://username@ppm.internal/cran/latest/bin/linux/4.4-jammy/contrib/4.4/PACKAGES.gz",
      allow_prompt = FALSE
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
  synchronise(download_file(url2, tmp))
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
  expect_snapshot(error = TRUE, {
    synchronise(download_file(url2, tmp3))
  })
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
