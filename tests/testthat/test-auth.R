test_that("looking up auth headers for repositories works as expected", {
  skip_if_not_installed("keyring")

  # No package directory in the URL.
  expect_null(repo_auth_headers(
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

  # Verify that the environment variable keyring backend is picked up correctly.
  withr::with_envvar(
    c("https://ppm.internal/cran/latest:username" = "token"),
    expect_equal(
      repo_auth_headers(
        "https://username@ppm.internal/cran/__linux__/jammy/latest/src/contrib/PACKAGES.gz",
        allow_prompt = FALSE,
        use_cache = FALSE,
        set_cache = FALSE
      ),
      list(
        headers = c("Authorization" = "Basic dXNlcm5hbWU6dG9rZW4="),
        auth_domain = "https://ppm.internal/cran/latest"
      )
    )
  )

  # Verify that we fall back to checking for a hostname credential when none
  # is available for a specific repo.
  withr::with_envvar(
    c("https://ppm.internal:username" = "token"),
    expect_equal(
      repo_auth_headers(
        "https://username@ppm.internal/cran/latest/bin/linux/4.4-jammy/contrib/4.4/PACKAGES.gz",
        allow_prompt = FALSE,
        use_cache = FALSE,
        set_cache = FALSE
      ),
      list(
        headers = c("Authorization" = "Basic dXNlcm5hbWU6dG9rZW4="),
        auth_domain = "https://ppm.internal"
      )
    )
  )
})

test_that("caching", {
  on.exit(clear_auth_cache(), add = TRUE)

  clear_auth_cache()
  withr::with_envvar(
    c("https://ppm.internal/cran/latest:username" = "token"),
    expect_equal(
      repo_auth_headers(
        "https://username@ppm.internal/cran/__linux__/jammy/latest/src/contrib/PACKAGES.gz",
        allow_prompt = FALSE,
        use_cache = TRUE,
        set_cache = TRUE
      ),
      list(
        headers = c("Authorization" = "Basic dXNlcm5hbWU6dG9rZW4="),
        auth_domain = "https://ppm.internal/cran/latest"
      )
    )
  )
  expect_equal(
    pkgenv$credentials[["https://ppm.internal/cran/latest"]],
    list(
      headers = c("Authorization" = "Basic dXNlcm5hbWU6dG9rZW4="),
      auth_domain = "https://ppm.internal/cran/latest"
    )
  )

  clear_auth_cache()
  withr::with_envvar(
    c("https://ppm.internal:username" = "token"),
    expect_equal(
      repo_auth_headers(
        "https://username@ppm.internal/cran/latest/bin/linux/4.4-jammy/contrib/4.4/PACKAGES.gz",
        allow_prompt = FALSE,
        use_cache = TRUE,
        set_cache = TRUE
      ),
      list(
        headers = c("Authorization" = "Basic dXNlcm5hbWU6dG9rZW4="),
        auth_domain = "https://ppm.internal"
      )
    )
  )
  expect_equal(
    pkgenv$credentials[["https://ppm.internal"]],
    list(
      headers = c("Authorization" = "Basic dXNlcm5hbWU6dG9rZW4="),
      auth_domain = "https://ppm.internal"
    )
  )
})

test_that("basic auth credentials can be extracted from various URL formats", {
  expect_equal(
    extract_basic_auth_credentials("https://user.name:pass-word123@example.com"),
    list(
      hosturl = "https://example.com",
      hostuserurl = "https://user.name@example.com",
      repourl = "https://example.com",
      repouserurl = "https://user.name@example.com",
      username = "user.name",
      password = "pass-word123"
    )
  )
  expect_equal(
    extract_basic_auth_credentials("http://user@example.com"),
    list(
      hosturl = "http://example.com",
      hostuserurl = "http://user@example.com",
      repourl = "http://example.com",
      repouserurl = "http://user@example.com",
      username = "user",
      password = ""
    )
  )
  expect_equal(
    extract_basic_auth_credentials("https://example.com"),
    list(
      hosturl = "https://example.com",
      hostuserurl = "https://example.com",
      repourl = "https://example.com",
      repouserurl = "https://example.com",
      username = "",
      password = ""
    )
  )
  expect_error(
    extract_basic_auth_credentials("notaurl"),
    "Unrecognized URL format"
  )
})
