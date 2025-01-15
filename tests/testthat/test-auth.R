test_that("looking up auth headers for repositories works as expected", {
  skip_if_not_installed("keyring")

  # No package directory in the URL.
  expect_null(repo_auth_headers("https://username@ppm.internal/healthz"))

  # The URL already contains a password.
  expect_null(
    repo_auth_headers(
      "https://username:password@ppm.internal/cran/latest/src/contrib/PACKAGES.gz"
    )
  )

  # No username in the repo URL.
  expect_null(
    repo_auth_headers(
      "https://ppm.internal/cran/latest/src/contrib/PACKAGES.gz"
    )
  )

  # Verify that the environment variable keyring backend is picked up correctly.
  withr::with_envvar(
    c("https://ppm.internal/cran/latest:username" = "token"),
    expect_equal(
      repo_auth_headers(
        "https://username@ppm.internal/cran/__linux__/jammy/latest/src/contrib/PACKAGES.gz",
        allow_prompt = FALSE
      ),
      c("Authorization" = "Basic dXNlcm5hbWU6dG9rZW4=")
    )
  )

  # Verify that we fall back to checking for a hostname credential when none
  # is available for a specific repo.
  withr::with_envvar(
    c("https://ppm.internal:username" = "token"),
    expect_equal(
      repo_auth_headers(
        "https://username@ppm.internal/cran/latest/bin/linux/4.4-jammy/contrib/4.4/PACKAGES.gz",
        allow_prompt = FALSE
      ),
      c("Authorization" = "Basic dXNlcm5hbWU6dG9rZW4=")
    )
  )
})

test_that("basic auth credentials can be extracted from various URL formats", {
  expect_equal(
    extract_basic_auth_credentials("https://user.name:pass-word123@example.com"),
    list(username = "user.name", password = "pass-word123")
  )
  expect_equal(
    extract_basic_auth_credentials("http://user@example.com"),
    list(username = "user", password = NULL)
  )
  expect_equal(
    extract_basic_auth_credentials("https://example.com"),
    list(username = NULL, password = NULL)
  )
  expect_error(
    extract_basic_auth_credentials("notaurl"),
    "Unrecognized URL format"
  )
})

test_that("we can extract hostnames and repository URLs from package URLs", {
  expect_equal(
    extract_repo_url(
      "https://username@ppm.internal/cran/__linux__/jammy/latest/src/contrib/PACKAGES.gz"
    ),
    "https://ppm.internal/cran/latest"
  )
  expect_equal(
    extract_repo_url(
      "https://username@ppm.internal/cran/latest/bin/linux/4.4-jammy/contrib/pkg.tar.gz"
    ),
    "https://ppm.internal/cran/latest"
  )
  expect_equal(
    extract_hostname(
      "https://username@ppm.internal/cran/latest/__linux__/jammy/src/contrib/PACKAGES.gz"
    ),
    "https://ppm.internal"
  )
})
