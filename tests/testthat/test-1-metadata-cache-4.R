
test_that("ppm_fallback_r_version", {
  withr::local_envvar(PKGCACHE_PPM_R_VERSION_FALLBACK = NA_character_)
  withr::local_options(HTTPUserAgent = "foobar")
  expect_equal(ppm_fallback_r_version("foo"), "foo")

  withr::local_envvar(PKGCACHE_PPM_R_VERSION_FALLBACK = "true")
  mockery::stub(ppm_fallback_r_version, "async_get_ppm_status", NULL)
  pkgenv$ppm_r_versions <- pkgenv$ppm_r_versions_cached

  expect_equal(ppm_fallback_r_version("4.2"), "4.2")
  expect_equal(ppm_fallback_r_version(c("4.2", "4.2")), c("4.2", "4.2"))
  expect_equal(ppm_fallback_r_version(c("4.2", "4.3")), c("4.2", "4.2"))
  expect_equal(ppm_fallback_r_version(c("4.3", "4.3")), c("4.2", "4.2"))
  expect_equal(ppm_fallback_r_version(c("1.0", "4.3")), c("1.0", "4.2"))

  withr::local_envvar(PKGCACHE_PPM_R_VERSION_FALLBACK = NA_character_)
  withr::local_options(HTTPUserAgent = "R/4.2.2 blah")
  expect_equal(ppm_fallback_r_version("4.2"), "4.2")
  expect_equal(ppm_fallback_r_version(c("4.2", "4.2")), c("4.2", "4.2"))
  expect_equal(ppm_fallback_r_version(c("4.2", "4.3")), c("4.2", "4.2"))
  expect_equal(ppm_fallback_r_version(c("4.3", "4.3")), c("4.2", "4.2"))
  expect_equal(ppm_fallback_r_version(c("1.0", "4.3")), c("1.0", "4.2"))
})

test_that("ppm_calculate_headers", {
  pkgs <- data_frame(bin_url = NA_character_)
  expect_equal(ppm_calculate_headers(pkgs)$headers, list(NULL))

  # use a sinplified implementation for testing
  mockery::stub(
    ppm_calculate_headers,
    "ppm_fallback_r_version",
    function(x) {
      if (is_true_env_var("PKGCACHE_PPM_R_VERSION_FALLBACK")) {
        x[package_version(x) > "4.2"] <- "4.2"
      }
      x
    }
  )

  # get rid of current version and platform variance
  mockery::stub(ppm_calculate_headers, "getRversion", package_version("4.2.2"))
  mockery::stub(
    ppm_calculate_headers,
    "R.Version", 
    list(platform = "aarch64-apple-darwin20", arch = "aarch64", os = "darwin20")
  )
    
  pkgs <- data_frame(
    bin_url = "https://packagemanager.rstudio.com/all/latest/bin/linux/4.2-jammy/contrib/4.2/PACKAGES.gz"
  )
  expect_snapshot(as.list(ppm_calculate_headers(pkgs)))

  withr::local_envvar(PKGCACHE_PPM_R_VERSION_FALLBACK = "true")
  expect_snapshot(as.list(ppm_calculate_headers(pkgs)))

  pkgs <- data_frame(
    bin_url = "https://packagemanager.rstudio.com/all/latest/bin/linux/4.3-jammy/contrib/4.3/PACKAGES.gz"
  )
  expect_snapshot(as.list(ppm_calculate_headers(pkgs)))

  withr::local_envvar(PKGCACHE_PPM_R_VERSION_FALLBACK = NA_character_)
  withr::local_options(HTTPUserAgent = "foobar")
  expect_snapshot(as.list(ppm_calculate_headers(pkgs)))
})
