
test_that("ppm_repo_url", {
  withr::local_envvar(
    PKGCACHE_PPM_URL = NA_character_,
    PKGCACHE_RSPM_URL = my <- "https://my.rspm/repo"
  )
  expect_equal(ppm_repo_url(), my)
})

test_that("ppm_repo_url 2", {
  withr::local_envvar(
    PKGCACHE_PPM_URL = NA_character_,
    PKGCACHE_RSPM_URL = NA_character_
  )
  withr::local_options(
    repos = c(
      RSPM = "https://packagemanager.rstudio.com/all/__linux__/jammy/latest",
      CRAN = "https://cran.rstudio.com"
    )
  )

  expect_equal(ppm_repo_url(), "https://packagemanager.rstudio.com/all")
})

test_that("ppm_snapshots", {
  ver <- c(
    `2023-02-20T00:00:00Z` = "16856763",
    `2023-02-21T00:00:00Z` = "16882269",
    `2023-02-22T00:00:00Z` = "16903694",
    `2023-02-24T00:00:00Z` = "16958782",
    `2023-02-27T00:00:00Z` = "17028146",
    `2023-02-28T00:00:00Z` = "17054670"
  )
  mockery::stub(
    ppm_snapshots,
    "async_get_ppm_versions",
    function(...) async_constant(ver)
  )
  expect_snapshot(ppm_snapshots())
})

test_that("ppm_platforms", {
  plt <- structure(list(
    name = c("centos7", "centos8", "rhel9"),
    os = c("linux", "linux", "linux"),
    binary_url = c("centos7", "centos8", "rhel9"),
    distribution = c("centos", "centos", "rockylinux"),
    release = c("7", "8", "9"),
    binaries = c(TRUE, TRUE, TRUE)
  ), row.names = c(NA, 3L), class = "data.frame")

  mockery::stub(
    ppm_platforms,
    "async_get_ppm_status",
    function(...) async_constant(list(distros = plt))
  )
  expect_snapshot(ppm_platforms())
})

test_that("async_get_ppm_versions", {
  mockery::stub(
    async_get_ppm_versions,
    "download_file",
    function(...) stop("nope")
  )
  # uses cache by default
  expect_silent(synchronise(async_get_ppm_versions()))

  # forget = TRUE forces an update
  expect_error(synchronise(async_get_ppm_versions(forget = TRUE)))
})

test_that("async_get_ppm_versions 2", {
  withr::local_envvar(
    PKGCACHE_PPM_TRANSACTIONS_URL = repo$url("/ppmversions")
  )

  ret <- synchronise(async_get_ppm_versions(date = "2100-10-10"))
  expect_snapshot(ret)
})

test_that("async_get_ppm_versions 3", {
  withr::local_envvar(
    PKGCACHE_PPM_TRANSACTIONS_URL = repo$url("/bogus")
  )

  expect_warning(
    synchronise(async_get_ppm_versions(date = "2100-10-10")),
    "Failed to download PPM versions"
  )
})

test_that("async_get_ppm_status", {
  mockery::stub(
    async_get_ppm_status,
    "download_file",
    function(...) stop("nope")
  )

  # uses cache by default
  pkgenv$ppm_distros <- NULL
  expect_silent(synchronise(async_get_ppm_status()))
  expect_equal(pkgenv$ppm_distros, pkgenv$ppm_distros_cached)

  # no update needed
  pkgenv$ppm_distros <- NULL
  expect_silent(synchronise(async_get_ppm_status(distribution = "ubuntu")))
  expect_equal(pkgenv$ppm_distros, pkgenv$ppm_distros_cached)

  # no update needed
  pkgenv$ppm_distros <- NULL
  expect_silent(synchronise(async_get_ppm_status(
    distribution = "ubuntu",
    release = "22.04"
  )))
  expect_equal(pkgenv$ppm_distros, pkgenv$ppm_distros_cached)

  # forget = TRUE forces an update
  expect_error(synchronise(async_get_ppm_status(forget = TRUE)))

  # so does an unknown distro or release, if we haven't updated yet
  pkgenv$ppm_distros <- NULL
  expect_error(synchronise(async_get_ppm_status(distribution = "123")))
  pkgenv$ppm_distros <- NULL
  expect_error(synchronise(async_get_ppm_status(
    distribution = "ubuntu",
    release = "123"
  )))
})

test_that("async_get_ppm_status 2", {
  withr::local_envvar(
    PKGCACHE_PPM_STATUS_URL = repo$url("/ppmstatus")
  )

  pkgenv$ppm_distros <- NULL
  ret <- synchronise(async_get_ppm_status(distribution = "notreal"))$distros
  expect_snapshot(ret)

  withr::local_envvar(
    PKGCACHE_PPM_STATUS_URL = repo$url("/bogus")
  )

  pkgenv$ppm_distros <- NULL
  expect_warning(
    synchronise(async_get_ppm_status(distribution = "notreal")),
    "Failed to download PPM status"
  )
})

test_that("async_get_ppm_status 3", {
  withr::local_envvar(
    PKGCACHE_PPM_STATUS_URL = repo$url("/ppmstatus")
  )

  pkgenv$ppm_r_versions <- NULL
  pkgenv$ppm_distros <- NULL
  ret <- synchronise(async_get_ppm_status(r_version = "1.0"))$r_versions
  expect_snapshot(ret)

  withr::local_envvar(
    PKGCACHE_PPM_STATUS_URL = repo$url("/bogus")
  )

  pkgenv$ppm_distros <- NULL
  expect_warning(
    synchronise(async_get_ppm_status(distribution = "notreal")),
    "Failed to download PPM status"
  )
})

test_that("ppm_has_binaries", {
  withr::local_envvar(PKGCACHE_PPM_BINARIES = "no")
  expect_false(ppm_has_binaries())
})

test_that("ppm_has_binaries 2", {
  withr::local_envvar(PKGCACHE_PPM_BINARIES = NA_character_)
  mockery::stub(
    ppm_has_binaries,
    "current_r_platform_data",
    structure(list(
      cpu = "aarch64", vendor = "pc", os = "linux-gnu",
      distribution = "ubuntu", release = "22.04",
      platform = "aarch64-pc-linux-gnu-ubuntu-22.04"
    ), row.names = c(NA, -1L), class = "data.frame")
  )
  expect_false(ppm_has_binaries())

  mockery::stub(
    ppm_has_binaries,
    "current_r_platform_data",
    structure(list(
      cpu = "x86_64", vendor = "apple", os = "darwin20",
      platform = "x86_64-apple-darwin20"
    ), row.names = c(NA, -1L), class = "data.frame")
  )
  expect_false(ppm_has_binaries())

  # Use cached values, no HTTP
  pkgenv$ppm_distros <- pkgenv$ppm_distros_cached
  pkgenv$ppm_r_versions <- pkgenv$ppm_r_versions_cached
  mockery::stub(ppm_has_binaries, "async_ppm_get_status", NULL)

  # Windows
  mockery::stub(
    ppm_has_binaries,
    "current_r_platform_data",
    structure(list(
      cpu = "x86_64", vendor = "w64", os = "mingw32",
      platform = "x86_64-w64-mingw32"
    ), row.names = c(NA, -1L), class = "data.frame")
  )
  mockery::stub(ppm_has_binaries, "getRversion", "4.2.2")
  expect_true(ppm_has_binaries())

  # Not supported Linux
  mockery::stub(
    ppm_has_binaries,
    "current_r_platform_data",
    structure(list(
      cpu = "x86_64", vendor = "pc", os = "linux-gnu",
      distribution = "ubuntu", release = "14.04",
      platform = "x86_64-pc-linux-gnu-ubuntu-14.04"
    ), row.names = c(NA, -1L), class = "data.frame")
  )
  mockery::stub(ppm_has_binaries, "getRversion", "4.2.2")
  expect_false(ppm_has_binaries())

  # Supported Linux
  mockery::stub(
    ppm_has_binaries,
    "current_r_platform_data",
    structure(list(
      cpu = "x86_64", vendor = "pc", os = "linux-gnu",
      distribution = "ubuntu", release = "22.04",
      platform = "x86_64-pc-linux-gnu-ubuntu-22.04"
    ), row.names = c(NA, -1L), class = "data.frame")
  )
  mockery::stub(ppm_has_binaries, "getRversion", "4.2.2")
  expect_true(ppm_has_binaries())

  # Not supported R version
  mockery::stub(ppm_has_binaries, "getRversion", "1.0.0")
  expect_false(ppm_has_binaries())
})

test_that("ppm_r_versions", {
  rver <- c("3.5", "3.6", "4.2")
  mockery::stub(
    ppm_r_versions,
    "async_get_ppm_status",
    function(...) async_constant(list(r_versions = rver))
  )
  expect_snapshot(ppm_r_versions())
})
