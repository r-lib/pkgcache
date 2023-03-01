
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
