
test_that("async_get_rspm_versions", {
  mockery::stub(
    async_get_rspm_versions,
    "download_file",
    function(...) stop("nope")
  )
  # uses cache by default
  expect_silent(synchronise(async_get_rspm_versions()))

  # forget = TRUE forces an update
  expect_error(synchronise(async_get_rspm_versions(forget = TRUE)))
})

test_that("async_get_rspm_versions 2", {
  withr::local_envvar(
    PKGCACHE_RSPM_TRANSACTIONS_URL = repo$url("/rspmversions")
  )

  ret <- synchronise(async_get_rspm_versions(date = "2100-10-10"))
  expect_snapshot(ret)
})

test_that("async_get_rspm_versions 3", {
  withr::local_envvar(
    PKGCACHE_RSPM_TRANSACTIONS_URL = repo$url("/bogus")
  )

  expect_warning(
    synchronise(async_get_rspm_versions(date = "2100-10-10")),
    "Failed to download RSPM versions"
  )
})

test_that("async_get_rspm_distros", {
  mockery::stub(
    async_get_rspm_distros,
    "download_file",
    function(...) stop("nope")
  )

  # uses cache by default
  pkgenv$rspm_distros <- NULL
  expect_silent(synchronise(async_get_rspm_distros()))
  expect_equal(pkgenv$rspm_distros, pkgenv$rspm_distros_cached)

  # no update needed
  pkgenv$rspm_distros <- NULL
  expect_silent(synchronise(async_get_rspm_distros(distribution = "ubuntu")))
  expect_equal(pkgenv$rspm_distros, pkgenv$rspm_distros_cached)

  # no update needed
  pkgenv$rspm_distros <- NULL
  expect_silent(synchronise(async_get_rspm_distros(
    distribution = "ubuntu",
    release = "22.04"
  )))
  expect_equal(pkgenv$rspm_distros, pkgenv$rspm_distros_cached)

  # forget = TRUE forces an update
  expect_error(synchronise(async_get_rspm_distros(forget = TRUE)))

  # so does an unknown distro or release, if we haven't updated yet
  pkgenv$rspm_distros <- NULL
  expect_error(synchronise(async_get_rspm_distros(distribution = "123")))
  pkgenv$rspm_distros <- NULL
  expect_error(synchronise(async_get_rspm_distros(
    distribution = "ubuntu",
    release = "123"
  )))
})

test_that("async_get_rspm_distros 2", {
  withr::local_envvar(
    PKGCACHE_RSPM_STATUS_URL = repo$url("/rspmstatus")
  )

  pkgenv$rspm_distros <- NULL
  ret <- synchronise(async_get_rspm_distros(distribution = "notreal"))
  expect_snapshot(ret)

  withr::local_envvar(
    PKGCACHE_RSPM_STATUS_URL = repo$url("/bogus")
  )

  pkgenv$rspm_distros <- NULL
  expect_warning(
    synchronise(async_get_rspm_distros(distribution = "notreal")),
    "Failed to download RSPM status"
  )
})
