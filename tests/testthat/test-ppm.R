if (Sys.getenv("R_COVR") == "true") {
  return()
}

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
  fake(
    ppm_snapshots,
    "async_get_ppm_versions",
    function(...) async_constant(ver)
  )
  expect_snapshot(ppm_snapshots()[1:1000, ])
})

test_that("ppm_platforms", {
  on.exit(pkgenv$ppm_distros <- NULL, add = TRUE)
  plt <- structure(
    list(
      name = c("centos7", "centos8", "rhel9"),
      os = c("linux", "linux", "linux"),
      binary_url = c("centos7", "centos8", "rhel9"),
      distribution = c("centos", "centos", "rockylinux"),
      release = c("7", "8", "9"),
      binaries = c(TRUE, TRUE, TRUE)
    ),
    row.names = c(NA, 3L),
    class = "data.frame"
  )

  fake(
    ppm_platforms,
    "async_get_ppm_status",
    function(...) async_constant(list(distros = plt))
  )
  expect_snapshot(ppm_platforms())
})

test_that("async_get_ppm_status", {
  on.exit(pkgenv$ppm_distros <- NULL, add = TRUE)
  fake(
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
  expect_snapshot(
    error = TRUE,
    synchronise(async_get_ppm_status(forget = TRUE))
  )

  # so does an unknown distro or release, if we haven't updated yet
  pkgenv$ppm_distros <- NULL
  expect_snapshot(
    error = TRUE,
    synchronise(async_get_ppm_status(distribution = "123"))
  )
  pkgenv$ppm_distros <- NULL
  expect_snapshot(
    error = TRUE,
    synchronise(async_get_ppm_status(
      distribution = "ubuntu",
      release = "123"
    ))
  )
})

test_that("async_get_ppm_status 2", {
  on.exit(pkgenv$ppm_distros <- NULL, add = TRUE)
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
  on.exit(pkgenv$ppm_distros <- NULL, add = TRUE)
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
  on.exit(pkgenv$ppm_distros <- NULL, add = TRUE)
  withr::local_envvar(PKGCACHE_PPM_BINARIES = NA_character_)
  fake(
    ppm_has_binaries,
    "current_r_platform_data",
    structure(
      list(
        cpu = "aarch64",
        vendor = "pc",
        os = "linux-gnu",
        distribution = "ubuntu",
        release = "22.04",
        platform = "aarch64-pc-linux-gnu-ubuntu-22.04"
      ),
      row.names = c(NA, -1L),
      class = "data.frame"
    )
  )
  expect_false(ppm_has_binaries())

  fake(
    ppm_has_binaries,
    "current_r_platform_data",
    structure(
      list(
        cpu = "x86_64",
        vendor = "apple",
        os = "darwin20",
        platform = "x86_64-apple-darwin20"
      ),
      row.names = c(NA, -1L),
      class = "data.frame"
    )
  )
  expect_false(ppm_has_binaries())

  # Use cached values, no HTTP
  pkgenv$ppm_distros <- pkgenv$ppm_distros_cached
  pkgenv$ppm_r_versions <- pkgenv$ppm_r_versions_cached
  fake(ppm_has_binaries, "async_ppm_get_status", NULL)

  # Windows
  fake(
    ppm_has_binaries,
    "current_r_platform_data",
    structure(
      list(
        cpu = "x86_64",
        vendor = "w64",
        os = "mingw32",
        platform = "x86_64-w64-mingw32"
      ),
      row.names = c(NA, -1L),
      class = "data.frame"
    )
  )
  fake(ppm_has_binaries, "getRversion", "4.2.2")
  expect_true(ppm_has_binaries())

  # Not supported Linux
  fake(
    ppm_has_binaries,
    "current_r_platform_data",
    structure(
      list(
        cpu = "x86_64",
        vendor = "pc",
        os = "linux-gnu",
        distribution = "ubuntu",
        release = "14.04",
        platform = "x86_64-pc-linux-gnu-ubuntu-14.04"
      ),
      row.names = c(NA, -1L),
      class = "data.frame"
    )
  )
  fake(ppm_has_binaries, "getRversion", "4.2.2")
  expect_false(ppm_has_binaries())

  # Supported Linux
  fake(
    ppm_has_binaries,
    "current_r_platform_data",
    structure(
      list(
        cpu = "x86_64",
        vendor = "pc",
        os = "linux-gnu",
        distribution = "ubuntu",
        release = "22.04",
        platform = "x86_64-pc-linux-gnu-ubuntu-22.04"
      ),
      row.names = c(NA, -1L),
      class = "data.frame"
    )
  )
  fake(ppm_has_binaries, "getRversion", "4.4.3")
  expect_true(ppm_has_binaries())

  # Not supported R version
  fake(ppm_has_binaries, "getRversion", "1.0.0")
  expect_false(ppm_has_binaries())
})

test_that("ppm_r_versions", {
  on.exit(pkgenv$ppm_distros <- NULL, add = TRUE)
  rver <- c("3.5", "3.6", "4.2")
  fake(
    ppm_r_versions,
    "async_get_ppm_status",
    function(...) async_constant(list(r_versions = rver))
  )
  expect_snapshot(ppm_r_versions())
})

test_that("pkgenv$ppm_distros_cached is current", {
  skip_on_cran()
  cached <- pkgenv$ppm_distros_cached
  current <- canonicalize_ppm_platforms(
    synchronise(async_get_ppm_status(forget = TRUE))$distros
  )
  expect_equal(cached, current)
  expect_snapshot(current)
})

test_that("ppm binary support is correctly detected", {
  has_bins <- function(...) {
    etc <- file.path(test_path("fixtures", "linux"), ...)
    raw <- data.frame(raw = "foo")
    platform <- current_r_platform_data_linux(raw = raw, etc = etc)
    withr::local_options(
      pkg.current_platform = paste0(
        "x86_64-unknown-linux-gnu-",
        platform$distribution,
        "-",
        platform$release
      )
    )
    fake(ppm_has_binaries, "get_minor_r_version", "4.5")
    fake(ppm_has_binaries, "async_get_ppm_status", NULL)
    pkgenv$ppm_distros <- pkgenv$ppm_distros_cached
    pkgenv$ppm_r_versions <- pkgenv$ppm_r_versions_cached
    on.exit(
      {
        pkgenv$ppm_distros <- pkgenv$ppm_r_versions <- NULL
      },
      add = TRUE
    )
    ppm_has_binaries()
  }

  expect_true(has_bins("ubuntu", "16.04"))
  expect_true(has_bins("ubuntu", "18.04"))
  expect_true(has_bins("ubuntu", "20.04"))
  expect_true(has_bins("ubuntu", "22.04"))
  expect_false(has_bins("ubuntu", "22.10"))
  expect_true(has_bins("ubuntu", "24.04"))

  expect_false(has_bins("debian", "10"))
  expect_true(has_bins("debian", "11"))
  expect_true(has_bins("debian", "12"))

  expect_true(has_bins("opensuse", "15.4"))
  expect_true(has_bins("opensuse", "15.5"))
  expect_true(has_bins("opensuse", "15.6"))

  expect_false(has_bins("centos", "6"))
  expect_true(has_bins("centos", "7"))
  expect_true(has_bins("centos", "8"))

  expect_false(has_bins("rhel", "6"))
  expect_true(has_bins("rhel", "7"))
  expect_true(has_bins("rhel", "8"))
  expect_true(has_bins("rhel", "9"))

  expect_false(has_bins("fedora", "39"))

  expect_true(has_bins("sles", "15.4"))
  expect_true(has_bins("sles", "15.5"))
  expect_true(has_bins("sles", "15.6"))

  expect_true(has_bins("almalinux", "8"))
  expect_true(has_bins("almalinux", "9"))

  expect_true(has_bins("rocky", "8"))
  expect_true(has_bins("rocky", "9"))

  expect_false(has_bins("alpine", "3.18"))
})
