
pkgs <- dcf("
  Package: pkg1
  Version: 1.0.0

  Package: pkg1
  Version: 0.9.0

  Package: pkg1
  Version: 0.8.0

  Package: pkg2
  Version: 1.0.0
  Depends: pkg1

  Package: pkg3
  Version: 1.0.0
  Depends: pkg2

  Package: pkg3
  Version: 0.9.9
")
cran <- webfakes::local_app_process(
  cran_app(pkgs),
  opts = webfakes::server_opts(num_threads = 3)
)

test_that("API", {
  withr::local_options(
    repos = c(CRAN = cran$url()),
    pkg.cran_metadata_url = cran$url(),
    width = 1000
  )

  trfm <- function(x) fix_port_number(fix_mtime(x))

  expect_snapshot(
    cran_archive_list(),
    transform = trfm
  )

  expect_snapshot({
    cran_archive_update()
    cran_archive_list()
  }, transform = trfm)

  expect_snapshot({
    cran_archive_list(packages = "pkg1")
  }, transform = trfm)

  expect_error(cran_archive_summary(), NA)
  expect_error(suppressMessages(cran_archive_cleanup(force = TRUE)), NA)
  expect_equal(cran_archive_summary()$size, 0L)
})

test_that("cran_archive_cache", {
  withr::local_options(
    repos = c(CRAN = cran$url()),
    pkg.cran_metadata_url = cran$url(),
    width = 1000
  )

  trfm <- function(x) fix_port_number(fix_mtime(x))

  cac <- cran_archive_cache$new()
  expect_snapshot(
    cac$check_update(),
    transform = trfm
  )

  expect_silent(cac$summary())
  msg <- ""
  withCallingHandlers(
    cac$cleanup(force = TRUE),
    message = function(m) {
      msg <<- paste0(msg, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  expect_match(msg, "Cleaning up archive cache in", fixed = TRUE)
})

test_that("check_update", {
  withr::local_options(
    repos = c(CRAN = cran$url()),
    pkg.cran_metadata_url = cran$url(),
    width = 1000
  )

  trfm <- function(x) fix_port_number(fix_mtime(x))

  cac <- cran_archive_cache$new()

  # Updates
  expect_error(cac$check_update(), NA)

  # Does not update
  expect_error(cac$check_update(), NA)
})
