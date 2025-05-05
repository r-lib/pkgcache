if (Sys.getenv("R_COVR") == "true") {
  return()
}

test_that("what if cran.r-pkg.org is down?", {
  setup_fake_apps()

  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(pri, rep, "source", bioc = FALSE)

  # First get the metadata, so updates are fast
  expect_error(suppressMessages(cmc$update()), NA)

  # Then simulate a broken site
  withr::local_options(
    pkg.cran_metadata_url = "http://192.0.2.0/"
  )
  withr::local_envvar(
    "PKGCACHE_TIMEOUT" = "2",
    "PKGCACHE_CONNECTTIMEOUT" = "1"
  )

  tic <- Sys.time()
  expect_error(suppressMessages(cmc$update()), NA)
  toc <- Sys.time()

  ## This is slow on older R versions
  limit <- if (getRversion() < "3.4") 30 else 20
  duration <- difftime(toc, tic, units = "secs")
  expect_true(
    duration < as.difftime(limit, units = "secs"),
    info = format(toc - tic)
  )
})
