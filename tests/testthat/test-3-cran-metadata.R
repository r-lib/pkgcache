
context("CRAN metadata")

test_that("what if cran.r-pkg.org is down?", {
  skip_if_offline()
  skip_on_cran()

  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(pri, rep, "source", bioc = FALSE)

  # First get the metadata, so updates are fast
  expect_error(cmc$update(), NA)

  # Then simulate a broken web site, giving 5 secs to the non-broken ones
  env <- c(
    "R_PKG_CRAN_METADATA_URL" = "http://192.0.2.0/",
    "PKGCACHE_TIMEOUT" = "5",
    "PKGCACHE_CONNECTTIMEOUT" = "2"
  )

  withr::with_envvar(env, {
    tic <- Sys.time()
    expect_error(cmc$update(), NA)
    toc <- Sys.time()

    ## This is slow on older R versions
    limit <- if (getRversion() < "3.4") 30 else 20
    duration <- difftime(toc, tic, units = "secs")
    expect_true(
      duration < as.difftime(limit, units = "secs"),
      info = format(toc - tic)
    )
  })
})
