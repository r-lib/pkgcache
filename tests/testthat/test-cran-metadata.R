
context("CRAN metadata")

test_that("what if cran.r-pkg.org is down?", {
  skip_if_offline()

  withr::with_envvar(c("R_PKG_CRAN_METADATA_URL" = "http://192.0.2.0/"), {
    dir.create(pri <- fs::path_norm(tempfile()))
    on.exit(unlink(pri, recursive = TRUE), add = TRUE)
    dir.create(rep <- fs::path_norm(tempfile()))
    on.exit(unlink(rep, recursive = TRUE), add = TRUE)
    cmc <- cranlike_metadata_cache$new(pri, rep, "source", bioc = FALSE)
    tic <- Sys.time()
    expect_error(cmc$update(), NA)
    toc <- Sys.time()

    ## This is slow on older R versions
    limit <- if (getRversion() < "3.4") 30 else 20
    expect_true(toc - tic < as.difftime(limit, units = "secs"))
  })
})
