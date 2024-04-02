test_that("macos platforms", {
  skip_on_cran()
  expect_snapshot({
    get_cran_macos_platform("4.2")
    get_cran_macos_platform("4.3")
    get_cran_macos_platform("4.4")
    get_cran_macos_platform("4.5")
    get_cran_macos_platform("4.6")
    get_cran_macos_platform("5.0")
    get_cran_macos_platform("10.0")
  })

  # error if we don't know the platform
  # we do this so we can detect the failure on the CI and update
  # the platform
  v <- paste0(getRversion()[,1:2])
  expect_true(v %in% macos_cran_platforms$rversion)

  # In case CRAN changes the platform for R-devel
  if (Sys.info()[["sysname"]] == "Darwin" && .Platform$pkgType != "source") {
    plt <- get_cran_macos_platform(v)
    expect_true(R.Version()$platform %in% plt$platform)
    expect_true(.Platform$pkgType %in% paste0("mac.binary.", plt$subdir))
  }
})
