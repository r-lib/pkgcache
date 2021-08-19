
test_that("current_r_platform", {
  mockery::stub(current_r_platform, "get_platform", "x86_64-apple-darwin17.0")
  expect_equal(current_r_platform(), "x86_64-apple-darwin17.0")
})

test_that("default_platforms", {
  mockery::stub(default_platforms, "current_r_platform", "macos")
  expect_equal(default_platforms(), c("macos", "source"))

  mockery::stub(default_platforms, "current_r_platform", "windows")
  expect_equal(default_platforms(), c("windows", "source"))

  mockery::stub(default_platforms, "current_r_platform", "source")
  expect_equal(default_platforms(), "source")
})

test_that("get_all_package_dirs", {
  res <- get_all_package_dirs(
    unique(c(current_r_platform(), "source")), getRversion())

  expect_s3_class(res, "tbl_df")
  expect_equal(
    colnames(res),
    c("platform", "rversion", "contriburl"))
  expect_gte(nrow(res), 1)
  expect_true(all(sapply(res, is.character)))
})

test_that("get_cran_extension", {
  expect_equal(
    get_cran_extension("x86_64-pc-linux-musl"),
    "_R_x86_64-pc-linux-musl.tar.gz"
  )
  expect_equal(
    get_cran_extension("foobar"),
    "_R_foobar.tar.gz"
  )
})

test_that("get_all_package_dirs", {
  d <- get_all_package_dirs(c("macos", "source"), "4.0.0")
  expect_true("x86_64-apple-darwin17.0" %in% d$platform)
  expect_true("source" %in% d$platform)

  expect_error(
    get_all_package_dirs("windows", "2.15.0"),
    "does not support packages for R versions before"
  )
  expect_error(
    get_all_package_dirs("macos", "3.1.3"),
    "does not support packages for R versions before"
  )

  d <- get_all_package_dirs("macos", "3.2.0")
  expect_equal(
    sort(d$contriburl),
    "bin/macosx/mavericks/contrib/3.2"
  )
  d <- get_all_package_dirs("macos", "3.3.0")
  expect_match(d$contriburl, "bin/macosx/mavericks/contrib/3.3")
  d <- get_all_package_dirs("macos", "3.6.3")
  expect_match(d$contriburl, "bin/macosx/el-capitan/contrib/3.6")

  d <- get_all_package_dirs("macos", "4.0.0")
  expect_match(d$contriburl, "bin/macosx/contrib/4.0")
})

test_that("current_r_platform_linux", {
  testthat::local_edition(3)
  dists <- dir(test_path("fixtures", "linux"))
  vers <- lapply(dists, function(d) dir(test_path("fixtures", "linux", d)))

  mapply(dists, vers, FUN = function(d, v) {
    etc <- test_path("fixtures", "linux", d, v)
    expect_snapshot(vcapply(etc, current_r_platform_linux, raw = "foo"))
  })
})
