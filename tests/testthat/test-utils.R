
context("utils")

test_that("current_r_platform", {
  mockery::stub(current_r_platform, "get_platform",
                list(pkgType = "mac.binary"))
  expect_equal(current_r_platform(), "macos")

  mockery::stub(current_r_platform, "get_platform",
                list(pkgType = "win.binary"))
  expect_equal(current_r_platform(), "windows")

  mockery::stub(current_r_platform, "get_platform", list(pkgType = "source"))
  expect_equal(current_r_platform(), "source")

  mockery::stub(current_r_platform, "get_platform", list(pkgType = "foobar"))
  expect_equal(current_r_platform(), "source")

  mockery::stub(current_r_platform, "get_platform", list(pkgType = NULL))
  expect_equal(current_r_platform(), "source")
})

test_that("default_cran_mirror", {
  m1 <- withr::with_options(
    list(repos = c(CRAN = "@CRAN@")),
    default_cran_mirror()
  )
  m2 <- withr::with_options(
    list(repos = NULL),
    default_cran_mirror()
  )
  m3 <- withr::with_options(
    list(repos = c("foo" = "bar")),
    default_cran_mirror()
  )

  expect_true(is.character(m1) && length(m1) == 1 && !is.na(m1))
  expect_identical(m1, m2)
  expect_identical(m1, m3)

  m4 <- withr::with_options(
    list(repos = c(CRAN = "mymirror")),
    default_cran_mirror()
  )
  expect_identical(m4, c(CRAN = "mymirror"))
})

test_that("current_r_version", {
  ver <- current_r_version()
  expect_true(is.character(ver))
  expect_true(length(ver) == 1)
})

test_that("vlapply", {
  l <- list(NULL, "", character(), 1)
  expect_identical(
    vapply(l, is.character, logical(1)),
    vlapply(l, is.character)
  )
  expect_identical(
    vapply(list(), is.character, logical(1)),
    vlapply(list(), is.character)
  )
  expect_error(vlapply(l, identity), "values must be length 1")
  expect_error(vlapply(1:5, identity), "values must be type .*logical")
})

test_that("is_na_scalar", {
  pos <- list(NA, NA_character_, NA_real_, NA_integer_, NA_complex_)
  neg <- list(logical(), integer(), 1, 1L, NULL, "foobar", c(NA, 1))

  for (p in pos) expect_true(is_na_scalar(p))
  for (n in neg) expect_false(is_na_scalar(n))
})

test_that("get_all_package_dirs", {
  res <- get_all_package_dirs(
    unique(c(current_r_platform(), "source")), current_r_version())

  expect_s3_class(res, "tbl_df")
  expect_equal(
    colnames(res),
    c("platform", "rversion", "contriburl", "prefix"))
  expect_gte(nrow(res), 1)
  expect_true(all(sapply(res, is.character)))
})
