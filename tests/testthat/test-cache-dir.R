
test_that("tools vs rappdirs", {
  skip_on_cran()
  if (getRversion() < "4.0.0") skip("Needs newer R")

  withr::local_envvar(
    "_R_CHECK_PACKAGE_NAME_" = NA_character_,
    R_USER_CACHE_DIR = NA_character_,
    R_PKG_CACHE_DIR = NA_character_
  )

  d1 <- my_R_user_dir("foo", "cache")
  d2 <- tools::R_user_dir("foo", "cache")

  mkdirp(d1)
  mkdirp(d2)

  expect_equal(normalizePath(d1), normalizePath(d2))

  withr::local_envvar(
    "_R_CHECK_PACKAGE_NAME_" = NA_character_,
    R_USER_CACHE_DIR = tempfile(),
    R_PKG_CACHE_DIR = NA_character_
  )

  d1 <- my_R_user_dir("foo", "cache")
  d2 <- tools::R_user_dir("foo", "cache")

  mkdirp(d1)
  mkdirp(d2)

  expect_equal(normalizePath(d1), normalizePath(d2))
})
