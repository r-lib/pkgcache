
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

test_that("error in R CMD check", {
  withr::local_envvar(
    "_R_CHECK_PACKAGE_NAME_" = "foo",
    "R_USER_CACHE_DIR" = NA_character_,
    "R_PKG_CACHE_DIR" = NA_character_
  )
  expect_error(
    get_user_cache_dir(),
    "env var not set during package check"
  )
})

test_that("fall back to R_USER_CACHE_DIR via R_user_dir()", {
  args <- NULL
  mockery::stub(
    get_user_cache_dir,
    "R_user_dir",
    function(...) {
      args <<- list(...)
      stop("wait")
    }
  )

  withr::local_envvar(
    "R_PKG_CACHE_DIR" = NA_character_,
    "R_USER_CACHE_DIR" = tempdir()
  )

  expect_error(get_user_cache_dir(), "wait")
  expect_equal(args, list("pkgcache", "cache"))
})

test_that("cleanup_old_cache_dir", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  mockery::stub(cleanup_old_cache_dir, "user_cache_dir", function(...) tmp)
  expect_message(cleanup_old_cache_dir(), "nothing to do")

  mkdirp(tmp)
  mockery::stub(cleanup_old_cache_dir, "interactive", FALSE)
  expect_error(cleanup_old_cache_dir(), "non-interactive session")

  mockery::stub(cleanup_old_cache_dir, "interactive", TRUE)
  mockery::stub(cleanup_old_cache_dir, "readline", "n")
  expect_error(cleanup_old_cache_dir(), "Aborted")

  expect_true(file.exists(tmp))
  mockery::stub(cleanup_old_cache_dir, "readline", "y")
  expect_message(cleanup_old_cache_dir(), "Cleaned up cache")
  expect_false(file.exists(tmp))
})
