test_that("timed out", {
  skip_on_cran()
  testthat::local_edition(3)
  f <- function() delay(1 / 10)$then(function(value) "OK")
  expect_snapshot(error = TRUE, synchronise(async_timeout(f, 1 / 1000)))
})

test_that("did not time out", {
  skip_on_cran()
  f <- function() delay(1 / 100)$then(function(value) "OK")
  expect_equal(synchronise(async_timeout(f, 1 / 10)), "OK")
})

test_that("error before async_timeout", {
  skip_on_cran()
  testthat::local_edition(3)
  f <- function() delay(1 / 1000)$then(function(value) stop("oops"))
  expect_snapshot(error = TRUE, synchronise(async_timeout(f, 1 / 10)))
})

test_that("error after async_timeout", {
  skip_on_cran()
  testthat::local_edition(3)
  f <- function() delay(1 / 10)$then(function(value) stop("oops"))
  expect_snapshot(error = TRUE, synchronise(async_timeout(f, 1 / 1000)))
})
