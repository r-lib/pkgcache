test_that("def__make_parent_*", {
  testthat::local_edition(3)
  good <- list(
    NULL,
    function(x) x,
    function() 42,
    function(value, resolve) resolve(value)
  )

  bad <- list(
    123,
    function(a, b, c) resolve(value)
  )

  eta <- function(value, resolve) {
  }

  for (f in good) {
    res <- def__make_parent_resolve(f)
    expect_equal(formals(res), formals(eta))
    res2 <- def__make_parent_reject(f)
    expect_equal(formals(res2), formals(eta))
  }

  for (f in bad) {
    expect_snapshot(error = TRUE, def__make_parent_resolve(f))
    expect_snapshot(error = TRUE, def__make_parent_reject(f))
  }
})

test_that("def__make_parent_resolve", {
  ## NULL
  r1 <- def__make_parent_resolve(NULL)
  res <- NULL
  val <- NULL
  r1(42, function(x) {
    res <<- "resolve"
    val <<- x
  })
  expect_equal(res, "resolve")
  expect_equal(val, 42)

  ## function without args
  r2 <- def__make_parent_resolve(function() 42 * 42)
  res <- NULL
  val <- NULL
  r2(42, function(x) {
    res <<- "resolve"
    val <<- x
  })
  expect_equal(res, "resolve")
  expect_equal(val, 42 * 42)

  ## function with value arg
  r2 <- def__make_parent_resolve(function(val) val)
  res <- NULL
  val <- NULL
  r2(42, function(x) {
    res <<- "resolve"
    val <<- x
  })
  expect_equal(res, "resolve")
  expect_equal(val, 42)
})

test_that("def__make_parent_resolve", {
  testthat::local_edition(3)
  ## NULL
  r1 <- def__make_parent_reject(NULL)
  res <- NULL
  val <- NULL
  expect_snapshot(
    error = TRUE,
    r1("foobar", function(x) {
      res <<- "resolve"
      val <<- x
    })
  )
  expect_null(res)
  expect_null(val)

  ## function without args
  r2 <- def__make_parent_reject(function() 42 * 42)
  res <- NULL
  val <- NULL
  r2(42, function(x) {
    res <<- "resolve"
    val <<- x
  })
  expect_equal(res, "resolve")
  expect_equal(val, 42 * 42)

  ## function with value arg
  r2 <- def__make_parent_reject(function(val) val)
  res <- NULL
  val <- NULL
  r2(42, function(x) {
    res <<- "resolve"
    val <<- x
  })
  expect_equal(res, "resolve")
  expect_equal(val, 42)
})
