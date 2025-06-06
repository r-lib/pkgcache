test_that("rejection", {
  do <- async(function() {
    dx <- delay(1 / 10000)$then(function() stop("ohno!"))$catch(
      error = function(.) expect_match(.$message, "ohno!")
    )
  })
  synchronise(do())
})

test_that("error propagates", {
  do <- async(function() {
    called <- FALSE
    dx <- delay(1 / 10000)$then(function(.) .)$then(
      function() stop("ohno!")
    )$then(function(x) called <<- TRUE)

    dx$catch(error = function(.) expect_match(.$message, "ohno!"))$then(
      function(x) expect_false(called)
    )
  })
  synchronise(do())
})

test_that("handled error is not an error any more", {
  do <- async(function() {
    delay(1 / 10000)$then(function(x) stop("ohno!"))$catch(
      error = function(x) "OK"
    )$then(function(.) expect_equal(., "OK"))$catch(
      error = function() stop("not called")
    )
  })
  synchronise(do())
})

test_that("catch", {
  do <- async(function() {
    dx <- delay(1 / 1000)$then(function(.) .)$then(
      function() stop("ooops")
    )$then(function() "not this one")$catch(
      error = function(.) "nothing to see here"
    )
  })
  expect_equal(
    synchronise(do()),
    "nothing to see here"
  )
})

test_that("finally", {
  testthat::local_edition(3)
  called <- FALSE
  do <- async(function() {
    delay(1 / 1000)$then(function(.) .)$then(function() stop("oops"))$then(
      function() "not this one"
    )$finally(function() called <<- TRUE)
  })
  expect_snapshot(error = TRUE, synchronise(do()))
  expect_true(called)

  called <- FALSE
  do <- async(function() {
    delay(1 / 1000)$then(function(.) .)$then(function() "this one")$finally(
      function() called <<- TRUE
    )
  })
  expect_equal(synchronise(do()), "this one")
  expect_true(called)
})

test_that("error in action", {
  do <- function() {
    deferred$new(function(resolve, reject) stop("foobar"))
  }

  err <- tryCatch(synchronise(do()), error = identity)
  expect_s3_class(err, "async_rejected")
  expect_match(conditionMessage(err), "foobar")
})

test_that("error in then function", {
  do <- function() {
    delay(1 / 100)$then(function(x) stop("foobar"))
  }

  err <- tryCatch(synchronise(do()), error = identity)
  expect_s3_class(err, "async_rejected")
  expect_match(conditionMessage(err), "foobar")
})

test_that("can catch error in action", {
  do <- function() {
    deferred$new(function(resolve, reject) stop("foobar"))$catch(
      error = function(e) e
    )
  }

  err <- synchronise(do())
  expect_s3_class(err, "async_rejected")
  expect_match(conditionMessage(err), "foobar")
})

test_that("can catch error in then function", {
  do <- function() {
    delay(1 / 100)$then(function(x) stop("foobar"))$catch(error = function(e) e)
  }

  err <- synchronise(do())
  expect_s3_class(err, "async_rejected")
  expect_match(conditionMessage(err), "foobar")
})

test_that("catch handers", {
  spec <- foobar1 <- foobar2 <- NULL
  do <- async(function() {
    async_constant(42)$then(function() {
      err <- structure(
        list(message = "foobar"),
        class = c("foobar", "error", "condition")
      )
      stop(err)
    })$catch(special = function(e) spec <<- e)$catch(
      foobar = function(e) foobar1 <<- e
    )$then(function(e) foobar2 <<- e)$then(function() "ok")
  })

  expect_equal(synchronise(do()), "ok")
  expect_null(spec)
  expect_s3_class(foobar1, "foobar")
  expect_s3_class(foobar1, "error")
  expect_s3_class(foobar2, "foobar")
  expect_s3_class(foobar2, "error")
})
