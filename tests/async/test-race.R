test_that("race() rejects (#76)", {
  testthat::local_edition(3)
  defer_fail <- function() {
    deferred$new(action = function(resolve) stop("foo"))
  }

  expect_snapshot(
    error = TRUE,
    synchronise(async_race(
      delay(0.1),
      defer_fail()
    ))
  )
})
