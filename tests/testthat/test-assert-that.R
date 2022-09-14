
test_that("assert_that", {
  expect_silent({
    assert_that(is.integer(1L))
    assert_that(is.integer(1L), is.character("a"))
  })

  expect_snapshot(error = TRUE, {
    assert_that(is.integer("a"))
  })

  expect_snapshot(error = TRUE, {
    assert_that(is.integer(1L[[2]]))
  })
})

test_that("bad assertions", {
  expect_snapshot(error = TRUE, {
    assert_that(1:10)
  })

  expect_snapshot(error = TRUE, {
    assert_that(c(TRUE, NA))
  })

  expect_snapshot(error = TRUE, {
    assert_that(c(TRUE, TRUE))
  })
})

test_that("call is deparsed correctly", {

  is_integer <- function(x) {
    is.integer(x)
  }

  expect_snapshot(error = TRUE, {
    assert_that(is_integer(1.1))
  })

  expect_snapshot(error = TRUE, {
    assert_that(is_integer(
      1.00001 * 1.00001 * 1.00001 * 1.00001 * 1.00001 *
      1.00001 * 1.00001 * 1.00001 * 1.00001 * 1.00001
    ))
  })
})

test_that("custom failure message", {

  is_count <- function(x) {
    is.integer(x) && length(x) == 1 && !is.na(x) && x >= 0
  }
  on_failure(is_count) <- function(call, env) {
    paste0(deparse(call$x), " is not a count, a positive integer scalar")
  }

  expect_snapshot(error = TRUE, {
    assert_that(is_count("nope"))
  })
})
