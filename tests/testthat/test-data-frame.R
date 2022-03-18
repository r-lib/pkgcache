
test_that("find_in_data_frame", {
  df <- data.frame(
    stringsAsFactors = FALSE,
    foo = c("foo", "foo", "foo2"),
    bar = c("bar", "bar2", "bar")
  )

  expect_equal(find_in_data_frame(df, foo = "foo"), 1:2)
  expect_equal(find_in_data_frame(df, foo = "foo", bar = "bar"), 1L)
  expect_equal(
    find_in_data_frame(df, bar = "bar", .list = list(foo = "foo")), 1L)
  expect_equal(find_in_data_frame(df, bar = "bar", foo = NULL), c(1L, 3L))
})

test_that("delete_from_data_frame", {
  df <- data.frame(
    stringsAsFactors = FALSE,
    foo = c("foo", "foo", "foo2"),
    bar = c("bar", "bar2", "bar")
  )

  expect_equal(delete_from_data_frame(df, foo = "foo"), df[3,])
  expect_equal(delete_from_data_frame(df, .list = list(bar = "bar2")),
               df[c(1,3),])
  expect_equal(delete_from_data_frame(df, foo = "nope"), df)
})
