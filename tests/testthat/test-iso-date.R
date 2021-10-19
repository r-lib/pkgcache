
test_that("handle local time zone", {
  expect_equal(
    parse_iso_8601("2021-10-19 12:30:00", default_tz = ""),
    parse_iso_8601("2021-10-19 12:30:00", default_tz = Sys.timezone())
  )
})

test_that("format_iso_8601", {
  local_edition(3)
  date <- parse_iso_8601("2021-10-19 12:30:00", default_tz = "UTC")
  expect_equal(format_iso_8601(date), "2021-10-19T12:30:00+00:00")
})
