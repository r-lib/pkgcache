
test_that("crud", {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)

  db <- sql3_open(tmp)
  sql3_exec(db, "CREATE TABLE foo (id INTEGER PRIMARY KEY, name TEXT)")
  sql3_exec(db, "INSERT INTO foo VALUES (1, 'Bugs Bunny')")
  sql3_exec(db, "INSERT INTO foo VALUES (2, 'Donald Duck')")
  ret <- sql3_get_query(db, "SELECT * fROM foo")

  expect_equal(ret$id, 1:2)
  expect_equal(ret$name, c("Bugs Bunny", "Donald Duck"))
})
