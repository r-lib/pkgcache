test_that("new_pkgcache_cond", {
  cnd <- new_pkgcache_cond(
    "a ",
    "message",
    class = "myclass",
    data = list(foo = 1)
  )
  cch <- tryCatch(signalCondition(cnd), pkgcache_condition = identity)
  expect_equal(conditionMessage(cch), "a message")
  expect_s3_class(cch, "myclass")
  expect_equal(cch$foo, 1)
})

test_that("new_pkgcache_warning", {
  cnd <- new_pkgcache_warning(
    "a ",
    "message",
    class = "myclass",
    data = list(foo = 1)
  )
  cch <- tryCatch(warning(cnd), pkgcache_condition = identity)
  expect_equal(conditionMessage(cch), "a message")
  expect_s3_class(cch, "myclass")
  expect_equal(cch$foo, 1)
})
