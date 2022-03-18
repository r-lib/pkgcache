
test_that("metadata api", {

  skip_if_offline()
  skip_on_cran()
  if (packageVersion("callr") < "3.1.0.9000") skip("Need newer callr")

  tmp <- test_temp_dir()
  withr::local_envvar(c(R_PKG_CACHE_DIR = tmp))
  expect_equal(dir(tmp), character())

  fun <- function() {
    pkgcache::meta_cache_update()
    ret1 <- pkgcache::meta_cache_list()
    ret2 <- dir(Sys.getenv("R_PKG_CACHE_DIR"))
    ret3 <- pkgcache::meta_cache_list("igraph")
    ret4 <- pkgcache::meta_cache_deps("igraph")
    ret5 <- pkgcache::meta_cache_revdeps("pkgconfig", recursive = FALSE)
    ret6 <- pkgcache::meta_cache_cleanup(force = TRUE)
    ret7 <- pkgcache::get_cranlike_metadata_cache()
    list(class(ret1), ret2, ret3, ret4, ret5, NULL, class(ret7))
  }

  ret <- callr::r(fun)
  expect_true("tbl" %in% ret[[1]])
  expect_true("R" %in% ret[[2]])
  expect_s3_class(ret[[3]], "tbl")
  expect_true("igraph" %in% ret[[3]]$package)
  expect_true("igraph" %in% ret[[4]]$package)
  expect_true("pkgconfig" %in% ret[[5]]$package)
  expect_true("cranlike_metadata_cache" %in% ret[[7]])
  expect_true("R6" %in% ret[[7]])

  expect_false("_metadata" %in% dir(tmp))
})
