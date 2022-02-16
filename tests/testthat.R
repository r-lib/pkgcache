
if (Sys.getenv("NOT_CRAN") == "true") {
  library(testthat)
  library(pkgcache)
  test_check("pkgcache", reporter = "summary")
}
