
if (file.exists("async")) {
  library(testthat)
  library(pkgcache)
  print(sessioninfo::package_info("pkgcache", dependencies = TRUE))

  test <- function() {
    package <- "pkgcache"
    env_test <- asNamespace("testthat")$env_test
    env_test$in_test <- TRUE
    env_test$package <- package
    on.exit({
      env_test$in_test <- FALSE
      env_test$package <- NULL
    })
    test_path <- "async"
    asNamespace("testthat")$test_package_dir(
      package = package, test_path = test_path,
      filter = NULL, reporter = "summary")
  }
  test()
}
