if (
  file.exists("async") &&
    Sys.getenv("NOT_CRAN") == "true" &&
    Sys.getenv("R_COVR") == ""
) {
  library(testthat)
  library(pkgcache)
  print(sessioninfo::package_info("pkgcache", dependencies = TRUE))

  test_dir(
    "async",
    env = new.env(parent = asNamespace("pkgcache")),
    reporter = "summary"
  )
}
