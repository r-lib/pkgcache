library(testthat)
library(pkgcache)

sessioninfo::package_info("pkgcache", dependencies = TRUE)

test_check("pkgcache")
