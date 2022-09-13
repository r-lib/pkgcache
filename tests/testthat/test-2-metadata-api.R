
pkgs <- dcf("
  Package: pkg1
  Version: 1.0.0

  Package: pkg2
  Version: 1.0.0
  Depends: pkg1

  Package: pkg3
  Version: 1.0.0
  Depends: pkg2
")
cran <- webfakes::local_app_process(
  cran_app(pkgs),
  opts = webfakes::server_opts(num_threads = 3)
)

test_that("metadata api", {

  if (packageVersion("callr") < "3.1.0.9000") skip("Need newer callr")

  # callr copies over the `repos` option to the subprocess
  withr::local_options(
    repos = c(CRAN = cran$url()),
    pkg.cran_metadata_url = cran$url()
  )

  # Need to set this so it holds in the subprocess
  withr::local_envvar(
    R_PKG_CRAN_METADATA_URL = cran$url()
  )

  tmp <- test_temp_dir()
  withr::local_envvar(c(R_PKG_CACHE_DIR = tmp))
  expect_equal(dir(tmp), character())

  fun <- function() {
    pkgcache::meta_cache_update()
    ret1 <- pkgcache::meta_cache_list()
    ret2 <- dir(Sys.getenv("R_PKG_CACHE_DIR"))
    ret3 <- pkgcache::meta_cache_list("pkg3")
    ret4 <- pkgcache::meta_cache_deps("pkg3")
    ret5 <- pkgcache::meta_cache_revdeps("pkg1", recursive = FALSE)
    ret6 <- pkgcache::meta_cache_cleanup(force = TRUE)
    ret7 <- pkgcache::get_cranlike_metadata_cache()
    list(class(ret1), ret2, ret3, ret4, ret5, NULL, class(ret7))
  }

  ret <- callr::r(fun)
  expect_true("tbl" %in% ret[[1]])
  expect_true("R" %in% ret[[2]])
  expect_s3_class(ret[[3]], "tbl")
  expect_true("pkg3" %in% ret[[3]]$package)
  expect_true("pkg3" %in% ret[[4]]$package)
  expect_true("pkg1" %in% ret[[5]]$package)
  expect_true("cranlike_metadata_cache" %in% ret[[7]])
  expect_true("R6" %in% ret[[7]])

  expect_false("_metadata" %in% dir(tmp))
})
