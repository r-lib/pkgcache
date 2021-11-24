
test_that("current_r_platform_data", {
  mockery::stub(current_r_platform_data, "get_platform", "x86_64-apple-darwin17.0")
  expect_equal(current_r_platform_data()$platform, "x86_64-apple-darwin17.0")
})

test_that("default_platforms", {
  mockery::stub(default_platforms, "current_r_platform", "macos")
  expect_equal(default_platforms(), c("macos", "source"))

  mockery::stub(default_platforms, "current_r_platform", "windows")
  expect_equal(default_platforms(), c("windows", "source"))

  mockery::stub(default_platforms, "current_r_platform", "source")
  expect_equal(default_platforms(), "source")
})

test_that("get_all_package_dirs", {
  res <- get_all_package_dirs(
    unique(c(current_r_platform(), "source")),
    getRversion()
  )

  expect_s3_class(res, "tbl_df")
  expect_equal(
    colnames(res),
    c("platform", "rversion", "contriburl"))
  expect_gte(nrow(res), 1)
  expect_true(all(sapply(res, is.character)))
  expect_error(get_all_package_dirs("source", "3.1"), "R versions before")
  expect_error(
    get_package_dirs_for_platform("source", "3.1"),
    "R versions before"
  )

  res2 <- get_all_package_dirs("i386+x86_64-w64-mingw32", "4.0")
  res3 <- get_all_package_dirs("windows", "4.0")
  expect_equal(res2, res3)
})

test_that("get_cran_extension", {
  expect_equal(get_cran_extension("source"), ".tar.gz")
  expect_equal(get_cran_extension("windows"), ".zip")
  expect_equal(get_cran_extension("macos"), ".tgz")
  expect_equal(get_cran_extension("x86_64-apple-darwin17.0"), ".tgz")
  expect_equal(
    get_cran_extension("x86_64-pc-linux-musl"),
    "_R_x86_64-pc-linux-musl.tar.gz"
  )
  expect_equal(
    get_cran_extension("foobar"),
    "_R_foobar.tar.gz"
  )
})

test_that("get_all_package_dirs", {
  if (grepl("^aarch64-apple-", R.version$platform)) skip("M1")
  d <- get_all_package_dirs(c("macos", "source"), "4.0.0")
  expect_true("x86_64-apple-darwin17.0" %in% d$platform)
  expect_true("source" %in% d$platform)

  expect_error(
    get_all_package_dirs("windows", "2.15.0"),
    "does not support packages for R versions before"
  )
  expect_error(
    get_all_package_dirs("macos", "3.1.3"),
    "does not support packages for R versions before"
  )

  d <- get_all_package_dirs("macos", "3.2.0")
  expect_equal(
    sort(d$contriburl),
    "bin/macosx/mavericks/contrib/3.2"
  )
  d <- get_all_package_dirs("macos", "3.3.0")
  expect_match(d$contriburl, "bin/macosx/mavericks/contrib/3.3")
  d <- get_all_package_dirs("macos", "3.6.3")
  expect_match(d$contriburl, "bin/macosx/el-capitan/contrib/3.6")

  d <- get_all_package_dirs("macos", "4.0.0")
  expect_match(d$contriburl, "bin/macosx/contrib/4.0")
})

test_that("current_r_platform_data_linux", {
  testthat::local_edition(3)
  dists <- dir(test_path("fixtures", "linux"))
  vers <- lapply(dists, function(d) dir(test_path("fixtures", "linux", d)))

  nlapply <- function(X, FUN, ...) {
    ret <- lapply(X, FUN, ...)
    if (is.character(X) && is.null(names(ret))) names(ret) <- X
    ret
  }

  raw <- data.frame(stringsAsFactors = FALSE, raw = "foo")
  mapply(dists, vers, FUN = function(d, v) {
    etc <- test_path("fixtures", "linux", d, v)
    expect_snapshot(nlapply(etc, current_r_platform_data_linux, raw = raw))
  })
})

test_that("linux", {
  mockery::stub(current_r_platform_data, "get_platform", "x86_64-pc-linux-gnu")
  mockery::stub(
    current_r_platform_data,
    "current_r_platform_data_linux",
    data.frame(stringsAsFactors = FALSE, x = "boo")
  )
  expect_equal(current_r_platform_data()$platform, "boo")
})

test_that("unknown linux", {
  dummy <- data.frame(stringsAsFactors = FALSE, x = "foo")
  expect_equal(
    current_r_platform_data_linux(dummy, tempfile())$distribution,
    "unknown"
  )
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dir.create(tmp)
  file.create(file.path(tmp, "os-release"))
  expect_equal(
    current_r_platform_data_linux(dummy, tmp)$distribution,
    "unknown"
  )
})

test_that("remove_quotes", {
  expect_equal(remove_quotes("x"), "x")
  expect_equal(remove_quotes("'xyz'"), "xyz")
  expect_equal(remove_quotes('"xyz"'), "xyz")
})

test_that("parse_redhat_release", {
  expect_equal(parse_redhat_release(""), unknown_dist())
  expect_equal(parse_redhat_release("Something")$distribution, "something")
})

test_that("default_cran_mirror", {
  local_edition(3)
  expect_snapshot(withr::with_options(
    list(repos = NULL),
    default_cran_mirror()
  ))

  expect_snapshot(withr::with_options(
    list(repos = list(ACME = "https://acme.com")),
    default_cran_mirror()
  ))

  expect_snapshot(withr::with_options(
    list(repos = c(ACME = "https://acme.com")),
    default_cran_mirror()
  ))

  expect_snapshot(withr::with_options(
    list(repos = list(CRAN = "@CRAN@")),
    default_cran_mirror()
  ))

  expect_snapshot(withr::with_options(
    list(repos = c(CRAN = "@CRAN@")),
    default_cran_mirror()
  ))

  expect_snapshot(withr::with_options(
    list(repos = list(CRAN = "https://mycran.com")),
    default_cran_mirror()
  ))

  expect_snapshot(withr::with_options(
    list(repos = c(CRAN = "https://mycran.com")),
    default_cran_mirror()
  ))
})

test_that("bioc_version", {
  local_edition(3)
  withr::local_options(useFancyQuotes = FALSE)
  expect_snapshot({
    bioc_version("4.1.1")
    bioc_version("4.0.0")
    bioc_version("3.6.0")
  })
})

test_that("bioc_version_map", {
  local_edition(3)
  withr::local_options(useFancyQuotes = FALSE)
  expect_snapshot(as.data.frame(bioc_version_map()))
})

test_that("bioc_release_version, bioc_devel_version", {
  # This will fail when a new bioc devel version is out
  skip_on_cran()
  local_edition(3)
  withr::local_options(useFancyQuotes = FALSE)
  expect_snapshot(bioc_release_version())
  expect_snapshot(bioc_devel_version())
})

test_that("bioc_repos", {
  local_edition(3)
  withr::local_options(useFancyQuotes = FALSE)
  withr::local_envvar(c(R_BIOC_MIRROR = "https://bioconductor.org"))
  expect_snapshot(
    bioc_repos("3.13")
  )
})
