if (Sys.getenv("R_COVR") == "true") {
  return()
}

test_that("current_r_platform_data", {
  fake(current_r_platform_data, "get_platform", "x86_64-apple-darwin17.0")
  expect_equal(current_r_platform_data()$platform, "x86_64-apple-darwin17.0")
})

test_that("default_platforms", {
  fake(default_platforms, "current_r_platform", "macos")
  expect_equal(default_platforms(), c("macos", "source"))

  fake(default_platforms, "current_r_platform", "windows")
  expect_equal(default_platforms(), c("windows", "source"))

  fake(default_platforms, "current_r_platform", "source")
  expect_equal(default_platforms(), "source")
})

test_that("parse_platform", {
  expect_snapshot({
    parse_platform(c(
      "something-else",
      "aarch64-apple-darwin20",
      "x86_64-w64-mingw32",
      "i386+x86_64-w64-mingw32",
      "aarch64-pc-linux",
      "aarch64-pc-linux-gnu",
      "aarch64-pc-linux-ubuntu",
      "aarch64-pc-linux-ubuntu-22.04",
      "aarch64-pc-linux-ubuntu-22.04-libc++",
      "aarch64-pc-linux-gnu-ubuntu",
      "aarch64-pc-linux-gnu-ubuntu-24.04",
      "aarch64-pc-linux-gnu-ubuntu-24.04-libc++",
      "aarch64-pc-linux-musl-alpine-13.4"
    ))
  })
})

test_that("re_linux_platform", {
  expect_snapshot({
    re_match(
      c(
        "something-else",
        "linux",
        "linux-gnu",
        "linux-ubuntu",
        "linux-ubuntu-22.04",
        "linux-ubuntu-22.04-libc++",
        "linux-gnu-ubuntu",
        "linux-gnu-ubuntu-24.04",
        "linux-gnu-ubuntu-24.04-libc++",
        "linux-musl-alpine-13.4"
      ),
      re_linux_platform()
    )
  })
})

test_that("get_all_package_dirs", {
  res <- get_all_package_dirs(
    unique(c(current_r_platform(), "source")),
    getRversion()
  )

  expect_s3_class(res, "tbl")
  expect_equal(
    colnames(res),
    c("platform", "rversion", "contriburl")
  )
  expect_gte(nrow(res), 1)
  expect_true(all(sapply(res, is.character)))
  expect_snapshot(error = TRUE, {
    get_all_package_dirs("source", "3.1")
    get_package_dirs_for_platform("source", "3.1")
  })

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

  expect_snapshot(error = TRUE, {
    get_all_package_dirs("windows", "2.15.0")
    get_all_package_dirs("macos", "3.1.3")
  })

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
  fake(current_r_platform_data, "get_platform", "x86_64-pc-linux-gnu")
  fake(
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

# If this or the next test fails, that means that we potentially
# need to update our Bioc metadata. If the R version -> Bioc version
# mapping is final, then we update it. Otherwise we don't. E.g.
# R 4.4 uses Bioc 3.19 right now, but eventually will use Bioc 3.20,
# so we don't update for Bioc 3.19, only for Bioc 3.20.
# If we don't update the metadata, we just update the test snapshots.

test_that("bioc_version_map", {
  on.exit(bioconductor$.internal$clear_cache())
  bioconductor$.internal$clear_cache()
  # This does need the internet, because we use it to check that our
  # bioc metadata snapshot is current
  skip_if_offline()
  local_edition(3)
  withr::local_options(useFancyQuotes = FALSE)
  expect_snapshot(as.data.frame(bioc_version_map(forget = TRUE)))
})

test_that("bioc_release_version, bioc_devel_version", {
  # This will fail when a new bioc devel version is out. If this is an odd
  # Bioc version, then we don'r include that in the version map, because
  # it will eventually change. In this case only update the snapshots.
  # If there is a new even version out, then also update the mappings.
  on.exit(bioconductor$.internal$clear_cache())
  bioconductor$.internal$clear_cache()
  skip_on_cran()
  skip_if_offline()
  local_edition(3)
  withr::local_options(useFancyQuotes = FALSE)
  expect_snapshot(bioc_release_version(forget = TRUE))
  expect_snapshot(bioc_devel_version(forget = TRUE))
})

test_that("bioc_repos", {
  on.exit(bioconductor$.internal$clear_cache())
  bioconductor$.internal$clear_cache()
  local_edition(3)
  withr::local_options(useFancyQuotes = FALSE)
  withr::local_envvar(c(R_BIOC_MIRROR = "https://bioconductor.org"))
  expect_snapshot(
    bioc_repos("3.13")
  )
})

test_that("valid_platform_string", {
  expect_true(valid_platform_string("a-b-c"))
  expect_true(valid_platform_string("a-b-c-"))
  expect_true(valid_platform_string("a-b-c-d"))
  expect_true(valid_platform_string("foo-bar-cup"))
  expect_true(valid_platform_string("foo-bar-cup-boo"))

  expect_false(valid_platform_string("-a-b-c"))
  expect_false(valid_platform_string("a---c"))
  expect_false(valid_platform_string("foo-bar"))
  expect_false(valid_platform_string("foobar"))
})

test_that("option, env var", {
  withr::local_options(pkg.current_platform = "foo-bar-foobar")
  expect_equal(current_r_platform(), "foo-bar-foobar")

  withr::local_options(pkg.current_platform = 1:10)
  expect_snapshot(
    error = TRUE,
    current_r_platform()
  )
  withr::local_options(pkg.current_platform = "foobar")
  expect_snapshot(
    error = TRUE,
    current_r_platform()
  )

  withr::local_options(pkg.current_platform = NULL)
  withr::local_envvar(PKG_CURRENT_PLATFORM = "foobar-foo-bar")
  expect_equal(current_r_platform(), "foobar-foo-bar")

  withr::local_envvar(PKG_CURRENT_PLATFORM = "foobar")
  expect_snapshot(
    error = TRUE,
    current_r_platform()
  )
})

test_that("platform with flavors", {
  withr::local_options(
    pkg.current_platform = "x86_64-pc-linux-gnu-ubuntu-22.04-libc++"
  )
  expect_snapshot(current_r_platform_data())
  expect_snapshot(current_r_platform())
})
