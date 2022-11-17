
test_that("repo_get", {
  withr::local_options(repos = character())
  rps <- repo_get()
  expect_s3_class(rps, "tbl")
  expect_equal(
    names(rps),
    c("name", "url", "type", "r_version", "bioc_version")
  )
})

test_that("repo_resolve", {
  # URL
  expect_equal(
    repo_resolve("https://foo.bar"),
    c(EXTRA = "https://foo.bar")
  )
  expect_equal(
    repo_resolve(c(AAA = "https://aaa.a")),
    c(AAA = "https://aaa.a")
  )

  # MRAN
  withr::local_envvar(PKGCACHE_MRAN_URL = NA_character_)
  expect_equal(
    repo_resolve("MRAN@2020-10-10"),
    c(CRAN = "https://cran.microsoft.com/snapshot/2020-10-10")
  )
  expect_equal(
    repo_resolve("MRAN@2020-10-10T14:33:56"),
    c(CRAN = "https://cran.microsoft.com/snapshot/2020-10-10")
  )

  # RSPM
  withr::local_envvar(PKGCACHE_RSPM_URL = NA_character_)
  expect_equal(
    repo_resolve("RSPM@2020-10-10"),
    c(CRAN = "https://packagemanager.posit.co/cran/344")
  )
  expect_equal(
    repo_resolve("RSPM@2020-10-10T14:33:56"),
    c(CRAN = "https://packagemanager.posit.co/cran/344")
  )
})

test_that("repo_add", {
  testthat::local_edition(3)
  withr::local_options(
    repos = character(),
    cli.num_colors = 1L,
    cli.unicode = FALSE
  )
  before <- repo_get()
  repo_add()
  expect_equal(repo_get(), before)

  repo_add(URL = "https://my.url", .list = c(PATH = "/foo/bar"))
  expect_snapshot(repo_get()[1:3,])
})

test_that("with_repo", {
  testthat::local_edition(3)
  withr::local_options(
    repos = character(),
    cli.num_colors = 1L,
    cli.unicode = FALSE
  )
  expect_snapshot(
    with_repo(c(URL = "https://my.url"), repo_get()[1,])
  )
})

test_that("repo_sugar_url", {
  expect_equal(
    repo_sugar_url("foobar", "name"),
    c(name = "foobar")
  )
  expect_equal(
    repo_sugar_url("foo", NULL),
    c(EXTRA = "foo")
  )
})

test_that("repo_sugar_path", {
  expect_equal(
    repo_sugar_path("/x/y", "name"),
    c(name = "/x/y")
  )
  expect_equal(
    repo_sugar_path("/x/y", NULL),
    c(LOCAL = "/x/y")
  )
})

test_that("repo_sugar_mran", {
  withr::local_envvar(PKGCACHE_MRAN_URL = NA_character_)
  expect_error(
    repo_sugar_mran("2015-01-31", NULL),
    "MRAN snapshots go back to 2015-02-01 only"
  )

  expect_equal(
    repo_sugar_mran("2020-01-21", NULL),
    c(CRAN = "https://cran.microsoft.com/snapshot/2020-01-21")
  )

  withr::local_envvar(PKGCACHE_MRAN_URL = "https://my.mran")
  expect_equal(
    repo_sugar_mran("2020-01-21", NULL),
    c(CRAN = "https://my.mran/2020-01-21")
  )
})

test_that("repo_sugar_rspm", {
  withr::local_envvar(PKGCACHE_RSPM_URL = NA_character_)
  expect_equal(
    repo_sugar_rspm("2020-06-30", NULL),
    c(CRAN = "https://packagemanager.posit.co/cran/298")
  )

  withr::local_envvar(PKGCACHE_RSPM_URL = "https://my.rspm")
  expect_equal(
    repo_sugar_rspm("2020-06-30", NULL),
    c(CRAN = "https://my.rspm/298")
  )

  called <- FALSE
  mockery::stub(repo_sugar_rspm, "get_rspm_versions", function(...) {
    called <<- TRUE
    pkgenv$rspm_versions
  })

  expect_error(
    repo_sugar_rspm(as.Date(names(pkgenv$rspm_versions)[1]) - 1, NULL),
    "RSPM snapshots go back to"
  )

  expect_error(
    repo_sugar_rspm(as.Date(last(names(pkgenv$rspm_versions))) + 1, NULL),
    "Cannot find matching RSPM snapshot for"
  )
  expect_true(called)
})

test_that("get_rspm_versions", {
  testthat::local_edition(3)
  withr::local_envvar(
    PKGCACHE_RSPM_TRANSACTIONS_URL = repo$url("/rspmversions")
  )

  ret <- get_rspm_versions()
  expect_snapshot(ret)
})

test_that("parse_spec", {
  expect_equal(
    parse_spec("R-4.0.0"),
    as.Date("2020-04-25")
  )

  expect_equal(
    parse_spec("2019-11-19"),
    as.Date("2019-11-19")
  )

  mockery::stub(parse_spec, "parse_spec_pkg", TRUE)
  expect_equal(
    parse_spec("dplyr-1.0.0"),
    TRUE
  )
})

test_that("parse_spec_r", {
  called <- FALSE
  mockery::stub(parse_spec_r, "get_r_versions", function(...) {
    called <<- TRUE
    pkgenv$r_versions
  })
  expect_error(
    parse_spec_r("100.0.0"),
    "Unknown R version"
  )
  expect_true(called)
})

test_that("get_r_versions", {
  testthat::local_edition(3)
  withr::local_envvar(PKGCACHE_R_VERSIONS_URL = repo$url("/rversions"))

  ret <- get_r_versions()
  expect_snapshot(ret)

  withr::local_envvar(PKGCACHE_R_VERSIONS_URL = repo$url("/rversionsx"))
  expect_error(get_r_versions(), "Failed to download R versions from")
})

test_that("parse_spec_date", {
  expect_s3_class(parse_spec_date("2020-10-20"), "Date")
})

test_that("parse_spec_pkg", {
  testthat::local_edition(3)
  withr::local_envvar(PKGCACHE_CRANDB_URL = repo$url("/crandb/%s"))

  expect_error(parse_spec_pkg("foo-"), "Invalid package version")
  expect_error(parse_spec_pkg("-1.0.1"), "Invalid package version")

  old <- pkgenv$pkg_versions[["dplyr"]]
  on.exit(pkgenv$pkg_versions[["dplyr"]] <- old, add = TRUE)
  pkgenv$pkg_versions[["dplyr"]] <- NULL

  expect_error(
    parse_spec_pkg("dplyr-0.0.0"),
    "Unknown 'dplyr' version: '0.0.0'",
    fixed = TRUE
  )
  expect_snapshot(pkgenv$pkg_versions[["dplyr"]])

  expect_error(
    parse_spec_pkg("foobar-1.0.0"),
    "Cannot find package versions for"
  )

  expect_error(
    parse_spec_pkg("bad-1.0.0"),
    "Failed to download package versions for"
  )

  expect_equal(parse_spec_pkg("dplyr-1.0.0"), as.Date("2020-05-30"))
})
