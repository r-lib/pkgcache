if (Sys.getenv("R_COVR") == "true") {
  return()
}

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
    c(CRAN = "https://packagemanager.posit.co/cran/2020-10-10")
  )
  expect_equal(
    repo_resolve("MRAN@2020-10-10T14:33:56"),
    c(CRAN = "https://packagemanager.posit.co/cran/2020-10-10")
  )

  # PPM
  withr::local_envvar(PKGCACHE_PPM_URL = NA_character_)
  withr::local_envvar(PKGCACHE_PPM_BINARIES = "false")
  withr::local_options(repos = NULL)
  expect_equal(
    repo_resolve("PPM@latest"),
    c(CRAN = "https://packagemanager.posit.co/cran/latest")
  )
  expect_equal(
    repo_resolve("PPM@2021-02-04"),
    c(CRAN = "https://packagemanager.posit.co/cran/2021-02-04")
  )
  expect_equal(
    repo_resolve("PPM@2021-02-04T14:33:56"),
    c(CRAN = "https://packagemanager.posit.co/cran/2021-02-04")
  )

  # RSPM
  withr::local_envvar(PKGCACHE_PPM_URL = NA_character_)
  withr::local_envvar(PKGCACHE_PPM_BINARIES = "false")
  expect_equal(
    repo_resolve("RSPM@2021-02-04"),
    c(CRAN = "https://packagemanager.posit.co/cran/2021-02-04")
  )
  expect_equal(
    repo_resolve("RSPM@2021-02-04T14:33:56"),
    c(CRAN = "https://packagemanager.posit.co/cran/2021-02-04")
  )
})


test_that("repo_resolve with PPM", {
  withr::local_envvar(
    PKGCACHE_PPM_URL = NA_character_,
    PKGCACHE_PPM_BINARIES = "true",
    PKGCACHE_PPM_TRANSACTIONS_URL = repo$url("/ppmversions"),
    PKGCACHE_PPM_STATUS_URL = repo$url("/ppmstatus")
  )
  withr::local_options(repos = NULL)

  fake(
    repo_sugar_ppm,
    "current_r_platform_data",
    data.frame(
      stringsAsFactors = FALSE,
      cpu = "x86_64",
      vendor = "pc",
      os = "linux-gnu",
      distribution = "ubuntu",
      release = "22.04",
      platform = "x86_64-pc-linux-gnu-ubuntu-22.04"
    )
  )

  fake(repo_sugar_ppm, "getRversion", "4.2.2")
  expect_equal(
    repo_sugar_ppm("PPM@2021-01-26", nm = NULL),
    c(CRAN = "https://packagemanager.posit.co/cran/__linux__/jammy/2021-01-26")
  )

  fake(repo_sugar_ppm, "getRversion", "1.0.0")
  expect_equal(
    repo_sugar_ppm("PPM@2021-01-26", nm = NULL),
    c(CRAN = "https://packagemanager.posit.co/cran/2021-01-26")
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
  expect_snapshot(repo_get()[1:3, ])
})

test_that("with_repo", {
  testthat::local_edition(3)
  withr::local_options(
    repos = character(),
    cli.num_colors = 1L,
    cli.unicode = FALSE
  )
  expect_snapshot(
    with_repo(c(URL = "https://my.url"), repo_get()[1, ])
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
  expect_snapshot(error = TRUE, repo_sugar_mran("2017-01-31", NULL))

  expect_equal(
    repo_sugar_mran("2020-01-21", NULL),
    c(CRAN = "https://packagemanager.posit.co/cran/2020-01-21")
  )

  withr::local_envvar(PKGCACHE_MRAN_URL = "https://my.mran")
  expect_equal(
    repo_sugar_mran("2020-01-21", NULL),
    c(CRAN = "https://my.mran/2020-01-21")
  )
})

test_that("repo_sugar_ppm", {
  withr::local_envvar(PKGCACHE_PPM_URL = NA_character_)
  withr::local_envvar(PKGCACHE_PPM_BINARIES = "false")
  withr::local_options(repos = NULL)
  expect_equal(
    repo_sugar_ppm("2021-02-04", NULL),
    c(CRAN = "https://packagemanager.posit.co/cran/2021-02-04")
  )

  withr::local_envvar(
    PKGCACHE_PPM_URL = "https://my.ppm",
    PKGCACHE_PPM_REPO = "repo"
  )
  expect_equal(
    repo_sugar_ppm("2021-02-04", NULL),
    c(CRAN = "https://my.ppm/repo/2021-02-04")
  )

  called <- FALSE
  fake(repo_sugar_ppm, "synchronise", function(...) {
    called <<- TRUE
    NULL
  })

  expect_snapshot(
    error = TRUE,
    repo_sugar_ppm(as.Date("2017-10-01"), NULL)
  )
  expect_true(called)
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

  fake(parse_spec, "parse_spec_pkg", TRUE)
  expect_equal(
    parse_spec("dplyr-1.0.0"),
    TRUE
  )
})

test_that("parse_spec_r", {
  called <- FALSE
  fake(parse_spec_r, "get_r_versions", function(...) {
    called <<- TRUE
    pkgenv$r_versions
  })
  expect_snapshot(error = TRUE, parse_spec_r("100.0.0"))
  expect_true(called)
})

test_that("get_r_versions", {
  testthat::local_edition(3)
  withr::local_envvar(PKGCACHE_R_VERSIONS_URL = repo$url("/rversions"))

  ret <- get_r_versions()
  expect_snapshot(ret)

  withr::local_envvar(PKGCACHE_R_VERSIONS_URL = repo$url("/rversionsx"))
  expect_snapshot(error = TRUE, get_r_versions(), transform = fix_port)
})

test_that("parse_spec_date", {
  expect_s3_class(parse_spec_date("2020-10-20"), "Date")
})

test_that("parse_spec_pkg", {
  testthat::local_edition(3)
  withr::local_envvar(PKGCACHE_CRANDB_URL = repo$url("/crandb/%s"))

  expect_snapshot(error = TRUE, {
    parse_spec_pkg("foo-")
    parse_spec_pkg("-1.0.1")
  })

  old <- pkgenv$pkg_versions[["dplyr"]]
  on.exit(pkgenv$pkg_versions[["dplyr"]] <- old, add = TRUE)
  pkgenv$pkg_versions[["dplyr"]] <- NULL

  expect_snapshot(error = TRUE, parse_spec_pkg("dplyr-0.0.0"))
  expect_snapshot(pkgenv$pkg_versions[["dplyr"]])

  expect_snapshot(error = TRUE, parse_spec_pkg("foobar-1.0.0"))
  expect_snapshot(
    error = TRUE,
    parse_spec_pkg("bad-1.0.0"),
    transform = fix_port
  )

  expect_equal(parse_spec_pkg("dplyr-1.0.0"), as.Date("2020-05-30"))
})
