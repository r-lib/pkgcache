test_that("get_matching_bioc_version uses builtin map without downloading", {
  gmbv <- bioconductor$.internal$get_matching_bioc_version
  # 4.4 is in the builtin map, so this must not need the YAML file at all
  fake(gmbv, "get_matching_bioc_version_from_yaml", function(...) {
    stop("must not download the YAML file")
  })
  expect_equal(gmbv("4.4.0"), package_version("3.20"))
})

test_that("get_matching_bioc_version uses R's default for the current R", {
  gmbv <- bioconductor$.internal$get_matching_bioc_version
  # An R version that is not in the builtin map, but is the one we are
  # "currently running": we must use R's own associated Bioc version,
  # and must not download the YAML file.
  fake(gmbv, "getRversion", function() package_version("9.9.9"))
  fake(gmbv, "asNamespace", function(ns) {
    list(.BioC_version_associated_with_R_version_default = "3.99")
  })
  fake(gmbv, "get_matching_bioc_version_from_yaml", function(...) {
    stop("must not download the YAML file")
  })
  expect_equal(gmbv("9.9.9"), package_version("3.99"))
})

test_that("get_matching_bioc_version downloads the YAML when needed", {
  on.exit(bioconductor$.internal$clear_cache(), add = TRUE)
  bioconductor$.internal$clear_cache()
  setup_fake_apps()

  gmbv <- bioconductor$.internal$get_matching_bioc_version
  # Pin the "current" R version to an ancient one, so the queried
  # versions always go through the YAML download path.
  fake(gmbv, "getRversion", function() package_version("1.0.0"))

  # R 4.6 matches both the release (3.23) and devel (3.24) Bioc versions
  # in the fixture config, the release one wins.
  expect_equal(gmbv("4.6.0", forget = TRUE), package_version("3.23"))

  # An R version that is newer than anything in the YAML falls back to
  # the devel Bioc version.
  expect_equal(gmbv("5.0.0", forget = TRUE), package_version("3.24"))
})

test_that("get_matching_bioc_version errors helpfully on download failure", {
  on.exit(bioconductor$.internal$clear_cache(), add = TRUE)
  bioconductor$.internal$clear_cache()

  # An app that serves an empty config.yaml, i.e. the download "fails".
  app <- webfakes::new_app()
  app$get("/config.yaml", function(req, res) res$send(""))
  proc <- webfakes::local_app_process(app)
  withr::local_envvar(
    R_BIOC_CONFIG_URL = paste0(proc$url(), "/config.yaml"),
    R_BIOC_VERSION = NA_character_
  )

  gmbv <- bioconductor$.internal$get_matching_bioc_version
  fake(gmbv, "getRversion", function() package_version("1.0.0"))

  err <- tryCatch(gmbv("5.0.0", forget = TRUE), error = function(e) e)
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "disable Bioconductor", fixed = TRUE)
  expect_match(conditionMessage(err), "R_BIOC_VERSION", fixed = TRUE)
})

test_that("get_matching_bioc_version errors helpfully on unparseable YAML", {
  on.exit(bioconductor$.internal$clear_cache(), add = TRUE)
  bioconductor$.internal$clear_cache()

  # An app that serves a config.yaml that we cannot parse, so we cannot
  # determine the Bioc version from it.
  app <- webfakes::new_app()
  app$get("/config.yaml", function(req, res) res$send("not a real config\n"))
  proc <- webfakes::local_app_process(app)
  withr::local_envvar(
    R_BIOC_CONFIG_URL = paste0(proc$url(), "/config.yaml"),
    R_BIOC_VERSION = NA_character_
  )

  gmbv <- bioconductor$.internal$get_matching_bioc_version
  fake(gmbv, "getRversion", function() package_version("1.0.0"))

  expect_error(
    gmbv("5.0.0", forget = TRUE),
    "R_BIOC_VERSION"
  )
})

test_that("default_use_bioconductor defaults to TRUE", {
  withr::local_options(pkg.use_bioconductor = NULL)
  withr::local_envvar(PKG_USE_BIOCONDUCTOR = NA_character_)
  expect_true(default_use_bioconductor())
})

test_that("default_use_bioconductor uses the option first", {
  withr::local_envvar(PKG_USE_BIOCONDUCTOR = "true")
  withr::local_options(pkg.use_bioconductor = FALSE)
  expect_false(default_use_bioconductor())
  withr::local_options(pkg.use_bioconductor = TRUE)
  expect_true(default_use_bioconductor())
})

test_that("default_use_bioconductor validates the option", {
  withr::local_options(pkg.use_bioconductor = "yes")
  expect_error(
    default_use_bioconductor(),
    "must be a boolean flag",
    fixed = TRUE
  )
})

test_that("default_use_bioconductor uses the env var if no option", {
  withr::local_options(pkg.use_bioconductor = NULL)
  for (v in c("yes", "true", "1", "on", "TRUE", "On")) {
    withr::local_envvar(PKG_USE_BIOCONDUCTOR = v)
    expect_true(default_use_bioconductor(), info = v)
  }
  for (v in c("no", "false", "0", "off", "FALSE", "Off")) {
    withr::local_envvar(PKG_USE_BIOCONDUCTOR = v)
    expect_false(default_use_bioconductor(), info = v)
  }
})

test_that("default_use_bioconductor validates the env var", {
  withr::local_options(pkg.use_bioconductor = NULL)
  withr::local_envvar(PKG_USE_BIOCONDUCTOR = "maybe")
  expect_error(
    default_use_bioconductor(),
    "must be",
    fixed = TRUE
  )
})
