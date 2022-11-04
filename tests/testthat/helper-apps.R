
http <- webfakes::new_app_process(
  webfakes::httpbin_app(), opts = webfakes::server_opts(num_threads = 3)
)

repo_app <- function() {
  app <- webfakes::new_app()

  app$get("/rspmversions", function(req, res) {
    res$send_json(
      text = readLines(testthat::test_path("fixtures/rspm-versions.json"))
    )
  })

  app$get("/rversions", function(req, res) {
    res$send_json(
      text = readLines(testthat::test_path("fixtures/r-versions.json"))
    )
  })

  app$get("/crandb/:pkg", function(req, res) {
    if (req$params$pkg == "dplyr") {
      res$send_json(
        text = readLines(gzfile(testthat::test_path("fixtures/dplyr.json.gz")))
      )
    } else if (req$params$pkg == "bad") {
      res$send_status(401)
    } else {
      res$send_status(404)
    }
  })

  app
}

repo <- webfakes::local_app_process(repo_app())

cran_app_pkgs <- dcf("
  Package: pkg1
  Version: 1.0.0

  Package: pkg1
  Version: 0.9.0

  Package: pkg1
  Version: 0.8.0

  Package: pkg2
  Version: 1.0.0
  Depends: pkg1

  Package: pkg3
  Version: 1.0.0
  Depends: pkg2

  Package: pkg3
  Version: 0.9.9
")

fake_cran <- webfakes::local_app_process(
  cran_app(cran_app_pkgs),
  opts = webfakes::server_opts(num_threads = 3)
)

fake_bioc <- webfakes::local_app_process(
  bioc_app(options = list(bioc_version = "3.16")),
  opts = webfakes::server_opts(num_threads = 3)
)

setup_fake_apps <- function(.local_envir = parent.frame()) {
  withr::local_options(
    repos = c(CRAN = fake_cran$url()),
    pkg.cran_metadata_url = fake_cran$url(),
    .local_envir = .local_envir
  )
  withr::local_envvar(
    R_PKG_CRAN_METADATA_URL = fake_cran$url(),
    R_BIOC_CONFIG_URL = paste0(fake_bioc$url(), "/config.yaml"),
    R_BIOC_VERSION = NA_character_,
    R_BIOC_MIRROR = fake_bioc$url(),
    .local_envir = .local_envir
  )
}
