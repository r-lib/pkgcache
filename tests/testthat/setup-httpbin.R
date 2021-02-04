
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
