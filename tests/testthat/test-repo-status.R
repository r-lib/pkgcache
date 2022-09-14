
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

test_that("repo_status", {
  withr::local_options(
    repos = c(CRAN = cran$url()),
    pkg.cran_metadata_url = cran$url(),
    width = 1000
  )

  configs <- list(
    list("source",                               "4.2.1"),
    list(c("aarch64-apple-darwin20", "source"),  "4.2.1"),
    list(c("aarch64-apple-darwin20", "source"),  c("4.1.2", "4.2.1")),
    list(c("x86_64-apple-darwin17.0", "source"), "4.2.1"),
    list(c("windows", "source"),                 "4.2.1"),
    list(c("windows", "source"),                 "4.0.5"),
    list(c("windows", "source"),                 "3.6.3"),
    list(c("windows", "source"),                 "3.5.3")
  )

  for (cf in configs) {
    expect_snapshot({
      print(cf)
      stat <- repo_status(bioc = FALSE, platforms = cf[[1]], r_version = cf[[2]])
      stat$ping[stat$ok] <- 0.1
      stat
      summary(stat)
    }, transform = fix_port_number)
  }

})

cli::test_that_cli(config = "fancy", "repo_status unicode output", {
  withr::local_options(
    repos = c(CRAN = cran$url()),
    pkg.cran_metadata_url = cran$url(),
    width = 1000
  )

  expect_snapshot({
    stat <- repo_status(bioc = FALSE, platforms = "source")
    stat$ping[stat$ok] <- 0.1
    summary(stat)
  }, transform = fix_port_number)
})

test_that("convert repo_status summary to data frame", {
  withr::local_options(
    repos = c(CRAN = cran$url()),
    pkg.cran_metadata_url = cran$url(),
    width = 1000
  )

  expect_snapshot({
    stat <- repo_status(bioc = FALSE, platforms = "source")
    stat$ping[stat$ok] <- 0.1
    summary(stat)[]
  }, transform = fix_port_number)
})
