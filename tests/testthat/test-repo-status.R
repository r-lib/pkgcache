
test_that("repo_status", {
  setup_fake_apps()
  withr::local_options(
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

test_that("bioc repo status", {
  if (getRversion() < "4.2.0") skip("Need R 4.2.x")

  setup_fake_apps()
  withr::local_options(width = 1000)

  expect_snapshot({
    stat <- repo_status(bioc = TRUE, platforms = "source", r_version = "4.2")
    stat$ping[stat$ok] <- 0.1
    stat
    summary(stat)
  }, transform = fix_port_number)
})

test_that("repo with binary packages", {
  if (getRversion() < "4.2.0" || getRversion() >= "4.3.0") skip("Need R 4.2.x")

  withr::local_options(width = 1000)

  platforms <- c("aarch64-apple-darwin20", "source")
  fake_cran <- webfakes::local_app_process(
    cran_app(cran_app_pkgs, options = list(platforms = platforms, r_version = "4.2")),
    opts = webfakes::server_opts(num_threads = 3)
  )
  withr::local_options(repos = c(CRAN = fake_cran$url()))

  expect_snapshot({
    stat <- repo_status(platforms = platforms, r_version = "4.2", bioc = FALSE)
    stat$ping[stat$ok] <- 0.1
    stat
    summary(stat)
  }, transform = fix_port_number)
})

cli::test_that_cli(config = "fancy", "repo_status unicode output", {
  setup_fake_apps()
  withr::local_options(
    width = 1000
  )

  expect_snapshot({
    stat <- repo_status(bioc = FALSE, platforms = "source")
    stat$ping[stat$ok] <- 0.1
    summary(stat)
  }, transform = fix_port_number)
})

test_that("convert repo_status summary to data frame", {
  setup_fake_apps()
  withr::local_options(
    width = 1000
  )

  expect_snapshot({
    stat <- repo_status(bioc = FALSE, platforms = "source")
    stat$ping[stat$ok] <- 0.1
    summary(stat)[]
  }, transform = fix_port_number)
})
