if (Sys.getenv("R_COVR") == "true") {
  return()
}

test_that("concurrency in update", {
  setup_fake_apps()

  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(pri, rep, "source", bioc = FALSE)

  ## TODO: somehow check that there are no parallel downloads
  do <- function() {
    dx1 <- cmc$async_update()
    dx2 <- cmc$async_update()
    dx3 <- cmc$async_update()
    when_all(dx1, dx2, dx3)
  }

  res <- suppressMessages(synchronise(do()))
  check_packages_data(res[[1]])
  check_packages_data(res[[2]])
  check_packages_data(res[[3]])

  expect_null(get_private(cmc)$update_deferred)
})

test_that("cmc__get_repos", {
  repos <- c(CRAN = "bad")

  ## No bioc, CRAN is replaced
  expect_equal(
    cmc__get_repos(repos, FALSE, cran_mirror = "good", r_version = "3.5"),
    data_frame(
      name = "CRAN",
      url = "good",
      type = "cran",
      r_version = "*",
      bioc_version = NA_character_
    )
  )

  ## BioC, all new
  res <- cmc__get_repos(repos, TRUE, "good", r_version = "3.5")
  expect_equal(
    res$name,
    c("CRAN", "BioCsoft", "BioCann", "BioCexp", "BioCworkflows")
  )
  expect_equal(res$url[1], "good")
  expect_equal(res$type, c("cran", "bioc", "bioc", "bioc", "bioc"))
  expect_equal(
    res$bioc_version,
    c(NA_character_, "3.8", "3.8", "3.8", "3.8")
  )

  ## BioC, some are custom
  repos <- c(CRAN = "bad", BioCsoft = "ok")
  res <- cmc__get_repos(repos, TRUE, "good", r_version = "3.5")
  expect_equal(
    res$name,
    c("CRAN", "BioCsoft", "BioCsoft", "BioCann", "BioCexp", "BioCworkflows")
  )
  expect_equal(res$url[1], "good")
  expect_equal(res$url[2], "ok")
  expect_equal(res$type, c("cran", "cranlike", "bioc", "bioc", "bioc", "bioc"))
  expect_equal(
    res$bioc_version,
    c(NA_character_, NA_character_, "3.8", "3.8", "3.8", "3.8")
  )
})

test_that("cleanup", {
  fake(cmc_cleanup, "interactive", FALSE)
  expect_snapshot(error = TRUE, cmc_cleanup(NULL, NULL, FALSE))
})

test_that("cleanup", {
  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(pri, rep, "source", bioc = FALSE)

  fake(cmc_cleanup, "interactive", TRUE)
  fake(cmc_cleanup, "readline", "")
  expect_snapshot(error = TRUE, cmc_cleanup(cmc, get_private(cmc), FALSE))
})

test_that("memory cache", {
  setup_fake_apps()

  pri <- test_temp_dir()
  rep <- test_temp_dir()
  cmc <- cranlike_metadata_cache$new(pri, rep, "source", bioc = FALSE)
  data <- suppressMessages(cmc$list())

  rep2 <- test_temp_dir()
  cmc2 <- cranlike_metadata_cache$new(pri, rep2, "source", bioc = FALSE)
  week <- as.difftime(7, units = "days")
  data2 <- get_private(cmc2)$get_memory_cache(week)
  expect_identical(data, data2$pkgs)

  rep3 <- test_temp_dir()
  cmc3 <- cranlike_metadata_cache$new(pri, rep3, "source", bioc = FALSE)
  instance <- as.difftime(1 / 100000, units = "secs")
  expect_snapshot(
    error = TRUE,
    data3 <- get_private(cmc3)$get_memory_cache(instance)
  )
})

test_that("update_memory_cache", {
  pri <- test_temp_dir()
  rep <- test_temp_dir()

  cmc <- cranlike_metadata_cache$new(
    pri,
    rep,
    c("macos", "source"),
    bioc = FALSE
  )

  fake(cmc__copy_to_replica, "filelock::lock", function(...) NULL)
  expect_snapshot(
    error = TRUE,
    cmc__copy_to_replica(cmc, get_private(cmc), TRUE, TRUE, TRUE)
  )
})

test_that("summary", {
  pri <- test_temp_dir()
  rep <- test_temp_dir()

  cmc <- cranlike_metadata_cache$new(
    pri,
    rep,
    c("macos", "source"),
    bioc = FALSE
  )
  sum <- cmc$summary()
  expect_equal(
    names(sum),
    c("cachepath", "lockfile", "current_rds", "raw_files", "rds_files", "size")
  )
})

test_that("corrupt metadata", {
  # The nullfile() trick does not work on Windows, we need a different
  # way to corrupt the files there
  skip_on_os("windows")
  setup_fake_apps()

  pri <- nullfile()
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(pri, rep, "source", bioc = FALSE)

  expect_snapshot(error = TRUE, suppressMessages(cmc$list()))
})

test_that("missing packages note", {
  fake_cran2 <- webfakes::local_app_process(
    cran_app(cran_app_pkgs),
    opts = webfakes::server_opts(num_threads = 3)
  )

  withr::local_options(
    repos = c(CRAN = fake_cran$url(), FOO = fake_cran2$url())
  )

  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(
    pri,
    rep,
    c("source", "windows"),
    bioc = FALSE
  )
  msg <- ""
  withCallingHandlers(
    invisible(cmc$update()),
    message = function(m) {
      msg <<- paste0(msg, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  expect_match(
    msg,
    "packages are missing from CRAN and 127.0.0.1:",
    fixed = TRUE
  )
})
