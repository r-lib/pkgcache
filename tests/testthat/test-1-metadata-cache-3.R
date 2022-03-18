
test_that("concurrency in update", {
  skip_if_offline()
  skip_on_cran()

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

  res <- synchronise(do())
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
    data_frame(name = "CRAN", url = "good", type = "cran",
               r_version = "*", bioc_version = NA_character_)
  )

  ## BioC, all new
  res <- cmc__get_repos(repos, TRUE, "good", r_version = "3.5")
  expect_equal(
    res$name,
    c("CRAN", "BioCsoft", "BioCann", "BioCexp", "BioCworkflows"))
  expect_equal(res$url[1], "good")
  expect_equal(res$type, c("cran", "bioc", "bioc", "bioc", "bioc"))
  expect_equal(
    res$bioc_version,
    c(NA_character_, "3.8", "3.8", "3.8", "3.8"))

  ## BioC, some are custom
  repos <- c(CRAN = "bad", BioCsoft = "ok")
  res <- cmc__get_repos(repos, TRUE, "good", r_version = "3.5")
  expect_equal(
    res$name,
    c("CRAN", "BioCsoft", "BioCsoft", "BioCann", "BioCexp", "BioCworkflows"))
  expect_equal(res$url[1], "good")
  expect_equal(res$url[2], "ok")
  expect_equal(res$type, c("cran", "cranlike", "bioc", "bioc", "bioc", "bioc"))
  expect_equal(
    res$bioc_version,
    c(NA_character_, NA_character_, "3.8", "3.8", "3.8", "3.8"))
})

test_that("cleanup", {
  mockery::stub(cmc_cleanup, "interactive", FALSE)
  expect_error(cmc_cleanup(NULL, NULL, FALSE), "Not cleaning up cache")
})

test_that("cleanup", {
  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(pri, rep, "source",  bioc = FALSE)

  mockery::stub(cmc_cleanup, "interactive", TRUE)
  mockery::stub(cmc_cleanup, "readline", "")
  expect_error(cmc_cleanup(cmc, get_private(cmc), FALSE), "Aborted")
})

test_that("memory cache", {

  skip_if_offline()
  skip_on_cran()

  pri <- test_temp_dir()
  rep <- test_temp_dir()
  cmc <- cranlike_metadata_cache$new(pri, rep, "source", bioc = FALSE)
  data <- cmc$list()

  rep2 <- test_temp_dir()
  cmc2 <- cranlike_metadata_cache$new(pri, rep2, "source", bioc = FALSE)
  week <- as.difftime(7, units = "days")
  data2 <- get_private(cmc2)$get_memory_cache(week)
  expect_identical(data, data2$pkgs)

  rep3 <- test_temp_dir()
  cmc3 <- cranlike_metadata_cache$new(pri, rep3, "source", bioc = FALSE)
  instance <- as.difftime(1/100000, units = "secs")
  expect_error(data3 <- get_private(cmc3)$get_memory_cache(instance),
               "Memory cache outdated")
})

test_that("update_memory_cache", {
  pri <- test_temp_dir()
  rep <- test_temp_dir()

  cmc <- cranlike_metadata_cache$new(pri, rep, c("macos", "source"),
                                     bioc = FALSE)

  mockery::stub(cmc__copy_to_replica, "lock", function(...) NULL)
  expect_error(
    cmc__copy_to_replica(cmc, get_private(cmc), TRUE, TRUE, TRUE),
    "Cannot acquire lock to copy primary cache")
})

test_that("summary", {
  pri <- test_temp_dir()
  rep <- test_temp_dir()

  cmc <- cranlike_metadata_cache$new(pri, rep, c("macos", "source"),
                                     bioc = FALSE)
  sum <- cmc$summary()
  expect_equal(
    names(sum),
    c("cachepath", "lockfile", "current_rds", "raw_files", "rds_files",
      "size")
  )
})
