
context("metadata cache")

test_that("get_cache_files", {
  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(pri, rep, "source",  bioc = FALSE)

  pri_files <- get_private(cmc)$get_cache_files("primary")
  rep_files <- get_private(cmc)$get_cache_files("replica")

  check <- function(files, root) {
    expect_equal(files$root, root)
    expect_true(all(c("meta", "lock", "rds") %in% names(files)))
    expect_equal(
      fs::path_common(c(files$rds, files$lock, files$rds, root)),
      root)
    expect_true(tibble::is_tibble(files$pkgs))
    expect_equal(
      sort(names(files$pkgs)),
      sort(c("path", "etag", "basedir", "base", "mirror", "url", "fallback_url",
             "platform", "type", "bioc_version", "meta_path", "meta_etag",
             "meta_url")))
    expect_equal(
      fs::path_common(c(files$pkgs$path, files$pkgs$etag, root)),
      root)
  }

  check(pri_files, pri)
  check(rep_files, rep)
})

test_that("get_current_data", {
  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(pri, rep, "source",  bioc = FALSE)

  set_private(cmc, "data") <- "DATA"
  set_private(cmc, "data_time") <- Sys.time()
  expect_equal(get_private(cmc)$get_current_data(oneday()), "DATA")

  set_private(cmc,  "data_time") <- Sys.time() - 2 * oneday()
  expect_error(
    get_private(cmc)$get_current_data(oneday()),
    "Loaded data outdated")

  set_private(cmc, "data_time") <- NULL
  expect_error(
    get_private(cmc)$get_current_data(oneday()),
    "Loaded data outdated")

  set_private(cmc, "data") <- NULL
  expect_error(get_private(cmc)$get_current_data(oneday()), "No data loaded")
})

test_that("load_replica_rds", {
  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(pri, rep, "source",  bioc = FALSE)

  expect_error(
    get_private(cmc)$load_replica_rds(oneday()),
    "No replica RDS file in cache"
  )

  rep_files <- get_private(cmc)$get_cache_files("replica")
  mkdirp(dirname(rep_files$rds))
  saveRDS("This is it", rep_files$rds)
  file_set_time(rep_files$rds, Sys.time() - 2 * oneday())
  expect_error(
    get_private(cmc)$load_replica_rds(oneday()),
    "Replica RDS cache file outdated"
  )

  file_set_time(rep_files$rds, Sys.time() - 1/2  * oneday())
  expect_equal(
    get_private(cmc)$load_replica_rds(oneday()),
    "This is it")
  expect_equal(get_private(cmc)$data, "This is it")
  expect_true(Sys.time() - get_private(cmc)$data_time < oneday())
})

test_that("load_primary_rds", {
  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(pri, rep, "source",  bioc = FALSE)

  expect_error(
    get_private(cmc)$load_primary_rds(oneday()),
    "No primary RDS file in cache"
  )

  pri_files <- get_private(cmc)$get_cache_files("primary")
  mkdirp(dirname(pri_files$rds))
  saveRDS("This is it", pri_files$rds)
  file_set_time(pri_files$rds, Sys.time() - 2 * oneday())
  expect_error(
    get_private(cmc)$load_primary_rds(oneday()),
    "Primary RDS cache file outdated"
  )

  file_set_time(pri_files$rds, Sys.time() - 1/2  * oneday())
  for (f in pri_files$pkgs$path) { mkdirp(dirname(f)); cat("x", file = f) }
  file_set_time(pri_files$pkgs$path, Sys.time() - 2  * oneday())
  expect_equal(
    get_private(cmc)$load_primary_rds(oneday()),
    "This is it")
  expect_equal(get_private(cmc)$data, "This is it")
  expect_true(Sys.time() - get_private(cmc)$data_time < oneday())

  ## Replica was also updated
  expect_equal(
    get_private(cmc)$load_replica_rds(oneday()),
    "This is it")
})

test_that("locking failures", {
  pri <- test_temp_dir()
  rep <- test_temp_dir()

  cmc <- cranlike_metadata_cache$new(pri, rep, "source", bioc = FALSE)

  mockery::stub(cmc__load_primary_rds, "lock", function(...) NULL)
  expect_error(
    cmc__load_primary_rds(cmc, get_private(cmc), oneday()),
    "Cannot acquire lock to copy RDS")

  mockery::stub(cmc__load_primary_pkgs, "lock", function(...) NULL)
  expect_error(
    cmc__load_primary_pkgs(cmc, get_private(cmc), oneday()),
    "Cannot acquire lock to copy PACKAGES")
})

test_that("load_primary_rds 3", {
  pri <- test_temp_dir()
  rep <- test_temp_dir()

  cmc <- cranlike_metadata_cache$new(pri, rep, "source", bioc = FALSE)

  pri_files <- get_private(cmc)$get_cache_files("primary")
  touch(pri_files$rds)
  expect_error(
    cmc__load_primary_rds(cmc, get_private(cmc), oneday()),
    "Primary PACKAGES missing")
})

test_that("load_primary_pkgs", {

  withr::local_options(list(repos = NULL))

  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(pri, rep, c("macos", "source"),
                                     bioc = FALSE)

  expect_error(
    get_private(cmc)$load_primary_pkgs(oneday()),
    "Some primary PACKAGES files don't exist")

  pri_files <- get_private(cmc)$get_cache_files("primary")
  mkdirp(dirname(pri_files$pkgs$path))
  fs::file_copy(get_fixture("PACKAGES-mac.gz"), pri_files$pkgs$path[1])
  expect_error(
    synchronise(get_private(cmc)$load_primary_pkgs(oneday())),
    "Some primary PACKAGES files don't exist")

  for (i in utils::tail(seq_len(nrow(pri_files$pkgs)), -1)) {
    fs::file_copy(get_fixture("PACKAGES-src.gz"), pri_files$pkgs$path[i])
  }
  file_set_time(pri_files$pkgs$path, Sys.time() - 2 * oneday())
  expect_error(
    synchronise(get_private(cmc)$load_primary_pkgs(oneday())),
    "Some primary PACKAGES files are outdated")

  file_set_time(pri_files$pkgs$path, Sys.time() - 1/2 * oneday())
  res <- synchronise(get_private(cmc)$load_primary_pkgs(oneday()))
  check_packages_data(res)

  ## RDS was updated as well
  rep_files <- get_private(cmc)$get_cache_files("replica")
  expect_true(file.exists(rep_files$rds))
  expect_true(Sys.time() - file_get_time(rep_files$rds) < oneday())

  ## Primary RDS was updated as well
  expect_true(file.exists(pri_files$rds))
  expect_true(Sys.time() - file_get_time(pri_files$rds) < oneminute())
})

test_that("update_replica_pkgs", {

  skip_if_offline()

  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(pri, rep, "source", bioc = FALSE)

  synchronise(get_private(cmc)$update_replica_pkgs())
  rep_files <- get_private(cmc)$get_cache_files("replica")
  expect_true(all(file.exists(rep_files$pkgs$path)))
  expect_true(all(file.exists(rep_files$pkgs$etag)))

  data <- get_private(cmc)$update_replica_rds()
  expect_identical(data, get_private(cmc)$data)
  check_packages_data(data)
})

test_that("update_replica_rds", {
  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(pri, rep, c("macos", "source"),
                                     bioc = FALSE)

  rep_files <- get_private(cmc)$get_cache_files("replica")
  mkdirp(dirname(rep_files$pkgs$path))
  fs::file_copy(get_fixture("PACKAGES-mac.gz"), rep_files$pkgs$path[1])
  for (i in utils::tail(seq_len(nrow(rep_files$pkgs)), -1)) {
    fs::file_copy(get_fixture("PACKAGES-src.gz"), rep_files$pkgs$path[i])
  }

  data <- get_private(cmc)$update_replica_rds()
  expect_identical(get_private(cmc)$data, data)
  expect_true(get_private(cmc)$data_time > Sys.time() - oneminute())
  check_packages_data(data)
})

test_that("update_primary", {
  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(pri, rep, c("macos", "source"),
                                     bioc = FALSE)

  pri_files <- get_private(cmc)$get_cache_files("primary")
  rep_files <- get_private(cmc)$get_cache_files("replica")

  mkdirp(dirname(rep_files$rds))
  saveRDS("RDS", rep_files$rds)
  get_private(cmc)$update_primary(rds = TRUE, packages = FALSE)
  expect_true(file.exists(pri_files$rds))
  expect_equal(readRDS(pri_files$rds), "RDS")

  lapply_rows(rep_files$pkgs, function(pkg) {
    mkdirp(dirname(pkg$path))
    cat(basename(pkg$path), "\n", sep = "", file = pkg$path)
    mkdirp(dirname(pkg$etag))
    cat(pkg$url, "\n", sep = "", file = pkg$etag)
  })
  get_private(cmc)$update_primary(rds = FALSE, packages = TRUE)
  expect_true(all(file.exists(pri_files$pkgs$path)))
  expect_true(all(file.exists(pri_files$pkgs$etag)))

  lapply_rows(pri_files$pkgs, function(pkg) {
    expect_equal(readLines(pkg$path), basename(pkg$path))
    expect_equal(readLines(pkg$etag), pkg$url)
  })
})

test_that("update_primary 2", {

  expect_null(cmc__update_primary(NULL, NULL, FALSE, FALSE, FALSE))

  pri <- test_temp_dir()
  rep <- test_temp_dir()

  cmc <- cranlike_metadata_cache$new(pri, rep, c("macos", "source"),
                                     bioc = FALSE)

  mockery::stub(cmc__update_primary, "lock", function(...) NULL)
  expect_error(
    cmc__update_primary(cmc, get_private(cmc), TRUE, TRUE, TRUE),
    "Cannot acquire lock to update primary cache")
})

test_that("update", {

  skip_if_offline()

  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(pri, rep, "source", bioc = FALSE)
  data <- cmc$update()
  check_packages_data(data)

  ## Data is loaded
  expect_identical(get_private(cmc)$data, data)
  expect_true(Sys.time() - get_private(cmc)$data_time < oneminute())

  ## There is a replica RDS
  rep_files <- get_private(cmc)$get_cache_files("replica")
  expect_true(file.exists(rep_files$rds))
  expect_true(Sys.time() - file_get_time(rep_files$rds) < oneminute())

  ## There is a primary RDS
  pri_files <- get_private(cmc)$get_cache_files("primary")
  expect_true(file.exists(pri_files$rds))
  expect_true(Sys.time() - file_get_time(pri_files$rds) < oneminute())

  ## There are replicate PACKAGES, with Etag files
  expect_true(all(file.exists(rep_files$pkgs$path)))
  expect_true(all(file.exists(rep_files$pkgs$etag)))

  ## There are primary PACKAGES, with Etag files
  expect_true(all(file.exists(pri_files$pkgs$path)))
  expect_true(all(file.exists(pri_files$pkgs$etag)))

  ## List
  expect_equal(as.list(data$pkgs), as.list(cmc$list()))
  lst <- cmc$list(c("igraph", "MASS"))
  expect_equal(sort(c("igraph", "MASS")), sort(unique(lst$package)))

  ## Revdeps
  rdeps <- cmc$revdeps("MASS")
  expect_true("abc" %in% rdeps$package)
  expect_true("abd" %in% rdeps$package)

  rdeps <- cmc$revdeps("MASS", recursive = FALSE)
  expect_true("abc" %in% rdeps$package)
  expect_false("abd" %in% rdeps$package)
})

test_that("check_update", {

  skip_if_offline()

  withr::local_options(
    list(repos = c(CRAN = "https://cloud.r-project.org"))
  )

  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(pri, rep, "source", bioc = FALSE)
  data <- cmc$check_update()
  check_packages_data(data)

  ## Data is loaded
  expect_identical(get_private(cmc)$data, data)
  expect_true(Sys.time() - get_private(cmc)$data_time < oneminute())

  ## There is a replica RDS
  rep_files <- get_private(cmc)$get_cache_files("replica")
  expect_true(file.exists(rep_files$rds))
  expect_true(Sys.time() - file_get_time(rep_files$rds) < oneminute())

  ## There is a primary RDS
  pri_files <- get_private(cmc)$get_cache_files("primary")
  expect_true(file.exists(pri_files$rds))
  expect_true(Sys.time() - file_get_time(pri_files$rds) < oneminute())

  ## There are replicate PACKAGES, with Etag files
  expect_true(all(file.exists(rep_files$pkgs$path)))
  expect_true(all(file.exists(rep_files$pkgs$etag)))

  ## There are primary PACKAGES, with Etag files
  expect_true(all(file.exists(pri_files$pkgs$path)))
  expect_true(all(file.exists(pri_files$pkgs$etag)))

  ## We don't download it again, if the Etag files are current
  cat("foobar\n", file = rep_files$pkgs$path[1])
  cat("foobar2\n", file = rep_files$rds)
  cat("foobar\n", file = pri_files$pkgs$path[1])
  cat("foobar2\n", file = pri_files$rds)
  data2 <- cmc$check_update()
  expect_identical(data, data2)
  expect_equal(read_lines(rep_files$pkgs$path[1]), "foobar")

  ## Cleanup
  cmc$cleanup(force = TRUE)
  expect_false(file.exists(pri_files$rds))
  expect_false(any(file.exists(pri_files$pkgs$path)))
  expect_false(file.exists(rep_files$rds))
  expect_false(any(file.exists(rep_files$pkgs$path)))
})

test_that("deps will auto-update as needed", {

  skip_if_offline()

  withr::local_options(list(repos = NULL))

  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(pri, rep, "source", bioc = FALSE)

  pri_files <- get_private(cmc)$get_cache_files("primary")
  mkdirp(dirname(pri_files$pkgs$path))
  fs::file_copy(get_fixture("PACKAGES-src.gz"), pri_files$pkgs$path)

  ## This will update the RDS files, and also load the data
  cmc$deps("A3", recursive = FALSE)

  ## Data is loaded
  expect_false(is.null(get_private(cmc)$data))
  expect_true(Sys.time() - get_private(cmc)$data_time < oneminute())

  ## There is a replica RDS
  rep_files <- get_private(cmc)$get_cache_files("replica")
  expect_true(file.exists(rep_files$rds))
  expect_true(Sys.time() - file_get_time(rep_files$rds) < oneminute())

  ## There is a primary RDS
  pri_files <- get_private(cmc)$get_cache_files("primary")
  expect_true(file.exists(pri_files$rds))
  expect_true(Sys.time() - file_get_time(pri_files$rds) < oneminute())

  ## There are replicate PACKAGES, no Etag files, since no downloads...
  expect_true(all(file.exists(rep_files$pkgs$path)))

  ## There are primary PACKAGES, no Etag files, since no downloads...
  expect_true(all(file.exists(pri_files$pkgs$path)))
})

test_that("deps, extract_deps", {

  skip_if_offline()

  withr::local_options(list(repos = NULL))

  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(pri, rep, "source", bioc = FALSE,
                                     cran_mirror = "mirror")

  pri_files <- get_private(cmc)$get_cache_files("primary")
  mkdirp(dirname(pri_files$pkgs$path))
  fs::file_copy(get_fixture("PACKAGES-src.gz"), pri_files$pkgs$path)
  file_set_time(pri_files$pkgs$path, Sys.time() - 1/2 * oneday())

  pkgs <- read_packages_file(
    get_fixture("PACKAGES-src.gz"),
    mirror = "mirror", repodir = "src/contrib", platform = "source",
    rversion = get_minor_r_version(current_r_version()), type = "cran")

  deps <- cmc$deps("abc", FALSE, FALSE)
  expect_identical(deps$package, "abc")
  expect_identical(attr(deps, "base"), character())
  expect_identical(attr(deps, "unknown"), character())
  deps2 <- extract_deps(pkgs, "abc", FALSE, FALSE)
  expect_identical(deps, deps2)

  deps <- extract_deps(pkgs, "abc", TRUE, FALSE)
  expect_identical(deps$package, c("abc", "abc.data", "MASS", "nnet"))
  expect_identical(attr(deps, "base"), character())
  expect_identical(attr(deps, "unknown"), c("quantreg", "locfit"))
  deps2 <- extract_deps(pkgs, "abc", TRUE, FALSE)
  expect_identical(deps, deps2)

  deps <- extract_deps(pkgs, "abc", TRUE, TRUE)
  expect_identical(deps$package, c("abc", "abc.data", "MASS", "nnet"))
  expect_identical(
    sort(attr(deps, "base")),
    sort(c("grDevices", "graphics", "stats", "utils", "methods")))
  expect_identical(attr(deps, "unknown"), c("quantreg", "locfit"))
  deps2 <- extract_deps(pkgs, "abc", TRUE, TRUE)
  expect_identical(deps, deps2)

  deps <- extract_deps(pkgs, "nnet", c("Depends", "Suggests"), FALSE)
  expect_identical(deps$package, c("MASS", "nnet"))
  expect_identical(attr(deps, "base"), c("stats", "utils"))
  expect_identical(attr(deps, "unknown"), character())
  deps2 <- extract_deps(pkgs, "nnet", c("Depends", "Suggests"), FALSE)
  expect_identical(deps, deps2)
})

test_that("concurrency in update", {
  skip_if_offline()

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
    tibble(name = "CRAN", url = "good", type = "cran",
           bioc_version = NA_character_)
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
    c("CRAN", "BioCsoft", "BioCann", "BioCexp", "BioCworkflows"))
  expect_equal(res$url[1], "good")
  expect_equal(res$url[2], "ok")
  expect_equal(res$type, c("cran", "bioc", "bioc", "bioc", "bioc"))
  expect_equal(
    res$bioc_version,
    c(NA_character_, "3.8", "3.8", "3.8", "3.8"))
})

test_that("download failures", {

  skip_if_offline()

  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(
    pri, rep, "source", bioc = FALSE,
    cran_mirror = "http://127.0.0.1:23424/")

  expect_error(
    expect_message(cmc$update(), "Metadata download failed"))
  expect_error(cmc$get_update())
  expect_error(cmc$list())
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
