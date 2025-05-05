if (Sys.getenv("R_COVR") == "true") {
  return()
}

test_that("get_cache_files", {
  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(pri, rep, "source", bioc = FALSE)

  pri_files <- get_private(cmc)$get_cache_files("primary")
  rep_files <- get_private(cmc)$get_cache_files("replica")

  check <- function(files, root) {
    expect_equal(files$root, root)
    expect_true(all(c("meta", "lock", "rds") %in% names(files)))
    expect_equal(
      fs::path_common(c(files$rds, files$lock, files$rds, root)),
      root
    )
    expect_true(inherits(files$pkgs, "tbl"))
    expect_equal(
      sort(names(files$pkgs)),
      sort(c(
        "path",
        "etag",
        "basedir",
        "base",
        "mirror",
        "url",
        "fallback_url",
        "platform",
        "r_version",
        "type",
        "bioc_version",
        "meta_path",
        "meta_etag",
        "meta_url",
        "bin_path",
        "bin_etag",
        "bin_url"
      ))
    )
    expect_equal(
      fs::path_common(c(files$pkgs$path, files$pkgs$etag, root)),
      root
    )
  }

  check(pri_files, pri)
  check(rep_files, rep)
})

test_that("get_current_data", {
  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(pri, rep, "source", bioc = FALSE)

  set_private(cmc, "data") <- "DATA"
  set_private(cmc, "data_time") <- Sys.time()
  expect_equal(get_private(cmc)$get_current_data(oneday()), "DATA")

  set_private(cmc, "data_time") <- Sys.time() - 2 * oneday()
  expect_snapshot(
    error = TRUE,
    get_private(cmc)$get_current_data(oneday())
  )

  set_private(cmc, "data_time") <- NULL
  expect_snapshot(
    error = TRUE,
    get_private(cmc)$get_current_data(oneday())
  )

  set_private(cmc, "data") <- NULL
  expect_snapshot(
    error = TRUE,
    get_private(cmc)$get_current_data(oneday())
  )
})

test_that("load_replica_rds", {
  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(pri, rep, "source", bioc = FALSE)

  expect_snapshot(
    error = TRUE,
    get_private(cmc)$load_replica_rds(oneday())
  )

  rep_files <- get_private(cmc)$get_cache_files("replica")
  mkdirp(dirname(rep_files$rds))
  save_rds("This is it", rep_files$rds)
  file_set_time(rep_files$rds, Sys.time() - 2 * oneday())
  expect_snapshot(
    error = TRUE,
    get_private(cmc)$load_replica_rds(oneday())
  )

  file_set_time(rep_files$rds, Sys.time() - 1 / 2 * oneday())
  expect_equal(
    suppressMessages(get_private(cmc)$load_replica_rds(oneday())),
    "This is it"
  )
  expect_equal(get_private(cmc)$data, "This is it")
  expect_true(Sys.time() - get_private(cmc)$data_time < oneday())
})

test_that("load_primary_rds", {
  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(pri, rep, "source", bioc = FALSE)

  expect_snapshot(
    error = TRUE,
    get_private(cmc)$load_primary_rds(oneday())
  )

  pri_files <- get_private(cmc)$get_cache_files("primary")
  mkdirp(dirname(pri_files$rds))
  save_rds("This is it", pri_files$rds)
  file_set_time(pri_files$rds, Sys.time() - 2 * oneday())
  expect_snapshot(
    error = TRUE,
    get_private(cmc)$load_primary_rds(oneday())
  )

  file_set_time(pri_files$rds, Sys.time() - 1 / 2 * oneday())
  for (f in pri_files$pkgs$path) {
    mkdirp(dirname(f))
    cat("x", file = f)
  }
  file_set_time(pri_files$pkgs$path, Sys.time() - 2 * oneday())
  expect_equal(
    suppressMessages(get_private(cmc)$load_primary_rds(oneday())),
    "This is it"
  )
  expect_equal(get_private(cmc)$data, "This is it")
  expect_true(Sys.time() - get_private(cmc)$data_time < oneday())

  ## Replica was also updated
  expect_equal(
    suppressMessages(get_private(cmc)$load_replica_rds(oneday())),
    "This is it"
  )
})

test_that("locking failures", {
  pri <- test_temp_dir()
  rep <- test_temp_dir()

  cmc <- cranlike_metadata_cache$new(pri, rep, "source", bioc = FALSE)

  fake(cmc__load_primary_rds, "filelock::lock", function(...) NULL)
  expect_snapshot(
    error = TRUE,
    cmc__load_primary_rds(cmc, get_private(cmc), oneday())
  )

  fake(cmc__load_primary_pkgs, "filelock::lock", function(...) NULL)
  expect_snapshot(
    error = TRUE,
    cmc__load_primary_pkgs(cmc, get_private(cmc), oneday())
  )
})

test_that("load_primary_rds 3", {
  pri <- test_temp_dir()
  rep <- test_temp_dir()

  cmc <- cranlike_metadata_cache$new(pri, rep, "source", bioc = FALSE)

  pri_files <- get_private(cmc)$get_cache_files("primary")
  touch(pri_files$rds)
  expect_snapshot(
    error = TRUE,
    cmc__load_primary_rds(cmc, get_private(cmc), oneday())
  )
})

test_that("load_primary_pkgs", {
  setup_fake_apps()

  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(
    pri,
    rep,
    c("macos", "source"),
    bioc = FALSE
  )

  expect_snapshot(
    error = TRUE,
    get_private(cmc)$load_primary_pkgs(oneday())
  )

  pri_files <- get_private(cmc)$get_cache_files("primary")
  mkdirp(dirname(pri_files$pkgs$path))
  fs::file_copy(test_path("fixtures/PACKAGES-mac.gz"), pri_files$pkgs$path[1])
  # if this fails, then we need to add a new R version to the list or
  # CRAN macOS platforms in platform.R
  expect_snapshot(
    error = TRUE,
    synchronise(get_private(cmc)$load_primary_pkgs(oneday()))
  )

  for (i in utils::tail(seq_len(nrow(pri_files$pkgs)), -1)) {
    fs::file_copy(test_path("fixtures/PACKAGES-src.gz"), pri_files$pkgs$path[i])
  }
  file_set_time(pri_files$pkgs$path, Sys.time() - 2 * oneday())
  expect_snapshot(
    error = TRUE,
    synchronise(get_private(cmc)$load_primary_pkgs(oneday()))
  )

  file_set_time(pri_files$pkgs$path, Sys.time() - 1 / 2 * oneday())
  res <- suppressMessages(
    synchronise(get_private(cmc)$load_primary_pkgs(oneday()))
  )
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
  setup_fake_apps()

  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(pri, rep, "source", bioc = FALSE)

  suppressMessages(synchronise(get_private(cmc)$update_replica_pkgs()))
  rep_files <- get_private(cmc)$get_cache_files("replica")
  expect_true(all(file.exists(rep_files$pkgs$path)))
  expect_true(all(file.exists(rep_files$pkgs$etag)))

  data <- suppressMessages(get_private(cmc)$update_replica_rds())
  expect_identical(data, get_private(cmc)$data)
  check_packages_data(data)
})

test_that("update_replica_rds", {
  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(
    pri,
    rep,
    c("macos", "source"),
    bioc = FALSE
  )

  rep_files <- get_private(cmc)$get_cache_files("replica")
  mkdirp(dirname(rep_files$pkgs$path))
  fs::file_copy(test_path("fixtures/PACKAGES-mac.gz"), rep_files$pkgs$path[1])
  for (i in utils::tail(seq_len(nrow(rep_files$pkgs)), -1)) {
    fs::file_copy(test_path("fixtures/PACKAGES-src.gz"), rep_files$pkgs$path[i])
  }

  data <- suppressMessages(get_private(cmc)$update_replica_rds())
  expect_identical(get_private(cmc)$data, data)
  expect_true(get_private(cmc)$data_time > Sys.time() - oneminute())
  check_packages_data(data)
})

test_that("update_primary", {
  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(
    pri,
    rep,
    c("macos", "source"),
    bioc = FALSE
  )

  pri_files <- get_private(cmc)$get_cache_files("primary")
  rep_files <- get_private(cmc)$get_cache_files("replica")

  mkdirp(dirname(rep_files$rds))
  save_rds("RDS", rep_files$rds)
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

  cmc <- cranlike_metadata_cache$new(
    pri,
    rep,
    c("macos", "source"),
    bioc = FALSE
  )

  fake(cmc__update_primary, "filelock::lock", function(...) NULL)
  expect_snapshot(
    error = TRUE,
    cmc__update_primary(cmc, get_private(cmc), TRUE, TRUE, TRUE)
  )
})

test_that("update", {
  setup_fake_apps()

  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(pri, rep, "source", bioc = FALSE)
  data <- suppressMessages(cmc$update())
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
  lst <- cmc$list(c("pkg1", "pkg2"))
  expect_equal(sort(c("pkg1", "pkg2")), sort(unique(lst$package)))

  ## Revdeps
  rdeps <- cmc$revdeps("pkg1")
  expect_true("pkg2" %in% rdeps$package)
  expect_true("pkg3" %in% rdeps$package)

  rdeps <- cmc$revdeps("pkg1", recursive = FALSE)
  expect_true("pkg2" %in% rdeps$package)
  expect_false("pkg3" %in% rdeps$package)
})
