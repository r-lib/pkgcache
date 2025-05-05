if (Sys.getenv("R_COVR") == "true") {
  return()
}

test_that("check_update", {
  setup_fake_apps()

  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(pri, rep, "source", bioc = FALSE)
  data <- suppressMessages(cmc$check_update())
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
  suppressMessages(cmc$cleanup(force = TRUE))
  expect_false(file.exists(pri_files$rds))
  expect_false(any(file.exists(pri_files$pkgs$path)))
  expect_false(file.exists(rep_files$rds))
  expect_false(any(file.exists(rep_files$pkgs$path)))
})

test_that("deps will auto-update as needed", {
  setup_fake_apps()

  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(pri, rep, "source", bioc = FALSE)

  pri_files <- get_private(cmc)$get_cache_files("primary")
  mkdirp(dirname(pri_files$pkgs$path))
  fs::file_copy(test_path("fixtures/PACKAGES-src.gz"), pri_files$pkgs$path)

  ## This will update the RDS files, and also load the data
  suppressMessages(cmc$deps("pkg3", recursive = FALSE))

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
  setup_fake_apps()

  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(
    pri,
    rep,
    "source",
    bioc = FALSE,
    cran_mirror = "mirror"
  )

  pri_files <- get_private(cmc)$get_cache_files("primary")
  mkdirp(dirname(pri_files$pkgs$path))
  fs::file_copy(test_path("fixtures/PACKAGES-src.gz"), pri_files$pkgs$path)
  file_set_time(pri_files$pkgs$path, Sys.time() - 1 / 2 * oneday())

  pkgs <- read_packages_file(
    test_path("fixtures/PACKAGES-src.gz"),
    mirror = "mirror",
    repodir = "src/contrib",
    platform = "source",
    rversion = "*",
    type = "cran"
  )

  deps <- suppressMessages(cmc$deps("abc", FALSE, FALSE))
  expect_identical(deps$package, "abc")
  expect_identical(attr(deps, "base"), character())
  expect_identical(attr(deps, "unknown"), character())
  deps2 <- extract_deps(pkgs, "abc", FALSE, FALSE)
  expect_identical(deps, deps2)

  deps <- extract_deps(pkgs, "abc", TRUE, FALSE)
  expect_identical(deps$package, c("abc", "abc.data", "MASS", "nnet"))
  expect_identical(attr(deps, "base"), character())
  expect_identical(attr(deps, "unknown"), c("locfit", "quantreg"))
  deps2 <- extract_deps(pkgs, "abc", TRUE, FALSE)
  expect_identical(deps, deps2)

  deps <- extract_deps(pkgs, "abc", TRUE, TRUE)
  expect_identical(deps$package, c("abc", "abc.data", "MASS", "nnet"))
  expect_identical(
    sort(attr(deps, "base")),
    sort(c("grDevices", "graphics", "stats", "utils", "methods"))
  )
  expect_identical(attr(deps, "unknown"), c("locfit", "quantreg"))
  deps2 <- extract_deps(pkgs, "abc", TRUE, TRUE)
  expect_identical(deps, deps2)

  deps <- extract_deps(pkgs, "nnet", c("Depends", "Suggests"), FALSE)
  expect_identical(deps$package, c("MASS", "nnet"))
  expect_identical(attr(deps, "base"), c("stats", "utils"))
  expect_identical(attr(deps, "unknown"), character())
  deps2 <- extract_deps(pkgs, "nnet", c("Depends", "Suggests"), FALSE)
  expect_identical(deps, deps2)
})
