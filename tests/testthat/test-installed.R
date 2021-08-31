
test_that("pkgcache_read_raw", {
  # cannot open file
  expect_match(
    .Call(pkgcache_read_raw, tempfile())[[1]],
    "Cannot open file"
  )
})

test_that("parse_description", {
  d <- parse_description(get_fixture("description/pkgcache/DESCRIPTION"))
  expect_equal(d[["RoxygenNote"]], "7.1.1.9001")

  # cannot open file
  expect_error(
    parse_description(tempfile()),
    "Cannot open file"
  )

  # empty file
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  file.create(tmp)
  expect_equal(
    parse_description(tmp),
    structure(character(), names = character())
  )

  # invalid files
  d <- ":notgood\n"
  expect_error(
    .Call(pkgcache_parse_description_raw, charToRaw(d)),
    "must start with an alphanumeric"
  )
  d <- "foobar\n"
  expect_error(
    .Call(pkgcache_parse_description_raw, charToRaw(d)),
    "Line 1 invalid in DESCRIPTION: must be of form `key: value`",
    fixed = TRUE
  )
  d <- "foobar"
  expect_error(
    .Call(pkgcache_parse_description_raw, charToRaw(d)),
    "DESCRIPTION file ended while parsing a key"
  )
  d <- "foo: bar\n:nokey\n"
  expect_error(
    .Call(pkgcache_parse_description_raw, charToRaw(d)),
    "Line 2 invalid in DESCRIPTION: must be of form `key: value`",
    fixed = TRUE
  )
})

test_that("parse_packages", {
  testthat::local_edition(3)
  testthat::local_reproducible_output(width = 60)
  pkgs <- parse_packages(get_fixture("packages/PACKAGES"))
  expect_snapshot(colnames(pkgs))
  expect_snapshot(pkgs$Path)
})

test_that("parse_packages, RDS", {
  testthat::local_edition(3)
  testthat::local_reproducible_output(width = 60)
  pkgs <- parse_packages(get_fixture("packages/PACKAGES.rds"))
  expect_snapshot(colnames(pkgs))
  expect_snapshot(pkgs$Path)
})

test_that("parse_packages, compressed", {
  p0      <- parse_packages(get_fixture("packages/PACKAGES"))
  p_gz    <- parse_packages(get_fixture("packages/PACKAGES.gz"))
  p_bz2   <- parse_packages(get_fixture("packages/PACKAGES.bz2"))
  p_bzip2 <- parse_packages(get_fixture("packages/PACKAGES.bzip2"))
  p_xz    <- parse_packages(get_fixture("packages/PACKAGES.xz"))
  expect_equal(p0, p_gz)
  expect_equal(p0, p_bz2)
  expect_equal(p0, p_bzip2)
  expect_equal(p0, p_xz)
})

test_that("parse_packages, errors", {
  expect_error(
    parse_packages(tempfile()),
    "Cannot open file"
  )

  p <- "Package: foo\n\n \n"
  expect_error(
    .Call(pkgcache_parse_packages_raw, charToRaw(p)),
    "Invalid PACKAGES file in line 3: expected key"
  )

  p <- "Package: foo\nVersion is not good\nAnother: x\n"
  expect_error(
    .Call(pkgcache_parse_packages_raw, charToRaw(p)),
    "Invalid line 2 in PACKAGES file: must contain `:`",
    fixed = TRUE
  )

  p <- "Package: foo\nimcoplete_key"
  expect_error(
    .Call(pkgcache_parse_packages_raw, charToRaw(p)),
    "PACKAGES file ended while parsing a key",
    fixed = TRUE
  )
})

test_that("somewhat weird packages files", {
  testthat::local_edition(3)
  testthat::local_reproducible_output(width = 60)
  pkgs <- parse_packages(get_fixture("packages/P1"))
  expect_snapshot(colnames(pkgs))
  expect_snapshot(pkgs$Package)
})

test_that("lib_status", {
  testthat::local_edition(3)
  testthat::local_reproducible_output(width = 60)
  pkgs <- lib_status(get_fixture("lib"))
  expect_snapshot(pkgs$Package)
  expect_true("LibPath" %in% names(pkgs))
})

test_that("lib_status, multiple libs", {
  testthat::local_edition(3)
  testthat::local_reproducible_output(width = 60)
  pkgs <- lib_status(get_fixture(c("lib", "lib2")))
  expect_snapshot(pkgs$Package)
  expect_true("LibPath" %in% names(pkgs))
})

test_that("lib_status, errors", {
  testthat::local_edition(3)
  testthat::local_reproducible_output(width = 60)
  cond <- NULL
  pkgs <- withCallingHandlers(
    lib_status(get_fixture(c("lib", "lib2", "lib3"))),
    "pkgcache_broken_package" = function(cnd) {
      cond <<- cnd
      invokeRestart("muffleWarning")
    }
  )
  expect_snapshot(pkgs$Package)
  expect_true("pkgcache_broken_package" %in% class(cond))
  expect_match(cond$errors$file, "lib3[\\\\/]foo[\\\\/]DESCRIPTION$")
  expect_match(cond$errors$error, "ended while parsing a key")
})

test_that("lib_status, more errors", {
  out <- .Call(pkgcache_parse_descriptions, tempfile())
  expect_true(out[[3]])
  expect_match(out[[2]], "Cannot open file")

  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  cat(":not a good start\n", file = tmp)
  out <- .Call(pkgcache_parse_descriptions, tmp)
  expect_true(out[[3]])
  expect_match(out[[2]], "must start with an alphanumeric character")

  cat("Package: foo\nOops again\nfoo:bar\n", file = tmp)
  out <- .Call(pkgcache_parse_descriptions, tmp)
  expect_true(out[[3]])
  expect_match(out[[2]], "Line 2 is invalid")

})
