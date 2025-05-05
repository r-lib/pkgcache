test_that("pkgcache_read_raw", {
  # cannot open file
  expect_match(
    .Call(pkgcache_read_raw, tempfile())[[1]],
    "Cannot open file"
  )
})

test_that("parse_description", {
  d <- parse_description(test_path("fixtures/description/pkgcache/DESCRIPTION"))
  expect_equal(d[["RoxygenNote"]], "7.1.1.9001")

  # cannot open file
  expect_snapshot(
    error = TRUE,
    parse_description(tempfile()),
    transform = fix_temp_path,
    variant = get_os_variant()
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
  expect_snapshot(
    error = TRUE,
    .Call(pkgcache_parse_description_raw, charToRaw(d)),
    transform = fix_c_line_number
  )
  d <- "foobar\n"
  expect_snapshot(
    error = TRUE,
    .Call(pkgcache_parse_description_raw, charToRaw(d)),
    transform = fix_c_line_number
  )
  d <- "foobar"
  expect_snapshot(
    error = TRUE,
    .Call(pkgcache_parse_description_raw, charToRaw(d))
  )
  d <- "foo: bar\n:nokey\n"
  expect_snapshot(
    error = TRUE,
    .Call(pkgcache_parse_description_raw, charToRaw(d)),
    transform = fix_c_line_number
  )

  # \r\n line endings
  d <- parse_description(test_path("fixtures/description/cli/DESCRIPTION"))
  expect_equal(d[["Encoding"]], "UTF-8")
})

test_that("parse_description encoding", {
  d <- parse_description(test_path("fixtures/description/pkgcache/DESCRIPTION"))
  expect_equal(Encoding(d[["Authors@R"]]), "UTF-8")

  d2 <- parse_description(test_path(
    "fixtures/description/pkgcachel1/DESCRIPTION"
  ))
  expect_equal(Encoding(d2[["Authors@R"]]), "UTF-8")
})

test_that("parse_packages", {
  testthat::local_edition(3)
  testthat::local_reproducible_output(width = 60)
  pkgs <- parse_packages(test_path("fixtures/packages/PACKAGES"))
  expect_snapshot(colnames(pkgs))
  expect_snapshot(pkgs$Path)
})

test_that("parse_packages, RDS", {
  testthat::local_edition(3)
  testthat::local_reproducible_output(width = 60)
  pkgs <- parse_packages(test_path("fixtures/packages/PACKAGES.rds"))
  expect_snapshot(colnames(pkgs))
  expect_snapshot(pkgs$Path)

  pkgs <- parse_packages(test_path("fixtures/packages/PACKAGES2.rds"))
  expect_snapshot(colnames(pkgs))
  expect_snapshot(pkgs$Path)
})

test_that("parse_packages, compressed", {
  p0 <- parse_packages(test_path("fixtures/packages/PACKAGES"))
  p_gz <- parse_packages(test_path("fixtures/packages/PACKAGES.gz"))
  p_bz2 <- parse_packages(test_path("fixtures/packages/PACKAGES.bz2"))
  p_bzip2 <- parse_packages(test_path("fixtures/packages/PACKAGES.bzip2"))
  p_xz <- parse_packages(test_path("fixtures/packages/PACKAGES.xz"))
  expect_equal(p0, p_gz)
  expect_equal(p0, p_bz2)
  expect_equal(p0, p_bzip2)
  expect_equal(p0, p_xz)
})

test_that("parse_packages, <CR><LF>", {
  p0 <- parse_packages(test_path("fixtures/packages/PACKAGES"))
  p <- parse_packages(test_path("fixtures/packages/PACKAGES2"))
  p_gz <- parse_packages(test_path("fixtures/packages/PACKAGES2.gz"))
  p_bz2 <- parse_packages(test_path("fixtures/packages/PACKAGES2.bz2"))
  p_bzip2 <- parse_packages(test_path("fixtures/packages/PACKAGES2.bzip2"))
  p_xz <- parse_packages(test_path("fixtures/packages/PACKAGES2.xz"))
  expect_equal(p0, p)
  expect_equal(p0, p_gz)
  expect_equal(p0, p_bz2)
  expect_equal(p0, p_bzip2)
  expect_equal(p0, p_xz)
})

test_that("parse_packages, errors", {
  expect_snapshot(
    error = TRUE,
    parse_packages(tempfile(), type = "uncompressed"),
    transform = fix_temp_path,
    variant = get_os_variant()
  )

  p <- "Package: foo\n\n \n"
  expect_snapshot(
    error = TRUE,
    .Call(pkgcache_parse_packages_raw, charToRaw(p)),
    transform = fix_c_line_number
  )

  p <- "Package: foo\nVersion is not good\nAnother: x\n"
  expect_snapshot(
    error = TRUE,
    .Call(pkgcache_parse_packages_raw, charToRaw(p)),
    transform = fix_c_line_number
  )

  p <- "Package: foo\nimcoplete_key"
  expect_snapshot(
    error = TRUE,
    .Call(pkgcache_parse_packages_raw, charToRaw(p))
  )
})

test_that("somewhat weird packages files", {
  testthat::local_edition(3)
  testthat::local_reproducible_output(width = 60)
  pkgs <- parse_packages(test_path("fixtures/packages/P1"))
  expect_snapshot(colnames(pkgs))
  expect_snapshot(pkgs$Package)

  pkgs2 <- parse_packages(test_path("fixtures/packages/P2"))
  expect_equal(pkgs, pkgs2)
})

test_that("parse_packages empty file", {
  # cf. https://github.com/r-lib/pkgcache/issues/107
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  file.create(tmp)
  expect_silent(parse_packages(tmp))
  expect_snapshot(parse_packages(tmp))
})

# https://github.com/r-lib/pkgcache/issues/122
test_that("parse_packages edge case (#122)", {
  res <- list(Package = "foo")
  do <- function(cnt) {
    .Call(pkgcache_parse_packages_raw, charToRaw(cnt))
  }
  expect_equal(do("Package: foo"), res)
  expect_equal(do("Package: foo\n"), res)
  expect_equal(do("Package: foo\n\n"), res)
  expect_equal(do("Package: foo\n\n\n"), res)
  expect_equal(do("Package: foo\n\n\n\n"), res)
  expect_equal(do("Package: foo\n\n\n\n\n"), res)
  expect_equal(do("Package: foo\n\n\n\n\n\n"), res)

  res2 <- list(Package = "foo\r")
  expect_equal(do("Package: foo\r\n"), res2)
  expect_equal(do("Package: foo\r\n\r\n"), res2)
  expect_equal(do("Package: foo\r\n\r\n\r\n"), res2)
  expect_equal(do("Package: foo\r\n\r\n\r\n\r\n"), res2)
})

test_that("parse_installed", {
  testthat::local_edition(3)
  testthat::local_reproducible_output(width = 60)
  pkgs <- parse_installed(test_path("fixtures/lib"))
  expect_snapshot(pkgs$Package)
  expect_true("LibPath" %in% names(pkgs))

  pkgs2 <- parse_installed(
    test_path("fixtures/lib"),
    packages = c("cli", "foo")
  )
  expect_snapshot(pkgs2$Package)
})

test_that("parse_installed, DESCRIPTION with <CR><LF>", {
  testthat::local_edition(3)
  testthat::local_reproducible_output(width = 60)
  pkgs <- parse_installed(test_path("fixtures/lib4"))
  expect_snapshot(pkgs$Package)
  expect_true("LibPath" %in% names(pkgs))
})

test_that("parse_installed, multiple libs", {
  testthat::local_edition(3)
  testthat::local_reproducible_output(width = 60)
  pkgs <- parse_installed(test_path(file.path("fixtures", c("lib", "lib2"))))
  expect_snapshot(pkgs$Package)
  expect_true("LibPath" %in% names(pkgs))
})

test_that("parse_installed, errors", {
  testthat::local_edition(3)
  testthat::local_reproducible_output(width = 60)
  cond <- NULL
  pkgs <- withCallingHandlers(
    parse_installed(test_path(file.path("fixtures", c("lib", "lib2", "lib3")))),
    "pkgcache_broken_install" = function(cnd) {
      cond <<- cnd
      invokeRestart("muffleWarning")
    }
  )
  expect_snapshot(pkgs$Package)
  expect_true("pkgcache_broken_install" %in% class(cond))
  expect_match(cond$errors$file, "lib3[\\\\/]foo[\\\\/]DESCRIPTION$")
  expect_match(cond$errors$error, "ended while parsing a key")
})

test_that("parse_installed, more errors", {
  out <- .Call(pkgcache_parse_descriptions, tempfile(), FALSE)
  expect_true(out[[3]])
  expect_match(out[[2]], "Cannot open file")

  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  cat(":not a good start\n", file = tmp)
  out <- .Call(pkgcache_parse_descriptions, tmp, FALSE)
  expect_true(out[[3]])
  expect_match(out[[2]], "must start with an alphanumeric character")

  cat("Package: foo\nOops again\nfoo:bar\n", file = tmp)
  out <- .Call(pkgcache_parse_descriptions, tmp, FALSE)
  expect_true(out[[3]])
  expect_match(out[[2]], "Line 2 is invalid")
})

test_that("parse_installed priority", {
  testthat::local_edition(3)
  testthat::local_reproducible_output(width = 60)
  lib5 <- test_path("fixtures/lib5")
  expect_snapshot(parse_installed(lib5, priority = "base")$Package)
  expect_snapshot(parse_installed(lib5, priority = "recommended")$Package)
  expect_snapshot(parse_installed(lib5, priority = NA)$Package)
  expect_snapshot(
    parse_installed(lib5, priority = c("base", "recommended"))$Package
  )
  expect_snapshot(parse_installed(lib5, priority = c("base", NA))$Package)
})

test_that("parse_installed lowercase", {
  pkgs <- parse_installed(test_path("fixtures/lib"))
  pkgsl <- parse_installed(test_path("fixtures/lib"), lowercase = TRUE)
  expect_equal(tolower(names(pkgs)), names(pkgsl))
})

as_bytes <- function(l) {
  lapply(l, function(x) {
    Encoding(x) <- "bytes"
    x
  })
}

test_that("fix_encodings", {
  testthat::local_edition(3)
  testthat::local_reproducible_output(width = 60)
  lst <- as_bytes(list(
    Package = c("foo", "bar", "foobar"),
    Encoding = c(NA_character_, "UTF-8", "latin1"),
    Maintainer = c(
      "Gabor",
      "G\u00e1bor",
      iconv("G\u00e1bor", "UTF-8", "latin1")
    ),
    Bad = c("G\u00e1bor", iconv("G\u00e1bor", "UTF-8", "latin1"), "Gabor")
  ))
  lst2 <- fix_encodings(lst)
  expect_snapshot(lst2$Package)
  expect_snapshot(lst2$Encoding)
  expect_snapshot(Encoding(lst2$Maintainer))
  expect_snapshot(lapply(lst2$Maintainer, charToRaw))
  expect_snapshot(Encoding(lst2$Bad))
  expect_snapshot(lapply(lst2$Bad, charToRaw))
})

test_that("fix encodings on data frames", {
  testthat::local_edition(3)
  testthat::local_reproducible_output(width = 60)
  lst <- as_bytes(list(
    Package = c("foo", "bar", "foobar"),
    Encoding = c(NA_character_, "UTF-8", "latin1"),
    Maintainer = c(
      "Gabor",
      "G\u00e1bor",
      iconv("G\u00e1bor", "UTF-8", "latin1")
    ),
    Bad = c("G\u00e1bor", iconv("G\u00e1bor", "UTF-8", "latin1"), "Gabor")
  ))
  tbl <- as_data_frame(lst)
  tbl2 <- fix_encodings(tbl)
  expect_s3_class(tbl2, "tbl")
  expect_snapshot(tbl2$Package)
  expect_snapshot(tbl2$Encoding)
  expect_snapshot(Encoding(tbl2$Maintainer))
  expect_snapshot(lapply(tbl2$Maintainer, charToRaw))
  expect_snapshot(Encoding(tbl2$Bad))
  expect_snapshot(lapply(tbl2$Bad, charToRaw))
})

test_that("parse packages with trailing whitespace (#93)", {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)

  writeLines(c("Package: foo", "", "Package: bar", ""), tmp, sep = "\r\n")
  expect_equal(parse_packages(tmp)$Package, c("foo", "bar"))
})
