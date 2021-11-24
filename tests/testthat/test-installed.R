
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

  # \r\n line endings
  d <- parse_description(get_fixture("description/cli/DESCRIPTION"))
  expect_equal(d[["Encoding"]], "UTF-8")
})

test_that("parse_description encoding", {
  d <- parse_description(get_fixture("description/pkgcache/DESCRIPTION"))
  expect_equal(Encoding(d[["Authors@R"]]), "UTF-8")

  d2 <- parse_description(get_fixture("description/pkgcachel1/DESCRIPTION"))
  expect_equal(Encoding(d2[["Authors@R"]]), "UTF-8")
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

test_that("parse_packages, <CR><LF>", {
  p0      <- parse_packages(get_fixture("packages/PACKAGES"))
  p       <- parse_packages(get_fixture("packages/PACKAGES2"))
  p_gz    <- parse_packages(get_fixture("packages/PACKAGES2.gz"))
  p_bz2   <- parse_packages(get_fixture("packages/PACKAGES2.bz2"))
  p_bzip2 <- parse_packages(get_fixture("packages/PACKAGES2.bzip2"))
  p_xz    <- parse_packages(get_fixture("packages/PACKAGES2.xz"))
  expect_equal(p0, p)
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

  pkgs2 <- parse_packages(get_fixture("packages/P2"))
  expect_equal(pkgs, pkgs2)
})

test_that("parse_installed", {
   testthat::local_edition(3)
  testthat::local_reproducible_output(width = 60)
  pkgs <- parse_installed(get_fixture("lib"))
  expect_snapshot(pkgs$Package)
  expect_true("LibPath" %in% names(pkgs))
})

test_that("parse_installed, DESCRIPTION with <CR><LF>", {
  testthat::local_edition(3)
  testthat::local_reproducible_output(width = 60)
  pkgs <- parse_installed(get_fixture("lib4"))
  expect_snapshot(pkgs$Package)
  expect_true("LibPath" %in% names(pkgs))
})

test_that("parse_installed, multiple libs", {
  testthat::local_edition(3)
  testthat::local_reproducible_output(width = 60)
  pkgs <- parse_installed(get_fixture(c("lib", "lib2")))
  expect_snapshot(pkgs$Package)
  expect_true("LibPath" %in% names(pkgs))
})

test_that("parse_installed, errors", {
  testthat::local_edition(3)
  testthat::local_reproducible_output(width = 60)
  cond <- NULL
  pkgs <- withCallingHandlers(
    parse_installed(get_fixture(c("lib", "lib2", "lib3"))),
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
  lib5 <- get_fixture("lib5")
  expect_snapshot(parse_installed(lib5, priority = "base")$Package)
  expect_snapshot(parse_installed(lib5, priority = "recommended")$Package)
  expect_snapshot(parse_installed(lib5, priority = NA)$Package)
  expect_snapshot(parse_installed(lib5, priority = c("base", "recommended"))$Package)
  expect_snapshot(parse_installed(lib5, priority = c("base", NA))$Package)
})

test_that("parse_installed lowercase", {
  pkgs <- parse_installed(get_fixture("lib"))
  pkgsl <- parse_installed(get_fixture("lib"), lowercase = TRUE)
  expect_equal(tolower(names(pkgs)), names(pkgsl))
})

as_bytes <- function(l) {
  lapply(l, function(x) { Encoding(x) <- "bytes"; x })
}

test_that("fix_encodings", {
  testthat::local_edition(3)
  testthat::local_reproducible_output(width = 60)
  lst <- as_bytes(list(
    Package = c("foo", "bar", "foobar"),
    Encoding = c(NA_character_, "UTF-8", "latin1"),
    Maintainer = c("Gabor", "G\u00e1bor", iconv("G\u00e1bor", "UTF-8", "latin1")),
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

test_that("fix encodings on tibbles", {
  testthat::local_edition(3)
  testthat::local_reproducible_output(width = 60)
  lst <- as_bytes(list(
    Package = c("foo", "bar", "foobar"),
    Encoding = c(NA_character_, "UTF-8", "latin1"),
    Maintainer = c("Gabor", "G\u00e1bor", iconv("G\u00e1bor", "UTF-8", "latin1")),
    Bad = c("G\u00e1bor", iconv("G\u00e1bor", "UTF-8", "latin1"), "Gabor")
  ))
  tbl <- tibble::as_tibble(lst)
  tbl2 <- fix_encodings(tbl)
  expect_s3_class(tbl2, "tbl_df")
  expect_snapshot(tbl2$Package)
  expect_snapshot(tbl2$Encoding)
  expect_snapshot(Encoding(tbl2$Maintainer))
  expect_snapshot(lapply(tbl2$Maintainer, charToRaw))
  expect_snapshot(Encoding(tbl2$Bad))
  expect_snapshot(lapply(tbl2$Bad, charToRaw))
})
