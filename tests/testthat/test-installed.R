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
    transform = function(x) fix_temp_path(fix_c_line_number(x)),
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
    .Call(pkgcache_parse_description_raw, charToRaw(d)),
    transform = fix_c_line_number
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

test_that("parse_description comments", {
  # comment before
  d <- lns(
    "# comment",
    "Package: foo"
  )
  expect_equal(
    .Call(pkgcache_parse_description_raw, d),
    c(Package = "foo")
  )
  # comment in between
  d <- lns(
    "Package: foo",
    "# comment",
    "Version: 1.0.0"
  )
  expect_equal(
    .Call(pkgcache_parse_description_raw, d),
    c(Package = "foo", Version = "1.0.0")
  )

  # comment inside a value
  d <- lns(
    "Package: foo",
    "F1: Multi-line field, with a comment.",
    "# comment",
    "# comment2",
    "  It is continued here.",
    "F2: Another one",
    "  still going",
    "# comment",
    "  and still going."
  )
  expect_equal(
    .Call(pkgcache_parse_description_raw, d),
    c(
      Package = "foo",
      F1 = "Multi-line field, with a comment.\n  It is continued here.",
      F2 = "Another one\n  still going\n  and still going."
    )
  )

  # multiple comment lines within a value
  d <- lns(
    "Package: foo",
    "F1: one",
    "# comment",
    "  two",
    "# comment 2",
    "  three"
  )
  expect_equal(
    .Call(pkgcache_parse_description_raw, d),
    c(
      Package = "foo",
      F1 = "one\n  two\n  three"
    )
  )

  # comment at the end
  d <- lns(
    "Package: foo",
    "F1: one",
    "# comment"
  )
  expect_equal(
    .Call(pkgcache_parse_description_raw, d),
    c(
      Package = "foo",
      F1 = "one"
    )
  )
  d <- lns(
    "Package: foo",
    "F1: one",
    "  two",
    "# comment",
    "  three",
    "# comment",
    "# comsdfsdfsdfsdfsf",
    ""
  )
  expect_equal(
    .Call(pkgcache_parse_description_raw, d),
    c(
      Package = "foo",
      F1 = "one\n  two\n  three"
    )
  )

  # comments and whitespace at the beginning
  d <- lns(
    "# comment ",
    "",
    "# more comment",
    "Package: foo"
  )
  expect_equal(
    .Call(pkgcache_parse_description_raw, d),
    c(Package = "foo")
  )
  d <- lns(
    "# comment ",
    "",
    "Package: foo"
  )
  expect_equal(
    .Call(pkgcache_parse_description_raw, d),
    c(Package = "foo")
  )
  d <- lns(
    "# comment ",
    "",
    "# comment 2",
    "Package: foo",
    "bad",
    "field"
  )

  expect_snapshot(
    error = TRUE,
    .Call(pkgcache_parse_description_raw, d),
    transform = fix_c_line_number
  )
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
    .Call(pkgcache_parse_packages_raw, charToRaw(p)),
    transform = fix_c_line_number
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

# https://github.com/r-lib/pak/issues/785
test_that("another parse_packages edge case (r-lib/pak#785)", {
  p <- "Package: foo\nVersion: 1.0.0\n \n\nPackage: bar\nVersion: 2.0.0\n"
  psd <- .Call(pkgcache_parse_packages_raw, charToRaw(p))
  expect_equal(
    psd,
    list(Package = c("foo", "bar"), Version = c("1.0.0", "2.0.0"))
  )
})

test_that("parse_packages comments", {
  # before
  d <- lns(
    "# comment",
    "Package: foo",
    "",
    "Package: bar"
  )
  expect_equal(
    .Call(pkgcache_parse_packages_raw, d),
    list(Package = c("foo", "bar"))
  )

  # between, after
  d <- lns(
    "# comment",
    "Package: foo",
    "# comment",
    "",
    "Package: bar",
    "# comment",
    "# comment"
  )
  expect_equal(
    .Call(pkgcache_parse_packages_raw, d),
    list(Package = c("foo", "bar"))
  )

  # within value
  d <- lns(
    "F: foo",
    "# comment",
    "  bar",
    "",
    "F: bar",
    "# comment",
    "# comment",
    "  baz",
    "V: 1.0.0"
  )
  expect_equal(
    .Call(pkgcache_parse_packages_raw, d),
    list(F = c("foo\n  bar", "bar\n  baz"), V = c("", "1.0.0"))
  )
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

test_that("parse_installed comments", {
  tmp1 <- tempfile()
  tmp2 <- tempfile()
  on.exit(unlink(c(tmp1, tmp2)), add = TRUE)

  wrt <- function(...) {
    bin <- lns(...)
    writeBin(bin, tmp1)
    writeBin(bin, tmp2)
  }

  # comment before
  wrt(
    "# comment",
    "Package: foo"
  )
  expect_equal(
    .Call(pkgcache_parse_descriptions, c(tmp1, tmp2), FALSE)[[1]],
    list(Package = c("foo", "foo"))
  )

  # comment in between
  wrt(
    "Package: foo",
    "# comment",
    "Version: 1.0.0"
  )
  expect_equal(
    .Call(pkgcache_parse_descriptions, c(tmp1, tmp2), FALSE)[[1]],
    list(Package = c("foo", "foo"), Version = c("1.0.0", "1.0.0"))
  )

  # comment inside a value
  wrt(
    "Package: foo",
    "F1: Multi-line field, with a comment.",
    "# comment",
    "# comment2",
    "  It is continued here.",
    "F2: Another one",
    "  still going",
    "# comment",
    "  and still going."
  )
  expect_equal(
    .Call(pkgcache_parse_descriptions, c(tmp1, tmp2), FALSE)[[1]],
    list(
      Package = c("foo", "foo"),
      F1 = rep("Multi-line field, with a comment.\n  It is continued here.", 2),
      F2 = rep("Another one\n  still going\n  and still going.", 2)
    )
  )

  # multiple comment lines within a value
  wrt(
    "Package: foo",
    "F1: one",
    "# comment",
    "  two",
    "# comment 2",
    "  three"
  )
  expect_equal(
    .Call(pkgcache_parse_descriptions, c(tmp1, tmp2), FALSE)[[1]],
    list(
      Package = rep("foo", 2),
      F1 = rep("one\n  two\n  three", 2)
    )
  )

  # comment at the end
  wrt(
    "Package: foo",
    "F1: one",
    "# comment"
  )
  expect_equal(
    .Call(pkgcache_parse_descriptions, c(tmp1, tmp2), FALSE)[[1]],
    list(
      Package = c("foo", "foo"),
      F1 = c("one", "one")
    )
  )
  wrt(
    "Package: foo",
    "F1: one",
    "  two",
    "# comment",
    "  three",
    "# comment",
    "# comsdfsdfsdfsdfsf",
    ""
  )
  expect_equal(
    .Call(pkgcache_parse_descriptions, c(tmp1, tmp2), FALSE)[[1]],
    list(
      Package = rep("foo", 2),
      F1 = rep("one\n  two\n  three", 2)
    )
  )

  # comments and whitespace at the beginning
  wrt(
    "# comment ",
    "",
    "# more comment",
    "Package: foo"
  )
  expect_equal(
    .Call(pkgcache_parse_descriptions, c(tmp1, tmp2), FALSE)[[1]],
    list(Package = rep("foo", 2))
  )
  wrt(
    "# comment ",
    "",
    "Package: foo"
  )
  expect_equal(
    .Call(pkgcache_parse_descriptions, c(tmp1, tmp2), FALSE)[[1]],
    list(Package = c("foo", "foo"))
  )
  wrt(
    "# comment ",
    "",
    "# comment 2",
    "Package: foo",
    "bad",
    "field"
  )

  expect_snapshot(
    .Call(pkgcache_parse_descriptions, c(tmp1, tmp2), FALSE),
    transform = function(x) trimws(fix_temp_path(fix_c_line_number(x)))
  )
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
