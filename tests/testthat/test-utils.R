
test_that("%||%", {
  expect_equal(NULL %||% 100, 100)
  expect_equal(100 %||% 1000, 100)
})

test_that("viapply", {
  l <- list(NULL, "", character(), 1)
  expect_identical(
    vapply(l, length, integer(1)),
    viapply(l, length)
  )
  expect_identical(
    vapply(list(), length, integer(1)),
    viapply(list(), length)
  )
  expect_error(viapply(l, identity), "values must be length 1")
  expect_error(viapply(letters, identity), "values must be type .*integer")
})

test_that("vcapply", {
  l <- list(NULL, "", character(), 1)
  f <- function(x) "a"
  expect_identical(
    vapply(l, f, character(1)),
    vcapply(l, f)
  )
  expect_identical(
    vapply(list(), f, character(1)),
    vcapply(list(), f)
  )
  expect_error(vcapply(l, identity), "values must be length 1")
  expect_error(vcapply(1:5, identity), "values must be type .*character")
})

test_that("vlapply", {
  l <- list(NULL, "", character(), 1)
  expect_identical(
    vapply(l, is.character, logical(1)),
    vlapply(l, is.character)
  )
  expect_identical(
    vapply(list(), is.character, logical(1)),
    vlapply(list(), is.character)
  )
  expect_error(vlapply(l, identity), "values must be length 1")
  expect_error(vlapply(1:5, identity), "values must be type .*logical")
})

test_that("vdapply", {
  l <- list(NULL, "", character(), 1)
  f <- function(x) 1.0
  expect_identical(
    vapply(l, f, double(1)),
    vdapply(l, f)
  )
  expect_identical(
    vapply(list(), f, double(1)),
    vdapply(list(), f)
  )
  expect_error(vdapply(l, identity), "values must be length 1")
  expect_error(vdapply(letters, identity), "values must be type .*double")
})

test_that("mapx", {
  expect_identical(
    mapx(1:5, identity),
    as.list(1:5)
  )
  expect_identical(
    mapx(1:5, 0, paste0),
    as.list(paste0(1:5, 0))
  )
  expect_identical(
    mapx(1:5, integer(), paste0),
    list()
  )

  expect_error(
    mapx(),
    "No arguments"
  )
  expect_error(
    mapx(1),
    "argument not a function"
  )
  expect_error(
    mapx(identity),
    "No data"
  )
  expect_error(
    mapx(1:2, 1:10, paste),
    "Incompatible data lengths"
  )
})

test_that("lapply_rows", {
  df <- data.frame(a = 1:5, b = 6:10)
  f <- function(r, plus = 5) r$a + r$b + plus
  expect_identical(
    lapply_rows(df, f),
    as.list(df$a + df$b + 5)
  )

  expect_identical(
    lapply_rows(df, f, plus = 0),
    as.list(df$a + df$b + 0)
  )

  expect_identical(
    lapply_rows(df[FALSE, ], f),
    list()
  )
})

test_that("zip_vecs", {
  expect_equal(
    zip_vecs(1:2, 3:4),
    list(c(1L, 3L), c(2L, 4L))
  )

  expect_equal(
    zip_vecs(1:2),
    list(1L, 2L)
  )

  expect_equal(
    zip_vecs(1:2, 3:4, 5:6),
    list(c(1L, 3L, 5L), c(2L, 4L, 6L))
  )
  
  expect_equal(
    zip_vecs(1:2, 3L),
    list(c(1L, 3L), c(2L, 3L))
  )

  # This has changed in R 4.2.0, apparently
  if (getRversion() <= "4.1.100") {
    expect_error(
      zip_vecs(integer(), 3L),
      "zero-length inputs cannot be mixed"
    )
  } else {
    expect_equal(
      zip_vecs(integer(), 3L),
      list()
    )
  }
})

test_that("add_attr", {
  expect_identical(
    x <- add_attr("foo", "att", "value"),
    structure("foo", att = "value")
  )
  expect_identical(
    add_attr(x, "att", "value2"),
    structure("foo", att = "value2")
  )
})

test_that("get_platform", {
  expect_identical(get_platform(), R.version$platform)
})

test_that("read_lines", {
  withr::local_options(encoding = "latin1")
  tmp <- tempfile("pkgcache-test-read-lines")
  on.exit(unlink(tmp), add = TRUE)
  writeBin(as.raw(c(0x47, 0xe1, 0x62, 0x6f, 0x72)), tmp)
  inp <- read_lines(tmp, warn = FALSE)
  expect_equal(inp, "G\u00e1bor")
  expect_equal(Encoding(inp), "UTF-8")
})

test_that("dep_types*", {
  expect_equal(
    sort(dep_types()),
    sort(c(dep_types_hard(), dep_types_soft()))
  )
})

test_that("base_packages", {
  bpkgs <- base_packages()
  dscs <- lapply(bpkgs, packageDescription, lib.loc = .Library)
  prio <- vcapply(dscs, "[[", "Priority")
  expect_true(all(prio == "base"))
})

test_that("is_na_scalar", {
  good <- list(
    NA_character_,
    NA_integer_,
    NA_real_,
    NA_complex_,
    NA
  )
  for (c in good) expect_true(is_na_scalar(c), info = c)
  
  bad <- list(
    NULL,
    c(NA, NA),
    list(NA),
    list()
  )
  for (c in bad) expect_false(is_na_scalar(c), info = c)
})

test_that("drop_nulls", {
  cases <- list(
    list(list(), list()),
    list(NULL, NULL),
    list(list(1, NULL, 2, NULL), list(1, 2)),
    list(list(NULL), list()),
    list(list(NULL, a = 1, b = NULL), list(a = 1))
  )
  for (c in cases) expect_equal(drop_nulls(c[[1]]), c[[2]], info = c[[1]])
})

test_that("null2na", {
  expect_identical(null2na(NULL), NA_character_)
  expect_identical(null2na(NA), NA)
})

test_that("na_omit", {
  cases <- list(
    list(1:5, 1:5),
    list(integer(), integer()),
    list(list(), list()),
    list(list(a = 1, b = 2, c = NA), list(a = 1, b = 2)),
    list(c(a = NA, b = "2", c = NA), c(b = "2"))
  )
  for (c in cases) expect_equal(na_omit(c[[1]]), c[[2]], info = c[[1]])
})

test_that("shasum256", {
  hello <- as.raw(c(0x48, 0x65, 0x6c, 0x6c, 0x6f, 0x20, 0x77, 0x6f, 0x72,
                    0x6c, 0x64, 0x21, 0x0a))
  tmp <- tempfile("pkgcache-test-shasum256")
  on.exit(unlink(tmp), add = TRUE)
  file.create(tmp)
  expect_equal(
    shasum256(tmp),
    "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
  )

  writeBin(hello, tmp)
  expect_equal(
    shasum256(tmp),
    "0ba904eae8773b70c75333db4de2f3ac45a8ad4ddba1b242f0b3cfc199391dd8"
  )
})

test_that("msg_wrap", {
  local_edition(3)
  expect_snapshot(error = TRUE, {
    msg <- msg_wrap(
      "some error message", "\n\n",
      "Could not load or update archive cache. If you think your local ",
      "cache is broken, try deleting it with `cran_archive_cleanup()` or ",
      "the `$cleanup()` method."
    )
    stop(msg)
  })
})

test_that("try_catch_null", {
  expect_null(try_catch_null(stop()))
  expect_equal(try_catch_null("foo"), "foo")
})

test_that("run_examples", {
  # CRAN check
  withr::local_envvar(
    "_R_CHECK_PACKAGE_NAME_" = "foo",
    NOT_CRAN = NA_character_,
    CI = NA_character_
  )
  expect_false(run_examples())

  # local check
  withr::local_envvar(NOT_CRAN = "true")
  expect_false(run_examples())

  # CI check
  withr::local_envvar(CI = "true")
  expect_true(run_examples())

  # not a check
  withr::local_envvar("_R_CHECK_PACKAGE_NAME_" = NA_character_)
  expect_true(run_examples())
})

test_that("modify_vec", {
  expect_equal(
    modify_vec(c(a = 1, b = 10, c = 100), c(a = 5, d = 1000)),
    c(a = 5, b = 10, c = 100, d = 1000)
  )
})

test_that("last", {
  expect_equal(last(1:3), 3)
  expect_equal(last(as.list(1:3)), 3)
  expect_error(last(list()))
})

test_that("get_os_type", {
  expect_equal(get_os_type(), .Platform$OS.type)
})

test_that("encode_path", {
  # To test this properly properlt, we would need to be able to create and
  # delete files non-ascii names. But this is very buggy in base R,
  # so we do it with our own C code. In addition, we would also need to
  # craete file with names that are in the current locale, and are
  # supported by the file system. So it is a bit cumbersome to test this
  # currently....
  mockery::stub(encode_path, "get_os_type", "windows")
  expect_silent(encode_path("G\u00e1bor"))

  mockery::stub(encode_path, "get_os_type", "unix")
  expect_silent(encode_path("G\u00e1bor"))
})

test_that("gzip_decompress", {
  p_gz <- get_fixture("packages/PACKAGES.gz")
  gz <- readBin(p_gz, "raw", file.size(p_gz))
  out <- gzip_decompress(gz)
  expect_true(grepRaw("Package:", out) > 0)
})

test_that("interpret_dependencies", {
  dp <- interpret_dependencies(TRUE)
  expect_equal(names(dp), c("direct", "indirect"))
  expect_equal(interpret_dependencies(dp),  dp)

  expect_equal(
    interpret_dependencies(FALSE),
    list(direct = character(), indirect = character())
  )

  expect_equal(
    interpret_dependencies(NA),
    list(direct = dep_types_hard(), indirect = dep_types_hard())
  )

  expect_equal(
    interpret_dependencies(c("foo", "bar")),
    list(direct = c("foo", "bar"), indirect = c("foo", "bar"))
  )
})

test_that("default_cran_mirror", {
  m1 <- withr::with_options(
    list(repos = c(CRAN = "@CRAN@")),
    default_cran_mirror()
  )
  m2 <- withr::with_options(
    list(repos = NULL),
    default_cran_mirror()
  )
  m3 <- withr::with_options(
    list(repos = c("foo" = "bar")),
    default_cran_mirror()
  )

  expect_true(is.character(m1) && length(m1) == 1 && !is.na(m1))
  expect_identical(m1, m2)
  expect_identical(m1, m3)

  m4 <- withr::with_options(
    list(repos = c(CRAN = "mymirror")),
    default_cran_mirror()
  )
  expect_identical(m4, c(CRAN = "mymirror"))
})

test_that("is_na_scalar", {
  pos <- list(NA, NA_character_, NA_real_, NA_integer_, NA_complex_)
  neg <- list(logical(), integer(), 1, 1L, NULL, "foobar", c(NA, 1))

  for (p in pos) expect_true(is_na_scalar(p))
  for (n in neg) expect_false(is_na_scalar(n))
})

test_that("file.size", {
  tmp <- test_temp_file(create = FALSE)
  expect_equal(file.size(tmp), NA_integer_)
  tmp <- test_temp_file()
  expect_equal(file.size(tmp), 0L)
  cat("1234567890\n", file = tmp)
  expect_true(file.size(tmp) %in% 11:12)
})
