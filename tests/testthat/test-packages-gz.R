
test_that("packages_make_target", {

  expect_equal(
    packages_make_target("source", "src/contrib", c("p1", "p2"),
                         c("1.0", "2.0"), NULL, NULL),
    c("src/contrib/p1_1.0.tar.gz", "src/contrib/p2_2.0.tar.gz")
  )

  expect_equal(
    packages_make_target("source", "src/contrib", c("p1", "p2"),
                         c("1.0", "2.0"), c("foo", "bar"), NULL),
    c("src/contrib/foo", "src/contrib/bar")
  )

  expect_equal(
    packages_make_target("source", "src/contrib", c("p1", "p2"),
                         c("1.0", "2.0"), c("foo", "bar"), c("1", "2")),
    c("src/contrib/foo", "src/contrib/bar")
  )

  expect_equal(
    packages_make_target("source", "src/contrib", c("p1", "p2"),
                         c("1.0", "2.0"), NULL, c("foo", "bar")),
    c("src/contrib/foo/p1_1.0.tar.gz", "src/contrib/bar/p2_2.0.tar.gz")
  )

  expect_equal(
    packages_make_target("source", "src/contrib", c("p1", "p2", "p3"),
                         c("1.0", "2.0", "3.0"), c("foo", NA, NA),
                         c("foox", "bar", NA)),
    c("src/contrib/foo", "src/contrib/bar/p2_2.0.tar.gz",
      "src/contrib/p3_3.0.tar.gz")
  )
})

test_that("packages_make_sources", {

  expect_equal(
    packages_make_sources(
      "URL", "x86_64-apple-darwin17.0",
      c("s/c/p1_1.0.tgz", "s/c/p2_2.0.tgz"), "s/c",
      c("p1", "p2"), c("1.0", "2.0"), type = "cran", downloadurl = NULL),
    list(c("URL/s/c/p1_1.0.tgz", "https://mac.r-project.org/s/c/p1_1.0.tgz"),
         c("URL/s/c/p2_2.0.tgz", "https://mac.r-project.org/s/c/p2_2.0.tgz")
  ))

  expect_equal(
    packages_make_sources(
      "URL", "source", c("s/c/xx.tar.gz", "s/c/yy.tar.gz"), "s/c",
      c("p1", "p2"), c("1.0", "2.0"), type = "cran", downloadurl = NULL),
    list(c("URL/s/c/xx.tar.gz", "URL/s/c/Archive/p1/p1_1.0.tar.gz"),
         c("URL/s/c/yy.tar.gz", "URL/s/c/Archive/p2/p2_2.0.tar.gz"))
  )

  urls <- c("foo", NA)

  expect_equal(
    packages_make_sources(
      "URL", "x86_64-apple-darwin17.0",
      c("s/c/p1_1.0.tgz", "s/c/p2_2.0.tgz"), "s/c",
      c("p1", "p2"), c("1.0", "2.0"), type = "cran", downloadurl = urls),
    list(c("foo"),
         c("URL/s/c/p2_2.0.tgz", "https://mac.r-project.org/s/c/p2_2.0.tgz")
  ))
})

test_that("read_packages_file", {

  pkg_files <- vcapply(
    file.path("fixtures", paste0("PACKAGES-", c("src", "win", "mac"), ".gz")),
    test_path
  )

  for (pf in pkg_files) {
    pkgs <- read_packages_file(
      pf, mirror = "mirror", repodir = "src/contrib", platform = "source",
      rversion = "rversion")
    check_packages_data(pkgs)
  }
})

test_that("read_packages_file windows", {
  testthat::local_edition(3)
  testthat::local_reproducible_output()
  pkg_file <- test_path("fixtures/PACKAGES-win2.gz")

  for (pl in c("x86_64-w64-mingw32", "i386-w64-mingw32",
               "i386+x86_64-w64-mingw32")) {
    pkgs <- read_packages_file(
      pkg_file,
      mirror = "m",
      repodir = "r",
      platform = pl
    )
    expect_snapshot({
      print(pl)
      pkgs$pkgs[, c("package", "platform")]
    })
  }
})

test_that("packages_parse_deps", {
  pkgs <- read_packages_file(
    test_path("fixtures/PACKAGES-src.gz"), mirror = "mirror",
    repodir = "src/contrib", platform = "source", rversion = "*")

  pkgs1 <- pkgs$pkgs[1,]
  deps <- packages_parse_deps(pkgs1)
  expect_true(inherits(deps, "tbl"))
  expect_equal(
    colnames(deps),
    c("upstream", "idx", "ref", "type", "package", "op",  "version"))
})

test_that("merge_packages_data", {
  pf <- vcapply(
    file.path("fixtures", paste0("PACKAGES-", c("src", "win", "mac"), ".gz")),
    test_path
  )

  pkgsx <- list(
    read_packages_file(pf[1], mirror = "m1", repodir = "r1",
                       platform = "source", rversion = "*"),
    read_packages_file(pf[2], mirror = "m2", repodir = "r2",
                       platform = "windows", rversion = "3.4"),
    read_packages_file(pf[3], mirror = "m3", repodir = "r3",
                       platform = "macos", rversion = "3.5")
  )

  pkgs <- merge_packages_data(.list = pkgsx)

  check_packages_data(pkgs)

  expect_equal(
    nrow(pkgs$pkgs),
    nrow(pkgsx[[1]]$pkgs) + nrow(pkgsx[[2]]$pkgs) + nrow(pkgsx[[3]]$pkgs))
  expect_equal(
    nrow(pkgs$deps),
    nrow(pkgsx[[1]]$deps) + nrow(pkgsx[[2]]$deps) + nrow(pkgsx[[3]]$deps))
  expect_true(
    all(pkgs$pkgs$package[pkgs$deps$idx] == pkgs$deps$upstream))
})

test_that("rbind_expand", {
  d1 <- data_frame(a = 1:2, b = c("a", "b"), c = NA_character_)
  d2 <- data_frame(a = 3:4, c = c("c", "d"), d = c(1L, 2L))
  m <- rbind_expand(d1, d2)
  expect_identical(names(m), c("a", "b", "c", "d"))
  expect_identical(m$a, 1:4)
  expect_identical(m$b, c("a", "b", NA, NA))
  expect_identical(m$c, c(NA, NA, "c", "d"))
  expect_identical(m$d, c(NA, NA, 1L, 2L))
})

test_that("empty PACKAGES file", {
  pkgs <- test_temp_file()
  data <- read_packages_file(pkgs, mirror = "mirror", repodir = "dir",
                             platform = "source", rversion = "rversion")
  check_packages_data(data)
  expect_equal(nrow(data$pkgs), 0)
  expect_equal(nrow(data$deps), 0)
})
