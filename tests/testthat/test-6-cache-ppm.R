test_that("add binary", {
  pc <- package_cache$new(withr::local_tempdir())
  pkg <- withr::local_tempfile()
  writeBin(c(charToRaw("just a file"), as.raw(0x0a)), pkg)
  pc$add(
    pkg,
    "src/contrib/x86_64-pc-linux-gnu-ubuntu-22.04/4.2/pkg_1.0.0.tar.gz",
    platform = "x86_64-pc-linux-gnu-ubuntu-22.04",
    .headers = c(
      "x-content-type-options: nosniff",
      "x-frame-options: DENY",
      "x-package-binary-tag: 4.2-jammy",
      "x-package-type: binary",
      "x-repository-type: RSPM"
    )
  )

  pkgs <- pc$list()
  cols <- setdiff(colnames(pkgs), "fullpath")
  expect_snapshot(pkgs[, cols])
  expect_true(all(file.exists(pkgs$fullpath)))
})

test_that("add source", {
  pc <- package_cache$new(withr::local_tempdir())
  pkg <- withr::local_tempfile()
  writeBin(c(charToRaw("just a file"), as.raw(0x0a)), pkg)
  pc$add(
    pkg,
    "src/contrib/pkg_1.0.0.tar.gz",
    platform = "source",
    .headers = c(
      "x-content-type-options: nosniff",
      "x-frame-options: DENY",
      "x-package-type: source",
      "x-repository-type: RSPM"
    )
  )

  pkgs <- pc$list()
  cols <- setdiff(colnames(pkgs), "fullpath")
  expect_snapshot(pkgs[, cols])
  expect_true(all(file.exists(pkgs$fullpath)))
})

test_that("binary expected, got source", {
  pc <- package_cache$new(withr::local_tempdir())
  pkg <- withr::local_tempfile()
  writeBin(c(charToRaw("just a file"), as.raw(0x0a)), pkg)
  pc$add(
    pkg,
    "src/contrib/x86_64-pc-linux-gnu-ubuntu-22.04/4.2/pkg_1.0.0.tar.gz",
    platform = "x86_64-pc-linux-gnu-ubuntu-22.04",
    .headers = c(
      "x-content-type-options: nosniff",
      "x-frame-options: DENY",
      "x-package-type: source",
      "x-repository-type: RSPM"
    )
  )

  pkgs <- pc$list()
  cols <- setdiff(colnames(pkgs), "fullpath")
  expect_snapshot(pkgs[, cols])
  expect_true(all(file.exists(pkgs$fullpath)))
})

test_that("source expected, got binary", {
  pc <- package_cache$new(withr::local_tempdir())
  pkg <- withr::local_tempfile()
  writeBin(c(charToRaw("just a file"), as.raw(0x0a)), pkg)
  pc$add(
    pkg,
    "src/contrib/pkg_1.0.0.tar.gz",
    platform = "source",
    .headers = c(
      "x-content-type-options: nosniff",
      "x-frame-options: DENY",
      "x-package-binary-tag: 4.2-jammy",
      "x-package-type: binary",
      "x-repository-type: RSPM"
    )
  )

  pkgs <- pc$list()
  cols <- setdiff(colnames(pkgs), "fullpath")
  fix_platform <- function(x) {
    sub("[-._a-zA-Z0-9]+ubuntu-22.04", "*-ubuntu-22.04", x)
  }
  pkgs$path <- fix_platform(pkgs$path)
  pkgs$platform <- fix_platform(pkgs$platform)
  expect_snapshot(pkgs[, cols])
  expect_true(all(file.exists(pkgs$fullpath)))
})

test_that("update_fields_for_ppm_download edge cases", {
  path <- "foobar"
  extra <- list(foo = "bar")

  expect_equal(
    update_fields_for_ppm_download(path, extra, character()),
    list(path = path, extra = extra)
  )

  expect_equal(
    update_fields_for_ppm_download(path, extra, "x-package-type: binary"),
    list(path = path, extra = extra)
  )

  expect_equal(
    update_fields_for_ppm_download(
      path,
      extra,
      c("x-package-type: binary", "x-package-binary-tag: 4.2-foobar")
    ),
    list(path = path, extra = extra)
  )
})
