
test_that("init", {
  pc <- package_cache$new(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE))
  expect_true(file.exists(tmp))
})

test_that("add / list / find / delete", {
  pc <- package_cache$new(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE))
  cat("f1\n", file = f1 <- tempfile())

  sha256 <- shasum256(f1)
  new <- pc$add(f1, path = "f/b", package = "p", url = "u",
                etag = "e", sha256 = sha256)

  path <- file.path("f", "b")
  fullpath <- file.path(tmp, path)
  exp <- list(fullpath = fullpath, path = path, package = "p", url = "u",
              etag = "e", sha256 = sha256)

  expect_equal(as.list(new), exp)

  expect_equal(as.list(pc$list()), exp)

  expect_equal(as.list(pc$find(package = "p")), exp)

  pc$copy_to(f2 <- tempfile(), package = "p")
  expect_true(file.exists(f2))
  expect_equal(shasum256(f2), sha256)

  pc$delete(package = "p")

  empty <- data.frame(
    stringsAsFactors = FALSE,
    fullpath = character(),  path = character(), package = character(),
    url = character(), etag = character(), sha256 = character()
  )

  expect_equal(pc$list(), empty)

  pc$find(package = "p")
  expect_equal(pc$find(package = "p"), empty)
})

test_that("add_url", {

  pc <- package_cache$new(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE))

  url <- http$url("/etag/foobar")
  new <- pc$add_url(url, "f/b", package = "p",
                    http_headers = c("accept-encoding" = ""))

  path <- file.path("f", "b")
  fullpath <- file.path(tmp, path)
  exp <- list(fullpath = fullpath, path = path, package = "p", url = url,
              etag = "foobar", sha256 = shasum256(fullpath))
  expect_equal(as.list(new), exp)
})

test_that("copy_or_add, positive", {
  pc <- package_cache$new(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE))

  cat("f1\n", file = f1 <- tempfile())

  sha256 <- shasum256(f1)
  new <- pc$add(f1, path = "f/b", package = "p", url = "u",
                etag = "e", sha256 = sha256)
  attr(new, "action") <- "Had"

  hit <- pc$copy_or_add(f1 <- tempfile(), url = "u", path = "f/b",
                        package = "p")
  expect_true(file.exists(f1))
  expect_equal(readLines(f1, warn = FALSE), "f1")
  expect_equal(new, hit)
})

test_that("copy_or_add, negative", {

  pc <- package_cache$new(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE))

  cat("f1\n", file = f1 <- tempfile())

  sha256 <- shasum256(f1)
  new <- pc$add(f1, path = "f/b", package = "p", url = "u",
                etag = "e", sha256 = sha256)

  url <- http$url("/etag/foobar")
  hit <- pc$copy_or_add(url = url, f1 <- tempfile(), path = "f/b",
    package = "p2", http_headers = c("accept-encoding" = ""))

  path <- file.path("f", "b")
  fullpath <- file.path(tmp, path)
  exp <- list(fullpath = fullpath, path = path, package = "p2", url = url,
              etag = "foobar", sha256 = shasum256(fullpath))
  attr(exp, "action") <- "Got"
  expect_equal(as.list(hit), exp)
  expect_true(file.exists(f1))
  expect_true(any(grepl("url\"*:.*/etag/foobar",
                        readLines(f1, warn = FALSE))))

  hit2 <- pc$find(url = url)
  attr(hit2, "action") <- "Got"
  expect_equal(hit2, hit)
})

test_that("update_or_add, not in cache", {

  pc <- package_cache$new(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE))

  url <- http$url("/etag/foobar")
  hit <- pc$update_or_add(url = url, f1 <- tempfile(), path = "f/b",
    package = "p", http_headers = c("accept-encoding" = ""))

  path <- file.path("f", "b")
  fullpath <- file.path(tmp, path)
  exp <- list(fullpath = fullpath, path = path, package = "p", url = url,
              etag = "foobar", sha256 = shasum256(fullpath))
  attr(exp, "action") <- "Got"
  expect_equal(as.list(hit), exp)

  expect_true(file.exists(f1))
  expect_true(any(grepl("url\"*:.*/etag/foobar",
                        readLines(f1, warn = FALSE))))

  hit2 <- pc$find(url = url)
  attr(hit2, "action") <- "Got"
  expect_equal(hit2, hit)
})

test_that("update_or_add, cache is too old", {

  pc <- package_cache$new(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE))

  cat("f1\n", file = f1 <- tempfile())

  url <- http$url("/etag/foobar")
  sha256 <- shasum256(f1)
  pc$add(f1, path = "f/b", package = "p", url = url, etag = "e", sha256 = sha256)

  hit <- pc$update_or_add(url = url, f1 <- tempfile(), path = "f/b",
    package = "p", http_headers = c("accept-encoding" = ""))

  path <- file.path("f", "b")
  fullpath <- file.path(tmp, path)
  exp <- list(fullpath = fullpath, path = path, package = "p", url = url,
              etag = "foobar", sha256 = shasum256(fullpath))
  attr(exp, "action") <- "Got"
  expect_equal(as.list(hit), exp)

  expect_true(file.exists(f1))
  expect_true(any(grepl("url\"*:.*/etag/foobar",
                        readLines(f1, warn = FALSE))))

  hit2 <- pc$find(url = url, etag = "foobar")
  attr(hit2, "action") <- "Got"
  expect_equal(hit2, hit)
})

test_that("update_or_add, cache is current", {
  pc <- package_cache$new(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE))

  cat("f1\n", file = f1 <- tempfile())

  url <- http$url("/etag/foobar")
  sha256 <- shasum256(f1)
  pc$add(f1, path = "f/b", package = "p", url = url, etag = "foobar",
         sha256 = sha256)

  hit <- pc$update_or_add(url = url, f1 <- tempfile(), path = "f/b",
    package = "p", http_headers = c("accept-encoding" = ""))

  path <- file.path("f", "b")
  fullpath <- file.path(tmp, path)
  exp <- list(fullpath = fullpath, path = path, package = "p", url = url,
              etag = "foobar", sha256 = sha256)
  attr(exp, "action") <- "Current"
  expect_equal(as.list(hit), exp)

  expect_true(file.exists(f1))
  expect_equal(readLines(f1), "f1")

  hit2 <- pc$find(url = url)
  attr(hit2, "action") <- "Current"
  expect_equal(hit2, hit)
})
