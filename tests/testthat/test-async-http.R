
context("async http")

test_that("read_etag", {
  cat("foobar\n", file = tmp <- tempfile())
  expect_equal(read_etag(tmp), "foobar")

  cat("\"foobar\"", file = tmp)
  expect_equal(read_etag(tmp), "\"foobar\"")

  cat(" ", file = tmp)
  expect_equal(read_etag(tmp), " ")

  cat("", file = tmp)
  expect_true(length(tmp) == 1 && is.na(read_etag(tmp)))

  cat("foo\nbar", file = tmp)
  expect_equal(read_etag(tmp), "foo")

  expect_true(is.na(read_etag(tempfile())))
})

test_that("download_file", {

  skip_if_offline()

  dir.create(dir <- tempfile())
  dx <- synchronise(download_file(
    url    <- "https://httpbin.org/response-headers?etag=foobar",
    target <- file.path(dir, "file1"),
    etag   <- file.path(dir, "etag"),
    headers = c("accept-encoding" = "")
  ))

  expect_true(file.exists(target))
  expect_equal(jsonlite::fromJSON(target)$etag, "foobar")
  expect_true(file.exists(etag))
  expect_equal(read_lines(etag), "foobar")
})

test_that("download_file, errors", {

  skip_if_offline()

  tmp <- tempfile()
  expect_error(
    synchronise(download_file("http://0.42.42.42", tmp)),
    class = c("async_rejected", "async_http_error")
  )

  expect_error(
    synchronise(download_file("https://eu.httpbin.org/status/404", tmp)),
    class = c("async_rejected", "async_http_401", "async_http_error")
  )
})

test_that("download_if_newer, no etag file", {

  skip_if_offline()

  dir.create(dir <- tempfile())
  dx <- synchronise(download_if_newer(
    url    <- "https://httpbin.org/etag/foobar",
    target <- file.path(dir, "file1"),
    etag   <- file.path(dir, "etag"),
    headers = c("accept-encoding" = "")
  ))

  expect_true(file.exists(target))
  expect_equal(jsonlite::fromJSON(target)$url, url)
  expect_true(file.exists(etag))
  expect_equal(read_lines(etag), "foobar")
})

test_that("download_if_newer, different etag", {

  skip_if_offline()

  dir.create(dir <- tempfile())

  cat("eeeetag\n", file = etag <- file.path(dir, "etag"))
  dx <- synchronise(download_if_newer(
    url    <- "https://httpbin.org/etag/foobar",
    target <- file.path(dir, "file1"),
    etag,
    headers = c("accept-encoding" = "")
  ))

  expect_true(file.exists(target))
  expect_equal(jsonlite::fromJSON(target)$url, url)
  expect_true(file.exists(etag))
  expect_equal(read_lines(etag), "foobar")
})

test_that("download_if_newer, matching etag", {

  skip_if_offline()

  dir.create(dir <- tempfile())

  cat("foobar\n", file = etag <- file.path(dir, "etag"))
  cat("dummy\n", file = target <- file.path(dir, "file1"))
  dx <- synchronise(download_if_newer(
    url    <- "https://httpbin.org/etag/foobar",
    target,
    etag
  ))

  expect_true(file.exists(target))
  expect_equal(read_lines(target), "dummy")
  expect_true(file.exists(etag))
  expect_equal(read_lines(etag), "foobar")
  expect_equal(dx$response$status_code, 304)
})

test_that("download_if_newer, error", {

  skip_if_offline()

  cat("dummy\n", file = target <- tempfile())
  on.exit(unlink(target), add = TRUE)

  expect_error(
    synchronise(download_if_newer(
      url <- "http://0.42.42.42",
      destfile = target
    )),
    class = c("async_rejected", "async_http_error")
  )

  expect_error(
    synchronise(download_if_newer(
      url <- "https://httpbin.org/status/404",
      destfile = target
    )),
    class = c("async_rejected", "async_http_404", "async_http_error")
  )

  err <- tryCatch(
    synchronise(download_if_newer(
      "https://httpbin.org/status/201",
      destfile = target
    )),
    error = function(e) e)
  expect_s3_class(err, "async_rejected")
  expect_match(conditionMessage(err), "Unknown HTTP response")
})

test_that("download_one_of", {

  skip_if_offline()

  dx <- synchronise(download_one_of(
    c("https://httpbin.org/status/404",
      "https://httpbin.org/status/403",
      "https://httpbin.org/get?q=1"),
    tmp <- tempfile()
  ))

  res <- jsonlite::fromJSON(read_lines(tmp), simplifyVector = FALSE)
  expect_equal(res$args$q, "1")
})

test_that("download_one_of, etag", {

  skip_if_offline()

  dir.create(dir <- tempfile())

  cat("eeeetag\n", file = etag <- file.path(dir, "etag"))
  dx <- synchronise(download_one_of(
    c("https://httpbin.org/status/404",
      "https://httpbin.org/status/403",
      url <- "https://httpbin.org/etag/foobar"),
    target <- file.path(dir, "file1"),
    etag_file = etag,
    headers = c("accept-encoding" = "")
  ))

  expect_true(file.exists(target))
  expect_equal(jsonlite::fromJSON(target)$url, url)
  expect_true(file.exists(etag))
  expect_equal(read_lines(etag), "foobar")
})

test_that("download_one_of, matching etag", {

  skip_if_offline()

  dir.create(dir <- tempfile())

  cat("foobar\n", file = etag <- file.path(dir, "etag"))
  cat("dummy\n", file = target <- file.path(dir, "file1"))
  dx <- synchronise(download_one_of(
    c("https://httpbin.org/status/404",
      "https://httpbin.org/status/403",
      url <- "https://httpbin.org/etag/foobar"),
    target,
    etag_file = etag
  ))

  expect_true(file.exists(target))
  expect_equal(read_lines(target), "dummy")
  expect_true(file.exists(etag))
  expect_equal(read_lines(etag), "foobar")
})

test_that("download_one_of, errors", {

  skip_if_offline()

  tmp <- tempfile()

  afun <- async(function() {
    download_one_of(
      c("https://httpbin.org/status/404",
        "https://httpbin.org/status/403",
        "https://httpbin.org/status/404"),
      tmp
    )
  })

  err <- tryCatch(synchronise(afun()), error = identity)
  expect_match(conditionMessage(err), "All URLs failed")
  expect_true("download_one_of_error" %in% class(err))
  expect_false(file.exists(tmp))
})

test_that("download_files", {

  skip_if_offline()

  dir <- test_temp_dir()
  downloads <- data.frame(
    stringsAsFactors = FALSE,
    url  = paste0("https://httpbin.org/etag/foobar", 1:3),
    path = file.path(dir, paste0("file", 1:3)),
    etag = file.path(dir, paste0("etag", 1:3))
  )

  ## First file has no etag file
  unlink(downloads$etag[1], recursive = TRUE)
  ## Second has a different etag, so response must be 200
  cat("eeeetag\n", file = downloads$etag[2])
  ## Third has the same
  cat("foobar3\n", file = downloads$etag[3])
  cat("dummy\n", file = downloads$path[3])

  ret <- synchronise(download_files(
    downloads,
    headers = c("accept-encoding" = "")
  ))

  expect_equal(file.exists(downloads$path), rep(TRUE, 3))
  expect_equal(file.exists(downloads$etag), rep(TRUE, 3))
  for (i in 1:2) {
    expect_equal(jsonlite::fromJSON(downloads$path[i])$url,
                 downloads$url[i])
    expect_equal(read_lines(downloads$etag[i]), paste0("foobar", i))
  }
  expect_equal(read_lines(downloads$path[3]), "dummy")

  expect_equal(ret[[1]]$response$status_code, 200)
  expect_equal(ret[[2]]$response$status_code, 200)
  expect_equal(ret[[3]]$response$status_code, 304)
})

test_that("download_files errors", {

  dir <- test_temp_dir()
  downloads <- data.frame(
    stringsAsFactors = FALSE,
    url  = paste0("https://httpbin.org/etag/foobar", 1:3),
    path = "thesamepath",
    etag = file.path(dir, paste0("etag", 1:3))
  )

  expect_error(
    synchronise(download_files(downloads)),
    "Duplicate target paths")
})
