
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

  if (packageVersion("presser") <= "1.0.0") {
    skip("Needs newer presser package")
  }

  dir.create(dir <- tempfile())
  dx <- synchronise(download_file(
    url    <- http$url("/response-headers?etag=foobar"),
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
    synchronise(download_file(http$url("/status/404"), tmp)),
    class = c("async_rejected", "async_http_401", "async_http_error")
  )

  ret <- synchronise(download_file(
    "https://eu.httpbin.org/status/404",
    tmp,
    error_on_status = FALSE
  ))
  expect_s3_class(ret, "async_rejected")
  expect_s3_class(ret, "async_http_404")
  expect_s3_class(ret, "async_http_error")
})

test_that("download_if_newer, no etag file", {

  dir.create(dir <- tempfile())
  dx <- synchronise(download_if_newer(
    url    <- http$url("/etag/foobar"),
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

  dir.create(dir <- tempfile())

  cat("eeeetag\n", file = etag <- file.path(dir, "etag"))
  dx <- synchronise(download_if_newer(
    url    <- http$url("/etag/foobar"),
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

  dir.create(dir <- tempfile())

  cat("foobar\n", file = etag <- file.path(dir, "etag"))
  cat("dummy\n", file = target <- file.path(dir, "file1"))
  dx <- synchronise(download_if_newer(
    url    <- http$url("/etag/foobar"),
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
      url <- http$url("/status/404"),
      destfile = target
    )),
    class = c("async_rejected", "async_http_404", "async_http_error")
  )

  err <- tryCatch(
    synchronise(download_if_newer(
      http$url("/status/201"),
      destfile = target
    )),
    error = function(e) e)
  expect_s3_class(err, "async_rejected")
  expect_match(conditionMessage(err), "Unknown HTTP response")

  ret <- synchronise(download_if_newer(
    http$url("/status/404"),
    destfile = target,
    error_on_status = FALSE
  ))
  expect_s3_class(ret, "async_rejected")
  expect_s3_class(ret, "async_http_404")
  expect_s3_class(ret, "async_http_error")
})

test_that("download_one_of", {

  dx <- synchronise(download_one_of(
    http$url(c("/status/404", "/status/403", "/get?q=1")),
    tmp <- tempfile()
  ))

  res <- jsonlite::fromJSON(
    read_lines(tmp, warn = FALSE),
    simplifyVector = FALSE
  )
  expect_equal(res$args$q, "1")
})

test_that("download_one_of, etag", {

  dir.create(dir <- tempfile())

  cat("eeeetag\n", file = etag <- file.path(dir, "etag"))
  dx <- synchronise(download_one_of(
    c(http$url("/status/404"),
      http$url("/status/403"),
      url <- http$url("/etag/foobar")),
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

  dir.create(dir <- tempfile())

  cat("foobar\n", file = etag <- file.path(dir, "etag"))
  cat("dummy\n", file = target <- file.path(dir, "file1"))
  dx <- synchronise(download_one_of(
    http$url(c("/status/404", "/status/403", "/etag/foobar")),
    target,
    etag_file = etag
  ))

  expect_true(file.exists(target))
  expect_equal(read_lines(target), "dummy")
  expect_true(file.exists(etag))
  expect_equal(read_lines(etag), "foobar")
})

test_that("download_one_of, errors", {

  tmp <- tempfile()

  afun <- async(function() {
    download_one_of(
      http$url(c("/status/404", "/status/403", "/status/404")),
      tmp
    )
  })

  err <- tryCatch(synchronise(afun()), error = identity)
  expect_match(conditionMessage(err), "All URLs failed")
  expect_true("download_one_of_error" %in% class(err))
  expect_false(file.exists(tmp))

  afun2 <- async(function() {
    download_one_of(
      http$url(c("/status/404", "/status/403")),
      error_on_status = FALSE,
      tmp
    )
  })
  ret <- synchronise(afun2())
  expect_s3_class(ret, "download_one_of_error")
  expect_s3_class(ret, "async_rejected")
  expect_equal(length(ret$error), 2)
  expect_s3_class(ret$error[[1]], "async_rejected")
  expect_s3_class(ret$error[[1]], "async_http_error")
  expect_s3_class(ret$error[[2]], "async_rejected")
  expect_s3_class(ret$error[[2]], "async_http_error")
  cl <- c(class(ret$error[[1]]), class(ret$error[[2]]))
  expect_true("async_http_404" %in% cl)
  expect_true("async_http_403" %in% cl)
})

test_that("download_files", {

  dir <- test_temp_dir()
  downloads <- data.frame(
    stringsAsFactors = FALSE,
    url  = http$url(paste0("/etag/foobar", 1:3)),
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
    url  = http$url(paste0("/etag/foobar", 1:3)),
    path = "thesamepath",
    etag = file.path(dir, paste0("etag", 1:3))
  )

  expect_error(
    synchronise(download_files(downloads)),
    "Duplicate target paths")
})

test_that("download_files, no errors", {

  dir <- test_temp_dir()
  downloads <- data.frame(
    stringsAsFactors = FALSE,
    url  = http$url(paste0("/status/", 400 + 1:3)),
    path = file.path(dir, paste0("file", 1:3)),
    etag = file.path(dir, paste0("etag", 1:3))
  )

  ret <- synchronise(download_files(downloads, error_on_status = FALSE))
  expect_equal(length(ret), 3)
  expect_s3_class(ret[[1]], "async_rejected")
  expect_s3_class(ret[[1]], "async_http_401")
  expect_s3_class(ret[[1]], "async_http_error")

  expect_s3_class(ret[[2]], "async_rejected")
  expect_s3_class(ret[[2]], "async_http_402")
  expect_s3_class(ret[[2]], "async_http_error")

  expect_s3_class(ret[[3]], "async_rejected")
  expect_s3_class(ret[[3]], "async_http_403")
  expect_s3_class(ret[[3]], "async_http_error")
})
