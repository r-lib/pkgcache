
httpbin <- (function() {
  url <- NULL

  update <- function(x) {
    chk_url <- function(url, ...) {
      async::http_head(url, ...)$
        then(async::http_stop_for_status)$
        then(function(r) r$url)
    }
    async::synchronise(async::when_any(
      chk_url("https://httpbin.org"),
      chk_url("https://eu.httpbin.org")
    ))
  }

  function(endpoint = "") {
    if (is.null(url)) url <<- update()
    paste0(url, endpoint)
  }
})()

check_packages_data <- function(pkgs) {
  cols <- packages_gz_cols()
  p_cols <- cols$pkgs
  d_cols <- cols$deps

  expect_equal(length(pkgs), 2)
  miss <- setdiff(p_cols, names(pkgs$pkgs))
  expect_identical(miss, character(0))

  miss2 <- setdiff(d_cols, names(pkgs$deps))
  expect_identical(miss2, character(0))
  expect_true(is.integer(pkgs$deps$idx))
  pkgs$deps$idx <- as.character(pkgs$deps$idx)
  expect_true(all(vlapply(pkgs$deps, is.character)))
}

get_private <- function(x) x$.__enclos_env__$private

`set_private<-` <- function(x, member, value) {
  pr <- get_private(x)
  pr[[member]] <- value
  invisible(x)
}

oneday <- function() as.difftime(1, units = "days")

oneminute <- function() as.difftime(1, units = "mins")
