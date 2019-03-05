
httpbin <- (function() {
  url <- NULL

  update <- function(x) {
    chk_url <- function(url, ...) {
      http_head(url, ...)$
        then(http_stop_for_status)$
        then(function(r) r$url)
    }
    synchronise(when_any(
      chk_url("https://httpbin.org"),
      chk_url("https://eu.httpbin.org")
    ))
  }

  function(endpoint = "") {
    if (is.null(url)) url <<- update()
    paste0(url, endpoint)
  }
})()

is_offline <- (function() {
  offline <- NULL
  function() {
    if (is.null(offline)) {
      offline <<- tryCatch(
        is.na(pingr::ping_port("google.com", port = 443, count = 1L)),
        error = function(e) TRUE
      )
    }
    offline
  }
})()

skip_if_offline <- function() {
  skip_on_cran()
  if (is_offline()) skip("Offline")
}
