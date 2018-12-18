
#' @importFrom cli get_spinner

create_progress_bar <- function(data) {
  if (!is_verbose()) return()
  bar <- new.env(parent = emptyenv())

  format <- ":xspinner Metadata current [:xok/:xtotal] | Downloading :xdl"
  bar$bar <- cliapp::cli_progress_bar(
    format = format, total = 1000, force = TRUE)

  bar$spinner <- get_spinner()
  bar$spinner_state <- 1L

  bar$data <- data
  bar$data$uptodate <- NA
  bar$data$size <- NA_integer_
  bar$data$current  <- NA_integer_

  bar$timer <- async_timer$new(1/10, function() show_progress_bar(bar))
  bar$timer$listen_on("error", function(...) { })

  bar
}

update_progress_bar_progress <- function(bar, data) {
  if (!is_verbose()) return()
  wh <- match(data$url, bar$data$url)
  ## If TRUE, then it stays TRUE, status 304 might report progress, we
  ## want to ignore that
  if (!isTRUE(bar$data$uptodate)) {
    bar$data$uptodate[[wh]] <- FALSE
    bar$data$current[[wh]] <- data[["current"]]
    bar$data$size[[wh]] <- data[["total"]]
  }
}

update_progress_bar_uptodate <- function(bar, url) {
  if (!is_verbose()) return()
  wh <- match(url, bar$data$url)
  bar$data$uptodate[[wh]] <- TRUE
  bar$data$current[[wh]] <- NA_integer_
  bar$data$size[[wh]] <- NA_integer_
}

update_progress_bar_done  <- function(bar, url) {
  if (!is_verbose()) return()
  wh <- match(url, bar$data$url)
  bar$data$uptodate[[wh]] <- FALSE
  bar$data$current[[wh]] <- bar$data$size[[wh]] <-
    file.size(bar$data$path[[wh]])
}

#' @importFrom prettyunits pretty_bytes

show_progress_bar <- function(bar) {
  if (!is_verbose()) return()
  data <- bar$data
  uptodate <- sum(data$uptodate, na.rm = TRUE)
  numfiles <- nrow(data)
  current <- sum(data$current, na.rm = TRUE)
  total <- sum(data$size, na.rm = TRUE)
  downloads <- paste0(
    "[", pretty_bytes(current), " / ", pretty_bytes(total), "]")

  spinner <- bar$spinner$frames[bar$spinner_state]
  bar$spinner_state <- bar$spinner_state + 1L
  if (bar$spinner_state > length(bar$spinner$frames)) {
    bar$spinner_state <- 1L
  }

  tokens <- list(
    xspinner = spinner,
    xok = uptodate,
    xtotal = numfiles,
    xdl = downloads
  )
  bar$bar$tick(0, tokens = tokens)
}

#' @importFrom cliapp cli_alert_danger

finish_progress_bar <- function(ok, bar) {
  if (!is_verbose()) return()
  if (!ok) {
    cli_alert_danger("Metadata download failed")

  } else if (FALSE %in% bar$data$uptodate) {
    dl <- vlapply(bar$data$uptodate, identical, FALSE)
    files <- sum(dl)
    bytes <- pretty_bytes(sum(bar$data$size[dl], na.rm = TRUE))
    cli_alert_success(
      "Downloaded metadata files, {bytes} in {files} files.")

  } else {
    files <- nrow(bar$data)
    cli_alert_success("All {files} metadata files are current.")
  }

  bar$bar$terminate()
}

#' @importFrom withr defer

cli_start_process <- function(msg, envir = parent.frame()) {
  if (!is_verbose()) {
    return(list(done = function() {}, terminate = function() {} ))
  }

  bar <- cliapp::cli_progress_bar(
    format = ":xsym :xmsg", total = 1, force = TRUE,
    show_after = 0, clear = FALSE)

  ## This should come from the theme....
  xsym <- crayon::cyan(cli::symbol$info)
  bar$tick(0, tokens = list(xsym = xsym, xmsg = msg))

  ## This needs to be called for a clean exit
  bar$done <- function() {
    xsym <- crayon::green(cli::symbol$tick)
    bar$tick(0, tokens = list(xsym = xsym, xmsg = crayon::reset(msg)))
    bar$terminate()
  }

  ## This will be called automatically, but if called after done(),
  ## it does not print anything
  defer({
    xsym <- crayon::red(cli::symbol$cross)
    bar$tick(0, tokens = list(xsym = xsym, xmsg = crayon::reset(msg)))
    bar$terminate()
  }, envir = envir)

  bar
}
