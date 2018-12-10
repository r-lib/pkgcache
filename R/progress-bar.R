
#' @importFrom cli get_spinner

create_progress_bar <- function(data) {
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
  wh <- match(url, bar$data$url)
  bar$data$uptodate[[wh]] <- TRUE
  bar$data$current[[wh]] <- NA_integer_
  bar$data$size[[wh]] <- NA_integer_
}

update_progress_bar_done  <- function(bar, url) {
  wh <- match(url, bar$data$url)
  bar$data$uptodate[[wh]] <- FALSE
  bar$data$current[[wh]] <- bar$data$size[[wh]] <-
    file.size(bar$data$path[[wh]])
}

#' @importFrom prettyunits pretty_bytes

show_progress_bar <- function(bar) {
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

finish_progress_bar <- function(bar) {
  if (FALSE %in% bar$data$uptodate) {
    dl <- vlapply(bar$data$uptodate, identical, FALSE)
    files <- sum(dl)
    bytes <- pretty_bytes(sum(bar$data$size[dl], na.rm = TRUE))
    cli_alert_success(
      "Metadata updated, downloaded {bytes} in {files} files.")
  } else {
    files <- nrow(bar$data)
    cli_alert_success("All {files} metadata files are current.")
  }
  bar$bar$terminate()
}
