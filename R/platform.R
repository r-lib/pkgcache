
#' Platform and R version information of the current session
#'
#' Currently used platforms:
#' - `"source"`
#' - `"macos"`
#' - `"windows"`
#'
#' @return `current_r_platform()` returns a character scalar.
#' @export
#' @examples
#' current_r_platform()

current_r_platform <- function() {
  type <- get_platform()$pkgType
  if (!is_string(type))
    "source"
  else if (grepl("^mac", type)) {
    "macos"
  } else if (grepl("^win", type)) {
    "windows"
  } else {
    "source"
  }
}

#' @rdname current_r_platform
#' @return `default_platform()` returns a character vector of the
#' default platforms.
#' @export
#' @examples
#' default_platforms()

default_platforms <- function() {
  unique(c(current_r_platform(), "source"))
}

#' Query the default CRAN repository for this session
#'
#' If `options("repos")` (see [options()]) contains an entry called
#' `"CRAN"`, then that is returned. If it is a list, it is converted
#' to a character vector.
#'
#' Otherwise the RStudio CRAN mirror is used.
#'
#' @return A named character vector of length one, where the
#'   name is `"CRAN"`.
#'
#' @export
#' @examples
#' default_cran_mirror()

default_cran_mirror <- function() {
  mirror <- getOption("repos")["CRAN"]
  if (is.null(mirror) || is.na(mirror) || mirror == "@CRAN@") {
    c(CRAN = "https://cran.rstudio.com")
  } else {
    unlist(mirror)
  }
}

#' Query Bioconductor version information
#'
#' `bioc_version()` queries the matching Bioconductor version for
#' and R version, defaulting to the current R version
#'
#' @param r_version The R version number to match.
#' @param forget Use `TRUE` to avoid caching the Bioconductor mapping.
#' @return `bioc_version()` returns a [package_version] object.
#'
#' @export
#' @examplesIf pkgcache:::is_online()
#' bioc_version()
#' bioc_version("4.0")
#' bioc_version("4.1")

bioc_version <- function(r_version = getRversion(), forget = FALSE) {
  bioconductor$get_bioc_version(r_version, forget)
}

#' @describeIn bioc_version
#'
#' `bioc_version_map()` returns the current mapping between R versions
#' and Bioconductor versions.
#'
#' @return `bioc_version_map()` returns a tibble with columns:
#' * `bioc_version`: [package_version] object, Bioconductor versions.
#' * `r_version`: [package_version] object, the matching R versions.
#' * `bioc_status`: factor, with levels: `out-of-date`, `release`,
#'   `devel`, `future`.
#'
#' @export
#' @examplesIf pkgcache:::is_online()
#' bioc_version_map()

bioc_version_map <- function(forget = FALSE) {
  tibble::as_tibble(bioconductor$get_version_map(forget))
}
