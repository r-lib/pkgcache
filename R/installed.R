
# This is not actually used anywhere, we I'll leave it here.
# It might be useful for testing improvements for the more complicated
# parsers.

parse_description <- function(path) {
  path <- path.expand(path)
  path <- encode_path(path)
  .Call(pkgcache_parse_description, path)
}

#' Parse a repository metadata `PACAKGES*` file
#'
#' @details
#' Non-existent, unreadable or corrupt `PACKAGES` files with trigger an
#' error.
#'
#' # Note
#' `parse_packages()` cannot currently read files that have very many
#' different fields (many columns in the result tibble). The current
#' limit is 1000. Typical `PACKAGES` files contain less than 20 field
#' types.
#'
#' @param path Path to `PACKAGES`. The file can be `gzip` compressed, with
#'   extension `.gz`; `bzip2` compressed, with extension `.bz2` or `bzip2`;
#'   or `xz` compressed with extension `xz`. It may also be a `PACKAGES.rds`
#'   file, which will be read using [base::readRDS()]. Otherwise the file at
#'   `path` is assumed to be uncompressed.
#' @return A tibble, with all columns from the file at `path`.
#'
#' @export

parse_packages <- function(path) {
  stopifnot(
    "`path` must be a character scalar" = is_string(path)
  )
  path <- path.expand(path)
  path <- encode_path(path)

  ext <- tools::file_ext(path)
  if (ext == "rds") {
    tab <- readRDS(path)

  } else {
    cmp <- .Call(pkgcache_read_raw, path)[[1]]
    if (is.character(cmp)) {
      stop(cmp)
    }

    if (ext == "gz") {
      if (getRversion() >= "4.0.0") {
        bts <- memDecompress(cmp, type = "gzip")
      } else {
        bts <- gzip_decompress(cmp)
      }
    } else if (ext %in% c("bz2", "bzip2")) {
      bts <- memDecompress(cmp, type = "bzip2")
    } else if (ext == "xz") {
      bts <- memDecompress(cmp, type = "xz")
    } else {
      bts <- cmp
    }

    tab <- .Call(pkgcache_parse_packages_raw, bts)
    tab[] <- lapply(tab, function(x) {
      empt <- is.na(x)
      miss <- x == ""
      x[empt] <- ""
      x[miss] <- NA_character_
      x
    })
  }

  tbl <- tibble::as_tibble(tab)

  tbl
}

#' List metadata of installed packages
#'
#' This function is similar to [utils::installed.packages()].
#' See the differences below.
#'
#' @details
#' Differences with [utils::installed.packages()]:
#' * `lib_status()` cannot subset the extracted fields. (But you can
#'   subset the result.)
#' * `lib_status()` cannot filter the packages based
#'   on their priority. (But you can filter the result.)
#' * `lib_status()` does not cache the results.
#' * `lib_status()` handles errors better. See Section 'Errors'`below.
#' #' * `lib_status()` uses the `DESCRIPTION` files in the installed packages
#'   instead of the `Meta/package.rds` files. This should not matter,
#'   but because of a bug `Meta/package.rds` might contain the wrong
#'   `Archs` field on multi-arch platforms.
#' * `lib_status()` reads _all_ fields from the `DESCRIPTION` files.
#'   [utils::installed.packages()] only reads
#' * `lib_status()` is considerably faster.
#'
#' ## Errors
#'
#' TODO
#'
#' @param library Character vector of library paths.
#'
#' @export

lib_status <- function(library = .libPaths()) {
  stopifnot(
    "`library` must be a charcater vector" = is.character(library),
    "`library` cannot have length zero" = length(library) > 0
  )

  # Merge multiple libraries
  if (length(library) > 1) {
    lsts <- lapply(
      library,
      lib_status
    )
    return(rbind_expand(.list = lsts))
  }

  # Handle ~ in path name
  library <- path.expand(library)

  # NOTE: a package is a directory with `DESCRIPTION`, other
  #       files and directories are ignored.
  # TODO: replace dir() with something that errors if library does not
  #       exist or we cannot get the list of directories.
  dscs <- file.path(library, dir(library), "DESCRIPTION")
  dscs <- dscs[file.exists(dscs)]

  dscs <- encode_path(dscs)
  prs <- .Call(pkgcache_parse_descriptions, dscs)
  tab <- prs[[1]]

  tab[] <- lapply(tab, function(x) {
    empt <- is.na(x)
    miss <- x == ""
    x[empt] <- ""
    x[miss] <- NA_character_
    x
  })

  tbl <- tibble::as_tibble(tab)
  tbl$LibPath <- library

  # Filter out errors
  if (prs[[3]]) {
    bad <- prs[[2]] != ""
    tbl <- tbl[!bad, ]
    cnd <- new_pkgcache_warning(
      "Cannot read DESCRIPTION files:\n", paste0("* ", prs[[2]][bad], "\n"),
      class = "pkgcache_broken_package",
      data = list(errors = tibble(file = dscs[bad], error = prs[[2]][bad]))
    )
    withRestarts(
      muffleWarning = function() NULL,
      signalCondition(cnd)
    )
  }

  tbl
}
