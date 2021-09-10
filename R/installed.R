
# This is not actually used anywhere, we I'll leave it here.
# It might be useful for testing improvements for the more complicated
# parsers.

parse_description <- function(path) {
  path <- path.expand(path)
  path <- encode_path(path)
  ret <- .Call(pkgcache_parse_description, path)
  ret2 <- gsub("\r", "", ret, fixed = TRUE)

  # Fix encoding. If UTF-8 is declared, then we just mark it.
  # Otherwise we convert it to UTF-8. Conversion might throw an error.
  if ("Encoding" %in% names(ret2)) {
    enc <- trimws(ret2[["Encoding"]])
    if (enc == "UTF-8") {
      Encoding(ret2) <- "UTF-8"
    } else {
      trs <- iconv(ret2, enc, "UTF-8")
      ret2[] <- ifelse(is.na(trs), ret2, trs)
    }
  }
  ret2
}

fix_encodings <- function(lst, col = "Encoding") {
  if (! col %in% names(lst)) return(lst)
  utf8 <- which(!is.na(lst[[col]]) & lst[[col]] == "UTF-8")
  other <- which(!is.na(lst[[col]]) & lst[[col]] != "UTf-8")
  unq <- unique(lst[[col]][other])
  if (length(utf8)) {
    for (i in seq_along(lst)) {
      Encoding(lst[[i]][utf8]) <- "UTF-8"
    }
  }
  if (length(unq)) {
    for (u in unq) {
      wh <- which(!is.na(lst[[col]]) & lst[[col]] == u)
      for (i in seq_along(lst)) {
        tryCatch({
          trs <- iconv(lst[[i]][wh], u, "UTF-8")
          lst[[i]][wh] <- ifelse(is.na(trs), lst[[i]][wh], trs)
        }, error = function(e) NULL)
      }
    }
  }
  lst
}

#' Parse a repository metadata `PACAKGES*` file
#'
#' @details
#' Non-existent, unreadable or corrupt `PACKAGES` files with trigger an
#' error.
#'
#' `PACKAGES*` files do not usually declare an encoding, but nevertheless
#' `parse_packages()` works correctly if they do.
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
      x <- gsub("\r", "", fixed = TRUE, x)
      empt <- is.na(x)
      miss <- x == ""
      x[empt] <- ""
      x[miss] <- NA_character_
      x
    })

    # this is rarely needed for PACKAGES files, included for completeness
    tab <- fix_encodings(tab)
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
#' * `parse_installed()` cannot subset the extracted fields. (But you can
#'   subset the result.)
#' * `parse_installed()` does not cache the results.
#' * `parse_installed()` handles errors better. See Section 'Errors' below.
#' #' * `parse_installed()` uses the `DESCRIPTION` files in the installed packages
#'   instead of the `Meta/package.rds` files. This should not matter,
#'   but because of a bug `Meta/package.rds` might contain the wrong
#'   `Archs` field on multi-arch platforms.
#' * `parse_installed()` reads _all_ fields from the `DESCRIPTION` files.
#'   [utils::installed.packages()] only reads the specified fields.
#' * `parse_installed()` converts its output to UTF-8 encoding, from the
#'   encodings declared in the `DESCRIPTION` files.
#' * `parse_installed()` is considerably faster.
#'
#' ## Encodings
#'
#' `parse_installed()` always returns its result in UTF-8 encoding.
#' It uses the `Encoding` fields in the `DESCRIPTION` files to learn their
#' encodings. `parse_installed()` does not check that an UTF-8 file has a
#' valid encoding. If it fails to convert a string to UTF-8 from another
#' declared encoding, then it leaves it as `"bytes"` encoded, without a
#' warning.
#'
#' ## Errors
#'
#' pkgcache silently ignores files and directories inside the library
#' directory.
#'
#' The result also omits broken package installations. These include
#'
#' * packages with invalid `DESCRIPTION` files, and
#' * packages the current user have no access to.
#'
#' These errors are reported via a condition with class
#' `pkgcache_broken_install`. The condition has an `errors` entry, which
#' is a tibble with columns
#'
#' * `file`: path to the `DESCRIPTION` file of the broken package,
#' * `error`: error message for this particular failure.
#'
#' If you intend to handle broken package installation, you need to catch
#' this condition with `withCallingHandlers()`.
#'
#' @param library Character vector of library paths.
#' @param priority If not `NULL` then it may be a `"base"` `"recommended"`
#'   `NA` or a vector of these to select _base_ packages, _recommended_
#'   packages or _other_ packages. (These are the official, CRAN supported
#'   package priorities, but you may introduce others in non-CRAN packages.)
#' @param lowercase Whether to convert keys in `DESCRIPTION` to lowercase.
#'
#' @export

parse_installed <- function(library = .libPaths(), priority = NULL,
                            lowercase = FALSE) {
  stopifnot(
    "`library` must be a character vector" = is.character(library),
    "`priority` must be `NULL` or a character vector" =
      is.null(priority) || is.character(priority) || identical(NA, priority),
    "`library` cannot have length zero" = length(library) > 0,
    "`lowercase` must be a boolean flag" = is_flag(lowercase)
  )

  # Merge multiple libraries
  if (length(library) > 1) {
    lsts <- lapply(
      library,
      parse_installed,
      priority = priority,
      lowercase = lowercase
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
  prs <- .Call(pkgcache_parse_descriptions, dscs, lowercase)
  tab <- prs[[1]]

  tab[] <- lapply(tab, function(x) {
    x <- gsub("\r", "", x, fixed = TRUE)
    empt <- is.na(x)
    miss <- x == ""
    x[empt] <- ""
    x[miss] <- NA_character_
    x
  })

  tbl <- tibble::as_tibble(tab)
  if (lowercase) {
    tbl$libpath <- library
  } else {
    tbl$LibPath <- library
  }

  # Filter out errors
  if (prs[[3]]) {
    bad <- prs[[2]] != ""
    tbl <- tbl[!bad, ]
    cnd <- new_pkgcache_warning(
      "Cannot read DESCRIPTION files:\n", paste0("* ", prs[[2]][bad], "\n"),
      class = "pkgcache_broken_install",
      data = list(errors = tibble(file = dscs[bad], error = prs[[2]][bad]))
    )
    withRestarts(
      muffleWarning = function() NULL,
      signalCondition(cnd)
    )
  }

  # filter for priority
  if (!is.null(priority)) {
    keep <- if (anyNA(priority)) {
      is.na(tbl$Priority)
    } else {
      FALSE
    }
    priority <- na.omit(priority)
    keep <- keep | tbl$Priority %in% priority
    tbl <- tbl[keep, ]
  }

  tbl <- fix_encodings(tbl)

  tbl
}