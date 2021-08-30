
#' @export

parse_description <- function(path) {
  .Call(pkgcache_parse_description, path.expand(path))
}

#' @export

parse_packages <- function(path) {
  stopifnot(
    "`path` must be a character scalar" = is_string(path)
  )
  path <- path.expand(path)

  ext <- tools::file_ext(path)
  if (ext == "rds") {
    tab <- readRDS(path)

  } else {
    cmp <- .Call(pkgcache_read_raw, path)[[1]]
    if (ext == "gz") {
      bts <- memDecompress(cmp, type = "gzip")
    } else if (ext %in% c("bz2", "bzip2")) {
      bts <- memDecompress(cmp, type = "bzip2")
    } else if (ext == "xz") {
      bts <- memDecompress(cmp, type = "xz")
    } else if (ext == "") {
      bts <- cmp
    } else {
      stop("Unknown PACKAGES file format in `", path, "`")
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

  dscs <- if (.Platform$OS.type == "windows") {
    enc2utf8(dscs)
  } else {
    enc2native(dscs)
  }

  tab <- .Call(pkgcache_parse_descriptions, dscs)

  # TODO: signal errors for bad files

  tab[] <- lapply(tab, function(x) {
    empt <- is.na(x)
    miss <- x == ""
    x[empt] <- ""
    x[miss] <- NA_character_
    x
  })

  tbl <- tibble::as_tibble(tab)

  tbl
}
