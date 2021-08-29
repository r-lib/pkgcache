
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

  db <- .Call(pkgcache_read_descriptions, dscs)

  # TODO: signal errors for bad files

  con <- rawConnection(db[[1]])
  on.exit(close(con), add = TRUE)

  # TODO: read.dcf slow, have our own parser...
  tbl <- tibble::as_tibble(read.dcf(con))
  tbl$Library <- library

  tbl
}

parse_description <- function(path) {
  .Call(pkgcache_read_description, path.expand(path))
}
