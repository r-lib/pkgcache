
get_user_cache_dir <- function() {
  ichk <- Sys.getenv("_R_CHECK_PACKAGE_NAME_", "") != ""
  rdir <- Sys.getenv("R_USER_CACHE_DIR", "")
  cdir <- Sys.getenv("R_PKG_CACHE_DIR", "")

  # If in R CMD check, then refuse unless R_USER_CACHE_DIR is set.
  # This is to prevent R CMD check creating files in the user directories,
  # while a reverse dependency runs its check.
  if (ichk && cdir == "" && rdir == "") {
    stop(
      "R_USER_CACHE_DIR env var not set during package check, ",
      "see https://github.com/r-lib/pkgcache#README"
    )
  }

  # R_PKG_CACHE_DIR first. R_user_dir uses R_USER_CACHE_DIR, if set.
  if (cdir == "") {
    cdir <- R_user_dir("pkgcache", "cache")
  } else {
    cdir <- file.path(cdir, "R", "pkgcache")
  }

  mkdirp(cdir)
  cdir <- normalizePath(cdir)

  res <- list(
    root = cdir,
    pkg  = file.path(cdir, "pkg"),
    meta = file.path(cdir, "_metadata"),
    lock = file.path(cdir, "_metadata.lock")
  )

  mkdirp(res$meta)

  res
}

#' @importFrom rappdirs user_cache_dir
#' @rawNamespace if (getRversion() >= "4.0.0") importFrom(tools, R_user_dir)

my_R_user_dir <- function(package, which = "cache") {
  stopifnot(is.character(package), length(package) == 1L)
  stopifnot(which == "cache")
  ev <- Sys.getenv("R_USER_CACHE_DIR", "")
  if (ev != "") {
    path.expand(file.path(ev, "R", package))
  } else if (Sys.info()[["sysname"]] == "Darwin") {
    path.expand(file.path(user_cache_dir(), "org.R-project.R", "R", package))
  } else if (.Platform$OS.type == "windows") {
    path.expand(file.path(dirname(user_cache_dir()), "R", "cache", "R", package))
  } else {
    path.expand(file.path(user_cache_dir(), "R", package))
  }
}

if (getRversion() < "4.0.0") {
  R_user_dir <- my_R_user_dir
}

cleanup_old_cache_dir <- function(force = FALSE) {
  dir <- user_cache_dir("R-pkg")
  if (!file.exists(dir)) {
    message("Old cache directory does not exists, nothing to do")
    return(invisible())
  }

  if (!interactive() && !force) {
    stop("Set `force = TRUE` in non-interactive sessions")
  }

  if (!force) {
    msg <- glue::glue("Are you sure you want to clean up `{dir}` (y/N)? ")
    ans <- readline(msg)
    if (!ans %in% c("y", "Y")) stop("Aborted")
  }

  unlink(dir, recursive = TRUE, force = TRUE)
  message("Cleaned up cache.")

  invisible()
}
