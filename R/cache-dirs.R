
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
  if (cdir == "") cdir <- R_user_dir("R-pkg", "cache")

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

#' @importFrom rappdirs user_data_dir user_config_dir user_cache_dir
#' @rawNamespace if (getRversion() >= "4.0.0") importFrom(tools, R_user_dir)

my_R_user_dir <- function(package, which = c("data", "config", "cache")) {
  stopifnot(is.character(package), length(package) == 1L)
  which <- match.arg(which)
  if (which == "data") {
    rappdirs::user_data_dir(package)
  } else if (which == "config") {
    rappdirs::user_config_dir(package)
  } else {
    rappdirs::user_cache_dir(package)
  }
}

if (getRversion() < "4.0.0") {
  R_user_dir <- my_R_user_dir
}
