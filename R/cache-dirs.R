
#' @importFrom rappdirs user_cache_dir

get_user_cache_dir <- function() {
  cdir <- Sys.getenv("R_PKG_CACHE_DIR", "")
  if (cdir == "")  {
    cdir <- tryCatch(
      user_cache_dir("R-pkg"),
      error = function(e) {
        warning(
          "Cannot set package cache directory, using temporary directory. (",
          conditionMessage(e), ")"
        )
        tempfile(pattern = "R-pkg-")
      }
    )
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
