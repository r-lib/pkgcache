
## nocov start

global_metadata_cache <- NULL

.onLoad <- function(libname, pkgname) {
  if (requireNamespace("debugme", quietly = TRUE)) debugme::debugme()
  global_metadata_cache <<- cranlike_metadata_cache$new()
}

## nocov end
