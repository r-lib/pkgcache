
## nocov start

pkgenv <- new.env(parent = emptyenv())

onload_pkgcache <- function(libname, pkgname) { }

if (exists(".onLoad", inherits = FALSE)) {
  onload_old <- .onLoad
  .onLoad <- function(libname, pkgname) {
    onload_old(libname, pkgname)
    onload_pkgcache(libname, pkgname)
  }
} else {
  .onLoad <- onload_pkgcache
}

## nocov end

#' The R6 object that implements the global metadata cache
#'
#' This is used by the [meta_cache_deps()], [meta_cache_list()], etc.
#' functions.
#'
#' @export
#' @examplesIf FALSE
#' get_cranlike_metadata_cache()
#' get_cranlike_metadata_cache()$list("cli")

get_cranlike_metadata_cache <- function() {
  if (is.null(pkgenv$global_metadata_cache)) {
    pkgenv$global_metadata_cache <- cranlike_metadata_cache$new()
  }
  pkgenv$global_metadata_cache
}
