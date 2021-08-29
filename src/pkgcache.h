
#include <R.h>
#include <Rinternals.h>
#include <Rversion.h>

SEXP pkgcache_read_raw(SEXP paths);
SEXP pkgcache_read_description(SEXP path);
SEXP pkgcache_parse_description_raw(SEXP path);
SEXP pkgcache_parse_packages_raw(SEXP raw);
SEXP pkgcache_merge_descriptions(SEXP paths);
