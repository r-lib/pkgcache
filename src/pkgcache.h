
#include <R.h>
#include <Rinternals.h>
#include <Rversion.h>

SEXP pkgcache_read_raw(SEXP paths);

SEXP pkgcache_parse_description_raw(SEXP raw);
SEXP pkgcache_parse_description(SEXP path);
SEXP pkgcache_parse_descriptions(SEXP paths, SEXP lowercase);

SEXP pkgcache_parse_packages_raw(SEXP raw);

SEXP c_sql3_set_tempdir(SEXP path);
SEXP c_sql3_create_db(SEXP path);
SEXP c_sql3_add_packages(SEXP path, SEXP packages, SEXP query);
