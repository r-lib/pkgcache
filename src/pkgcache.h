
#include <R.h>
#include <Rinternals.h>
#include <Rversion.h>

SEXP pkgcache_read_raw(SEXP paths);

SEXP pkgcache_parse_description_raw(SEXP raw);
SEXP pkgcache_parse_description(SEXP path);
SEXP pkgcache_parse_descriptions(SEXP paths, SEXP lowercase);

SEXP pkgcache_parse_packages_raw(SEXP raw);

SEXP c_sql3_set_tempdir(SEXP path);
SEXP c_sql3_open(SEXP filename);
SEXP c_sql3_close(SEXP con);
SEXP c_sql3_prepare(SEXP con, SEXP query);
SEXP c_sql3_exec(SEXP con, SEXP query, SEXP bind, SEXP bindlen);
SEXP c_sql3_get_query(SEXP con, SEXP query, SEXP bind, SEXP bindlen);
SEXP c_sql3_insert(SEXP con, SEXP table, SEXP df, SEXP dflen);
