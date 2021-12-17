
#include "pkgcache.h"
#include "cleancall.h"

#include <R_ext/Rdynload.h>

#ifdef GCOV_COMPILE

void __gcov_dump(void);
SEXP pkgcache__gcov_flush(void) {
  REprintf("Flushing coverage info\n");
  (void) __gcov_dump();
  return R_NilValue;
}

#else

SEXP pkgcache__gcov_flush(void) {
  return R_NilValue;
}

#endif

#define REG(name, args) { #name, (DL_FUNC) name, args }

static const R_CallMethodDef callMethods[]  = {
  CLEANCALL_METHOD_RECORD,

  REG(c_sql3_set_tempdir,             1),
  REG(c_sql3_open,                    1),
  REG(c_sql3_close,                   1),
  REG(c_sql3_prepare,                 2),
  REG(c_sql3_exec,                    4),
  REG(c_sql3_get_query,               4),
  REG(c_sql3_insert,                  4),

  REG(pkgcache_read_raw,              1),
  REG(pkgcache_parse_description_raw, 1),
  REG(pkgcache_parse_description,     1),
  REG(pkgcache_parse_descriptions,    2),
  REG(pkgcache_parse_packages_raw,    1),

  REG(pkgcache__gcov_flush,           0),
  { NULL, NULL, 0 }
};

void R_init_pkgcache(DllInfo *dll) {
  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
  cleancall_fns_dot_call = Rf_findVar(Rf_install(".Call"), R_BaseEnv);
}
