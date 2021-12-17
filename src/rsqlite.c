
#include <R.h>
#include <Rinternals.h>

#include <string.h>
#include <ctype.h>

#include "sqlite3.h"

static char *tempdir = NULL;
static char sqlite3_error[1024];

SEXP c_sql3_set_tempdir(SEXP path) {
  if (tempdir) error("sqlite3 temporary directory is already set");
  tempdir = strdup(CHAR(STRING_ELT(path, 0)));
  if (!tempdir) error("Not enough memory for sqlite3 tempdir name");
  sqlite3_temp_directory = tempdir;
  return R_NilValue;
}

static void sql3_finalizer(SEXP con) {
  sqlite3 *ccon = R_ExternalPtrAddr(con);
  if (ccon) {
    R_ClearExternalPtr(con);
    sqlite3_close(ccon);
  }
}

SEXP c_sql3_open(SEXP filename) {
  sqlite3 *ccon = NULL;
  int ret = sqlite3_open(CHAR(STRING_ELT(filename, 0)), &ccon);
  if (ret != SQLITE_OK) {
    if (!ccon) error("Failed to open sqlite3 database, out of memory");
    strncpy(sqlite3_error, sqlite3_errmsg(ccon), sizeof(sqlite3_error) - 1);
    sqlite3_close(ccon);
    error("Failed to open sqlite3: %s", sqlite3_error);
  }

  SEXP xptr = PROTECT(R_MakeExternalPtr(ccon, R_NilValue, R_NilValue));
  R_RegisterCFinalizerEx(xptr, sql3_finalizer, /* onexit= */ 1);
  setAttrib(xptr, R_ClassSymbol, mkString("sqlite3_connection"));

  UNPROTECT(1);
  return xptr;
}

SEXP c_sql3_close(SEXP con) {
  sqlite3 *ccon = R_ExternalPtrAddr(con);
  if (!ccon) error("sqlite3 connection already closed");
  R_ClearExternalPtr(con);
  sqlite3_close(ccon);
  return R_NilValue;
}

static void warn_for_tail(const char *tail) {
  while (isspace(*tail)) tail++;
  if (*tail) warning("Ignoring remainder of SQL statement: '%s'", tail);
}

void sql3_finalizer_stmt(SEXP xptr) {
  sqlite3_stmt *stmt = R_ExternalPtrAddr(xptr);
  if (stmt) {
    R_ClearExternalPtr(xptr);
    sqlite3_finalize(stmt);
  }
}

SEXP c_sql3_prepare(SEXP con, SEXP query) {
  sqlite3 *ccon = R_ExternalPtrAddr(con);
  sqlite3_stmt *stmt = NULL;
  const char *cquery = CHAR(STRING_ELT(query, 0));
  const char *tail = NULL;

  if (!ccon) error("sqlite3 connection already closed");

  int ret = sqlite3_prepare_v3(
    ccon,
    cquery,
    -1,
    SQLITE_PREPARE_PERSISTENT,
    &stmt,
    &tail);

  if (ret != SQLITE_OK) {
    error("sqlite prepare error: '%s'", sqlite3_errmsg(ccon));
  }

  if (!stmt) {
    error("sqlite statemt has no query: '%s'", cquery);
  }

  warn_for_tail(tail);

  SEXP xptr = PROTECT(R_MakeExternalPtr(stmt, R_NilValue, R_NilValue));
  R_RegisterCFinalizerEx(xptr, sql3_finalizer_stmt, /* onexit= */ 1);
  setAttrib(xptr, R_ClassSymbol, mkString("sqlite3_prepared_statement"));

  UNPROTECT(1);
  return xptr;
}

SEXP sql3_exec_noparam(sqlite3 *ccon, sqlite3_stmt *stmt) {
  int ret = sqlite3_step(stmt);
  if (ret != SQLITE_DONE && ret != SQLITE_ROW) {
    error("sqlite query error: '%s'", sqlite3_errmsg(ccon));
  }

  return R_NilValue;
}

SEXP sql3_exec_generic(SEXP con, SEXP query, SEXP bind, SEXP bindlen) {
  sqlite3 *ccon = R_ExternalPtrAddr(con);
  int cbindlen = INTEGER(bindlen)[0];
  if (!ccon) error("sqlite3 connection already closed");

  sqlite3_stmt *stmt = R_ExternalPtrAddr(query);
  if (!stmt) error("Prepared statement already finalized");

  sqlite3_reset(stmt);

  int nparams = sqlite3_bind_parameter_count(stmt);
  int numbind = LENGTH(bind);

  if (nparams > numbind) {
    error("Not enough bind parameters for sqlite3 query");
  }
  if (nparams < numbind) {
    error("Too many bind parameters for sqlite3 query");
  }

  /* If there are no parameters, then just run this once */
  if (nparams == 0) {
    SEXP result = sql3_exec_noparam(ccon, stmt);
    return result;
  }

  /* TODO: make this configurable? */
  sqlite3_clear_bindings(stmt);

  /* TODO: handle names */
  SEXP nms = PROTECT(getAttrib(bind, R_NamesSymbol));

  int ret;
  for (int r = 0; r < cbindlen; r++) {
    int lv, iv;
    double rv;
    SEXP sv;
    int bret;
    for (int i = 0; i < numbind; i++) {
      SEXP x = VECTOR_ELT(bind, i);
      SEXPTYPE t = TYPEOF(x);
      switch (t) {
      case LGLSXP:
        lv = LOGICAL(x)[r];
        if (lv == NA_LOGICAL) {
          bret = sqlite3_bind_null(stmt, i + 1);
        } else {
          bret = sqlite3_bind_int(stmt, i + 1, lv);
        }
        break;
      case INTSXP:
        iv = INTEGER(x)[r];
        if (iv == NA_INTEGER) {
          bret = sqlite3_bind_null(stmt, i + 1);
        } else {
          bret = sqlite3_bind_int(stmt, i + 1, iv);
        }
        break;
      case REALSXP:
        rv = REAL(x)[r];
        if (iv == NA_REAL) {
          bret = sqlite3_bind_null(stmt, i + 1);
        } else {
          bret = sqlite3_bind_double(stmt, i + 1, rv);
        }
        break;
      case STRSXP:
        sv = STRING_ELT(x, r);
        if (sv == NA_STRING) {
          bret = sqlite3_bind_null(stmt, i + 1);
        } else {
          bret = sqlite3_bind_text(stmt, i + 1, CHAR(sv), -1, SQLITE_TRANSIENT);
        }
        break;
      case VECSXP:
        /* TODO */
      default:
        error("Cannot bind type %s", Rf_type2char(t));
      }

      if (bret != SQLITE_OK) {
        error("sqlite bind error: '%s'", sqlite3_errmsg(ccon));
      }
    }

    ret = sqlite3_step(stmt);
    if (ret != SQLITE_ROW && ret != SQLITE_DONE) {
      error("sqlite query error: '%s'", sqlite3_errmsg(ccon));
    }
  }

  UNPROTECT(1);                 /* nms */

  if (ret == SQLITE_ROW) {
    return query;
  } else {
    return R_NilValue;
  }
}

SEXP c_sql3_exec(SEXP con, SEXP query, SEXP bind, SEXP bindlen) {
  int prepped = TYPEOF(query) == EXTPTRSXP;
  if (!prepped) {
    query = PROTECT(c_sql3_prepare(con, query));
  }

  sql3_exec_generic(con, query, bind, bindlen);

  if (!prepped) {
    sql3_finalizer_stmt(query);
    UNPROTECT(1);
  }

  return R_NilValue;
}

SEXP c_sql3_get_query(SEXP con, SEXP query, SEXP bind, SEXP bindlen) {
  int prepped = TYPEOF(query) == EXTPTRSXP;
  if (!prepped) {
    query = PROTECT(c_sql3_prepare(con, query));
  }

  if (!prepped) {
    sql3_finalizer_stmt(query);
    UNPROTECT(1);
  }

  return sql3_exec_generic(con, query, bind, bindlen);
}

SEXP c_sql3_insert(SEXP con, SEXP table, SEXP df, SEXP dflen) {
  /* TODO */
  return R_NilValue;
}
