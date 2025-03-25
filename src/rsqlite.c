
#include <R.h>
#include <Rinternals.h>

#include <string.h>
#include <ctype.h>

#include "sqlite3.h"
#include "errors.h"

static char *tempdir = NULL;

#define SQL3_CLOSE_ERROR(sql, code) do {                        \
    sqlite3_close_v2(sql);                                      \
    R_THROW_ERROR("sqlite3 error: %s", sqlite3_errstr(ret));    \
  } while (0)

#define SQL3_CLOSE_ERROR_MSG(sql, msg) do {                     \
    sqlite3_close_v2(sql);                                      \
    R_THROW_ERROR("sqlite3 error: %s", msg);                    \
  } while (0)

SEXP c_sql3_set_tempdir(SEXP path) {
  if (tempdir) error("sqlite3 temporary directory is already set");
  tempdir = strdup(CHAR(STRING_ELT(path, 0)));
  if (!tempdir) error("Not enough memory for sqlite3 tempdir name");
  sqlite3_temp_directory = tempdir;
  return R_NilValue;
}

void sql3_create_db(const char *path, const char *tbl_package) {
  sqlite3 *sql;
  char *errmsg;
  int ret;

  const char *idx_package =
    "CREATE INDEX idx_package ON package(   \n\
      package,                              \n\
      platform,                             \n\
      rversion,                             \n\
      mirror                                \n\
    );";

  const char *tbl_dep =
    "CREATE TABLE dep (                     \n\
       upstream TEXT,                       \n\
       ref TEXT,                            \n\
       type TEXT,                           \n\
       package TEXT,                        \n\
       op TEXT,                             \n\
       version TEXT                         \n\
    );";

  const char *idx_dep =
    "CREATE INDEX idx_dep ON dep(           \n\
       upstream,                            \n\
       ref,                                 \n\
       type,                                \n\
       package                              \n\
    );";

  const char *tbl_meta =
    "CREATE TABLE meta (                    \n\
       mirror TEXT,                         \n\
       updated TEXT,                        \n\
       etag TEXT                            \n\
    );";

  if ((ret = sqlite3_open(path, &sql)) != SQLITE_OK) {
    SQL3_CLOSE_ERROR(sql, ret);
  }

  if ((ret = sqlite3_exec(sql, tbl_package, NULL, NULL, &errmsg)) != SQLITE_OK) {
    SQL3_CLOSE_ERROR_MSG(sql, errmsg);
  }
  if ((ret = sqlite3_exec(sql, idx_package, NULL, NULL, &errmsg)) != SQLITE_OK) {
    SQL3_CLOSE_ERROR_MSG(sql, errmsg);
  }
  if ((ret = sqlite3_exec(sql, tbl_dep, NULL, NULL, &errmsg)) != SQLITE_OK) {
    SQL3_CLOSE_ERROR_MSG(sql, errmsg);
  }
  if ((ret = sqlite3_exec(sql, idx_dep, NULL, NULL, &errmsg)) != SQLITE_OK) {
    SQL3_CLOSE_ERROR_MSG(sql, errmsg);
  }
  if ((ret = sqlite3_exec(sql, tbl_meta, NULL, NULL, &errmsg)) != SQLITE_OK) {
    SQL3_CLOSE_ERROR_MSG(sql, errmsg);
  }

  sqlite3_close_v2(sql);
}

SEXP c_sql3_create_db(SEXP path, SEXP tblq) {
  sql3_create_db(CHAR(STRING_ELT(path, 0)), CHAR(STRING_ELT(tblq, 0)));
  return R_NilValue;
}

SEXP c_sql3_add_packages(SEXP path, SEXP packages, SEXP query) {
  const char *cpath = CHAR(STRING_ELT(path, 0));
  const char *cquery = CHAR(STRING_ELT(query, 0));
  int nr = Rf_length(VECTOR_ELT(packages, 0)), nc = Rf_length(packages);
  sqlite3 *sql;
  sqlite3_stmt *stmt;
  int ret;
  int i, j;
  char *errmsg;

  if ((ret = sqlite3_open(cpath, &sql)) != SQLITE_OK) {
    SQL3_CLOSE_ERROR(sql, ret);
  }

  if ((ret = sqlite3_exec(sql, "BEGIN", NULL, NULL, &errmsg)) != SQLITE_OK) {
    SQL3_CLOSE_ERROR_MSG(sql, errmsg);
  }

  if ((ret = sqlite3_prepare_v2(sql, cquery, -1, &stmt, 0)) != SQLITE_OK) {
    SQL3_CLOSE_ERROR(sql, ret);
  }

  for (i = 0; i < nr; i++) {
    for (j = 0; j < nc; j++) {
      SEXP col = VECTOR_ELT(packages, j);
      if (Rf_isString(col)) {
        SEXP el = STRING_ELT(col, i);
        if (el == R_NaString) {
          sqlite3_bind_null(stmt, j + 1);
        } else {
          sqlite3_bind_text(stmt, j + 1, CHAR(el), -1, SQLITE_STATIC);
        }
      } else if (Rf_isLogical(col)) {
        int el = LOGICAL(col)[i];
        if (el == NA_LOGICAL) {
          sqlite3_bind_null(stmt, j + 1);
        } else {
          sqlite3_bind_int(stmt, j + 1, el);
        }
      } else {
        sqlite3_finalize(stmt);
        sqlite3_close_v2(sql);
        R_THROW_ERROR("Unknown column type in pkgcache DB");
      }
    }

    if ((ret = sqlite3_step(stmt)) != SQLITE_DONE) {
      sqlite3_finalize(stmt);
      SQL3_CLOSE_ERROR(sql, ret);
    }

    if ((ret = sqlite3_reset(stmt)) != SQLITE_OK) {
      sqlite3_finalize(stmt);
      SQL3_CLOSE_ERROR(sql, ret);
    }
  }

  if ((ret = sqlite3_exec(sql, "COMMIT", NULL, NULL, &errmsg)) != SQLITE_OK) {
    sqlite3_finalize(stmt);
    SQL3_CLOSE_ERROR_MSG(sql, errmsg);
  }

  sqlite3_finalize(stmt);
  sqlite3_close_v2(sql);

  return R_NilValue;
}
