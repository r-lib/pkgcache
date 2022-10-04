
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
