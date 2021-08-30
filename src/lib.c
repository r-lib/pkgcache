
#include "pkgcache.h"
#include "errors.h"

#include <fcntl.h>
#include <unistd.h>
#include <errno.h>

#define HASH_SIZE 256
#define MAX_COLL 10

static R_INLINE int hash_string(char *str, int strlen) {
  int backup = str[strlen];
  str[strlen] = '\0';
  unsigned long hash = 5381;
  int c;
  while ((c = *str++)) {
    hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
  }

  str--;
  *str = backup;
  return hash % HASH_SIZE;
}

static SEXP hash_create(int max_cols, int npkgs) {
  SEXP table = PROTECT(allocVector(VECSXP, 4));
  SET_VECTOR_ELT(table, 0, allocVector(STRSXP, max_cols));
  SET_VECTOR_ELT(table, 1, allocVector(INTSXP, HASH_SIZE * MAX_COLL));
  memset(INTEGER(VECTOR_ELT(table, 1)), 0, HASH_SIZE * MAX_COLL * sizeof(int));
  SET_VECTOR_ELT(table, 2, allocVector(INTSXP, 3));
  INTEGER(VECTOR_ELT(table, 2))[0] = 0;
  INTEGER(VECTOR_ELT(table, 2))[1] = max_cols;
  INTEGER(VECTOR_ELT(table, 2))[2] = npkgs;
  SET_VECTOR_ELT(table, 3, allocVector(VECSXP,max_cols));
  UNPROTECT(1);
  return table;
}

static void hash_update(SEXP table, char *key, int keylen, int npkg, SEXP val) {
  SEXP nms = VECTOR_ELT(table, 0);
  SEXP *ptr = STRING_PTR(nms);
  SEXP tab = VECTOR_ELT(table, 1);
  SEXP cols = VECTOR_ELT(table, 3);
  int len = LENGTH(tab);
  int *t = INTEGER(tab);
  int hash = hash_string(key, keylen);
  int start = hash * MAX_COLL;

  for (; start < len; start++) {
    int p = t[start];
    if (p == 0) {
      int *pos = INTEGER(VECTOR_ELT(table, 2));
      if (pos[0] == pos[1]) {
        R_THROW_ERROR("pkgcache internal error, two many columns");
      }
      SET_STRING_ELT(nms, pos[0], Rf_mkCharLenCE(key, keylen, CE_NATIVE));
      SET_VECTOR_ELT(cols, pos[0], allocVector(STRSXP, pos[2]));
      SET_STRING_ELT(VECTOR_ELT(cols, pos[0]), npkg, val);
      t[start] = pos[0] + 1;
      pos[0] ++;
      return;
    } else {
      p--;
      if (!strncmp(key, CHAR(ptr[p]), keylen)) {
        SET_STRING_ELT(VECTOR_ELT(cols, p), npkg, val);
        return;
      }
    }
  }

  R_THROW_ERROR("pkgcache internal hash table is full, please report a bug");
}

static SEXP hash_getnames(SEXP table) {
  return VECTOR_ELT(table, 0);
}

/* --------------------------------------------------------------------- */

SEXP pkgcache__read_file_raw(const char *cpath) {
  SEXP result = R_NilValue;
  int err;
  int fd = open(cpath, O_RDONLY);

  if (fd == -1) {
    return(R_FORMAT_SYSTEM_ERROR("Cannot oepn file `%s`", cpath));
  }

  off_t len = lseek(fd, 0, SEEK_END);
  if (len == -1) {
    err = errno;
    close(fd);
    return R_FORMAT_SYSTEM_ERROR_CODE(err, "Cannot open `%s`", cpath);
  }
  off_t len2 = lseek(fd, 0, SEEK_SET);
  if (len2 == -1) {
    err = errno;
    close(fd);
    return R_FORMAT_SYSTEM_ERROR_CODE(err, "Cannot seek `%s`", cpath);
  }

  /* TODO: should use cleancall to close the file if allocVector fails */

  result = PROTECT(allocVector(RAWSXP, len));

  ssize_t ret = read(fd, RAW(result), len);
  if (ret == -1) {
    err = errno;
    close(fd);
    UNPROTECT(1);
    return R_FORMAT_SYSTEM_ERROR_CODE(err, "Cannot read `%s`", cpath);
  }

  close(fd);

  UNPROTECT(1);
  return result;
}

/* --------------------------------------------------------------------- */

SEXP pkgcache_read_raw(SEXP paths) {
  R_xlen_t i, len = XLENGTH(paths);
  SEXP result = PROTECT(allocVector(VECSXP, len));

  for (i = 0; i < len; i++) {
    SET_VECTOR_ELT(
      result,
      i,
      pkgcache__read_file_raw(CHAR(STRING_ELT(paths, i)))
    );
  }

  UNPROTECT(1);
  return result;
}

/* --------------------------------------------------------------------- */

#define S_BG 0                  /* beginning of the file */
#define S_KW 1                  /* inside a keyword */
#define S_VL 2                  /* inside a value */
#define S_NL 3                  /* right after a newline */
#define S_WS 4                  /* after newline + space */

SEXP pkgcache_parse_description_raw(SEXP raw) {
  char *p = NULL, *start = (char*) RAW(raw);
  char *end = start + XLENGTH(raw);
  int state = S_BG;
  char *kw = NULL, *vl = NULL;
  int kwsize = 0, vlsize = 0;
  int linum = 1;

  SEXP result = PROTECT(allocVector(STRSXP, 200));
  SEXP names = PROTECT(allocVector(STRSXP, 200));
  int ridx = 0;

  for (p = start; p < end; ) {
    switch (state) {

    /* -- at the begining ---------------------------------------------- */
    case S_BG:
      if (*p == ':' || *p == '\n' || *p == ' ' || *p == '\t') {
        R_THROW_ERROR(
          "Invalid DESCRIPTION file, must start with an "
          "alphanumeric character"
        );
      }
      /* Otherwise it must be the start of a keyword */
      kw = p++;
      state = S_KW;

      break;

    /* -- within a keyword --------------------------------------------- */
    case S_KW:
      /* Maybe the keyword ends here, and a value starts */
      if (*p == ':') {
        kwsize = p - kw;
        p++;
        vl = p;
        if (*vl == ' ') vl++;
        state = S_VL;

      /* A newline within a keyword is an error */
      } else if (*p == '\n') {
        R_THROW_ERROR(
          "Invalid line (%d) in DESCRIPTION: must contain `:`",
          linum
        );

      /* Otherwise we are inside the keyword */
      } else {
        p++;
      }

      break;

    /* --- within a value ---------------------------------------------- */
    case S_VL:
      /* newline might be the end of the value, if no continuation. */
      if (*p == '\n') {
        state = S_NL;
        vlsize = p - vl;
        p++;
        linum++;

      } else {
        p++;
      }
      break;

    /* -- right after a newline ---------------------------------------- */
    case S_NL:
      /* maybe a continuation line */
      if (*p == ' ' || *p == '\t') {
        state = S_WS;
        p++;

      /* othewise we can save the field, and start parsing the next one */
      } else {
        SET_STRING_ELT(result, ridx, Rf_mkCharLen(vl, vlsize));
        SET_STRING_ELT(names, ridx, Rf_mkCharLen(kw, kwsize));
        ridx++;
        kw = p;
        state = S_KW;
        p++;
      }

      break;

    /* -- after continuation space ------------------------------------- */
    case S_WS:
      /* more whitespace? */
      if (*p == ' ' || *p == '\t') {
        p++;

      /* otherwise continuation line, so this is still the value */
      } else {
        state = S_VL;
        p++;
      }
      break;

    /* ----------------------------------------------------------------- */
    default:
      R_THROW_ERROR("Internal DESCRIPTION parser error");
      break;
    }
  }

  if (state == S_KW) {
    R_THROW_ERROR("DESCRIPTION file ended while parsing a key");
  } else {
    vlsize = p - vl;
    SET_STRING_ELT(result, ridx, Rf_mkCharLen(vl, vlsize));
    SET_STRING_ELT(names, ridx, Rf_mkCharLen(kw, kwsize));
    ridx++;
  }

  Rf_setAttrib(result, R_NamesSymbol, names);
  SEXP final = PROTECT(Rf_lengthgets(result, ridx));

  UNPROTECT(3);
  return final;
}

/* --------------------------------------------------------------------- */

SEXP pkgcache_parse_description(SEXP path) {
  SEXP raw = PROTECT(pkgcache__read_file_raw(CHAR(STRING_ELT(path, 0))));
  SEXP desc = PROTECT(pkgcache_parse_description_raw(raw));

  UNPROTECT(2);
  return desc;
}

/* --------------------------------------------------------------------- */

SEXP pkgcache_parse_packages_raw(SEXP raw) {
  int len = LENGTH(raw);
  char *p = NULL;
  int npkgs = 1;

  /* ------------------------------------------------------------------- */
  /* Count number of empty lines, to guess the number of packages */
  p = (char*) RAW(raw);
  char tail = p[len - 1];
  p[len - 1] = '\0';

  /* This is faster than manual search, because strchr is optimized.
     It is also faster than strstr, for this special case of a two
     character pattern. */

  for (;;) {
    p = strchr(p, '\n');
    if (p == NULL) break;
    p++;
    if (*p == '\n') {
      p++;
      npkgs++;
    }
  }

  /* ------------------------------------------------------------------- */

  int state = S_BG;
  char *kw = NULL, *vl = NULL;
  int kwsize = 0, vlsize = 0;
  int linum = 1;

  SEXP table = PROTECT(hash_create(100, npkgs));
  int npkg = 0;

  p = (char*) RAW(raw);
  while (*p != '\0') {
    switch (state) {

    /* -- at the begining of a package --------------------------------- */
    case S_BG:
      if (*p == '\n') {
        linum++;
        p++;
      } else if (*p == ':' || *p == ' ' || *p == '\t') {
        R_THROW_ERROR(
          "Invalid PACKAGES file in line %d: expected keyword",
          linum
        );
      } else {
        kw = p++;
        state = S_KW;
      }
      break;

    /* -- within a keyword --------------------------------------------- */
    case S_KW:
      if (*p == ':') {
        kwsize = p - kw;
        p++;
        vl = p;
        if (*vl == ' ') vl++;   /* skip leading space */
        state = S_VL;

      } else if (*p == '\n') {
        R_THROW_ERROR(
          "Invalid line (%d) in PACKAGES file: must contain `:`",
          linum
        );

      } else {
        p++;
      }

      break;

    /* --- within a value ---------------------------------------------- */
    case S_VL:
      /* newline might be the end of the value, if no continuation. */
      if (*p == '\n') {
        state = S_NL;
        vlsize = p - vl;
        p++;
        linum++;

      } else {
        p++;
      }
      break;

    /* -- right after a newline ---------------------------------------- */
    case S_NL:
      /* maybe a continuation line */
      if (*p == ' ' || *p == '\t') {
        state = S_WS;
        p++;

      /* end of field */
      } else {
        /* Save field */
        SEXP val = PROTECT(mkCharLenCE(vl, vlsize, CE_BYTES));
        hash_update(table, kw, kwsize, npkg, val);
        UNPROTECT(1);

        /* end of package? */
        if (*p == '\n') {
          p++;
          npkg++;
          linum++;
          state = S_BG;

        /* or just a new key */
        } else {
          kw = p;
          p++;
          state = S_KW;
        }
      }

      break;

    /* -- after continuation space ------------------------------------- */
    case S_WS:
      /* more whitespace? */
      if (*p == ' ' || *p == '\t') {
        p++;

      /* otherwise continuation line, so this is still the value */
      } else {
        state = S_VL;
        p++;
      }

      break;

    /* ----------------------------------------------------------------- */
    default:
      R_THROW_ERROR("Internal PACKAGES parser error");
      break;
    }
  }

  vlsize = p - vl;
  p = (char*) RAW(raw);
  p[len - 1] = tail;
  if (state == S_VL && tail != '\n') vlsize++;

  if (state == S_KW) {
    R_THROW_ERROR("PACKAGES file ended while parsing a key");
  } else {
    /* Save field */
    SEXP val = PROTECT(mkCharLenCE(vl, vlsize, CE_BYTES));
    hash_update(table, kw, kwsize, npkg, val);
    UNPROTECT(1);
  }

  /* ------------------------------------------------------------------- */

  Rf_setAttrib(VECTOR_ELT(table, 3), R_NamesSymbol, VECTOR_ELT(table, 0));
  SEXP final = PROTECT(Rf_lengthgets(
    VECTOR_ELT(table, 3),
    INTEGER(VECTOR_ELT(table, 2))[0]
  ));
  UNPROTECT(2);
  return final;
}

/* --------------------------------------------------------------------- */

SEXP pkgcache_parse_descriptions(SEXP paths) {
  int npkg, npkgs = LENGTH(paths);

  int state = S_BG;
  char *kw = NULL, *vl = NULL;
  int kwsize = 0, vlsize = 0;
  int linum = 1;

  for (npkg = 0; npkg < npkgs; npkg++) {

    const char *cpath = CHAR(STRING_ELT(paths, npkg));
    SEXP raw = PROTECT(pkgcache__read_file_raw(cpath));
    if (TYPEOF(raw) != RAWSXP) {
      /* TODO: error or warning? */
      UNPROTECT(1);
      continue;
    }

    state = S_BG;
    kw = NULL;
    vl = NULL;
    kwsize = 0;
    vlsize = 0;
    linum = 1;

    int len = LENGTH(raw);
    char *p = (char*) RAW(raw);
    char tail = p[len - 1];
    p[len - 1] = '\0';

    while (*p != '\0') {
      switch(state) {
      /* -- at the begining -------------------------------------------- */
      case S_BG:
        if (*p == ':' || *p == '\n' || *p == ' ' || *p == '\t') {
          R_THROW_ERROR(
            "Invalid DESCRIPTION file `%s` , must start with an "
            "alphanumeric character",
            cpath
          );
        }
        /* Otherwise it must be the start of a keyword */
        kw = p++;
        state = S_KW;

        break;

      /* -- within a keyword ------------------------------------------- */
      case S_KW:
        /* Maybe the keyword ends here, and a value starts */
        if (*p == ':') {
          kwsize = p - kw;
          p++;
          vl = p;
          if (*vl == ' ') vl++;
          state = S_VL;

        /* A newline within a keyword is an error */
        } else if (*p == '\n') {
          R_THROW_ERROR(
            "Invalid line (%d) in DESCRIPTION file `%s`: must contain `:`",
            linum, cpath
          );

        /* Otherwise we are inside the keyword */
        } else {
          p++;
        }

        break;

      /* --- within a value -------------------------------------------- */
      case S_VL:
        if (*p == '\n') {
          state = S_NL;
          vlsize = p - vl;
          p++;
          linum++;

        } else {
          p++;
        }

        break;

      /* -- right after a newline -------------------------------------- */
      case S_NL:
        /* maybe a continuation line */
        if (*p == ' ' || *p == '\t') {
          state = S_WS;
          p++;

        /* othewise we can save the field, and start parsing the next one */
        } else {

          /* todo: save field */
          kw = p;
          state = S_KW;
          p++;
        }

        break;

      /* -- after continuation space ----------------------------------- */
      case S_WS:
        /* more whitespace? */
        if (*p == ' ' || *p == '\t') {
          p++;

        /* otherwise continuation line, so this is still the value */
        } else {
          state = S_VL;
          p++;
        }

        break;

      /* --------------------------------------------------------------- */
      default:
        R_THROW_ERROR("Internal DESCRIPTION parser error");
        break;
      }
    }

    vlsize = p - vl;
    p = (char*) RAW(raw);
    p[len - 1] = tail;
    if (state == S_VL && tail != '\n') vlsize++;

    if (state == S_KW) {
      R_THROW_ERROR("DESCRIPTION file `%s` ended while parsing a key", cpath);
    } else {
      /* todo: save field */
    }

    UNPROTECT(1);
  }


  /* Rf_setAttrib(data.cols, R_NamesSymbol, data.nms); */
  /* SEXP final = PROTECT(Rf_lengthgets(data.cols, data.nfld)); */
  UNPROTECT(2);
  return R_NilValue;
}
