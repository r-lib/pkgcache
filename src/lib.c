
#include "pkgcache.h"
#include "errors.h"

#include <fcntl.h>
#include <unistd.h>
#include <errno.h>

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

SEXP pkgcache_merge_descriptions(SEXP paths) {
  R_xlen_t i, len = XLENGTH(paths);
  SEXP result = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(result, 1, pkgcache_read_raw(paths));
  SEXP raw = VECTOR_ELT(result, 1);
  R_xlen_t total = 0;

  for (i = 0; i < len; i++) {
    SEXP x = VECTOR_ELT(raw, i);
    if (TYPEOF(x) == RAWSXP) total += XLENGTH(x) + 2;
  }

  SET_VECTOR_ELT(result, 0, allocVector(RAWSXP, total));

  char *ptr = (char*) RAW(VECTOR_ELT(result, 0));
  for (i = 0; i < len; i++) {
    SEXP x = VECTOR_ELT(raw, i);
    if (TYPEOF(x) == RAWSXP) {
      R_xlen_t l = XLENGTH(x);
      memcpy(ptr, RAW(x), l);
      ptr += l;
      ptr[0] = '\n';
      ptr[1] = '\n';
      ptr += 2;
    }
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
    SET_STRING_ELT(result, ridx, Rf_mkCharLen(vl, vlsize));
    SET_STRING_ELT(names, ridx, Rf_mkCharLen(kw, kwsize));
    ridx++;
  }

  Rf_setAttrib(result, R_NamesSymbol, names);
  SEXP final = PROTECT(Rf_lengthgets(result, ridx));

  UNPROTECT(3);
  return final;
}

SEXP pkgcache_read_description(SEXP path) {
  SEXP raw = PROTECT(pkgcache__read_file_raw(CHAR(STRING_ELT(path, 0))));
  SEXP desc = PROTECT(pkgcache_parse_description_raw(raw));

  UNPROTECT(2);
  return desc;
}

struct field_data {
  SEXP cols;
  SEXP nms;
  char *kw;
  int kwsize;
  char *vl;
  int vlsize;
  int totalcols;
  int nfld;
  int npkgs;
  int npkg;
};

static void clean_true_length(struct field_data *data) {
  SEXP *sptr = STRING_PTR(data->nms);
  int i;
  for (i = 0; i < data->nfld; i++) {
    SET_TRUELENGTH(sptr[i], 0);
  }
}

static inline void save_field(struct field_data *data) {
  SEXP key = PROTECT(mkCharLen(data->kw, data->kwsize));
  SEXP val = PROTECT(mkCharLen(data->vl, data->vlsize));

  SEXP keyptr = STRING_PTR(Rf_ScalarString(key))[0];
  int tl = TRUELENGTH(keyptr);
  int idx;

  if (tl < 0) {
    /* Known column */
    idx = - tl - 1;

  } else {
    /* new column */
    if (data->nfld == data->totalcols) {
      clean_true_length(data);
      R_THROW_ERROR("Internal pkgcache error, too many columns");
    }

    SET_STRING_ELT(data->nms, data->nfld, key);
    SEXP col = PROTECT(allocVector(STRSXP, data->npkgs));
    SET_VECTOR_ELT(data->cols, data->nfld, col);
    UNPROTECT(1);

    idx = data->nfld;

    data->nfld += 1;
    SET_TRUELENGTH(keyptr, - data->nfld);
  }

  SET_STRING_ELT(VECTOR_ELT(data->cols, idx), data->npkg, val);

  UNPROTECT(2);
}

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

  struct field_data data;
  data.totalcols = 200;
  data.cols = PROTECT(allocVector(VECSXP, 200));
  data.nms = PROTECT(allocVector(STRSXP, 200));
  data.nfld = 0;
  data.npkgs = npkgs;
  data.npkg = 0;

  p = (char*) RAW(raw);
  while (*p != '\0') {
    switch (state) {

    /* -- at the begining of a package --------------------------------- */
    case S_BG:
      if (*p == '\n') {
        linum++;
        p++;
      } else if (*p == ':' || *p == ' ' || *p == '\t') {
        clean_true_length(&data);
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
        state = S_VL;
      } else if (*p == '\n') {
        clean_true_length(&data);
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
        data.kw = kw;
        data.kwsize = kwsize;
        data.vl = vl;
        data.vlsize = vlsize;
        save_field(&data);

        /* end of package? */
        if (*p == '\n') {
          p++;
          data.npkg++;
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
      clean_true_length(&data);
      R_THROW_ERROR("Internal DESCRIPTION parser error");
      break;
    }
  }

  if (state == S_KW) {
    R_THROW_ERROR("PACKAGES file ended while parsing a key");
  } else {
    data.kw = kw;
    data.kwsize = kwsize;
    data.vl = vl;
    data.vlsize = vlsize;
    save_field(&data);
  }

  clean_true_length(&data);

  /* ------------------------------------------------------------------- */

  p = (char*) RAW(raw);
  p[len - 1] = tail;

  Rf_setAttrib(data.cols, R_NamesSymbol, data.nms);
  SEXP final = PROTECT(Rf_lengthgets(data.cols, data.nfld));
  UNPROTECT(3);
  return final;
}


SEXP pkgcache_read_descriptions(SEXP paths) {


  return R_NilValue;
}
