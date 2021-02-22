
#' @useDynLib pkgcache, .registration = TRUE, .fixes = "c_"
NULL

os_type <- function() {
  .Platform$OS.type
}

encode_path <- function(path) {
  if (os_type() == "unix") {
    enc2native(path)
  } else {
    enc2utf8(path)
  }
}

sql3_open <- function(path) {
  stopifnot(is_string(path))
  path <- encode_path(path)
  .Call(c_sql3_open, path)
}

sql3_close <- function(con) {
  stopifnot(is_sql3_connection(con))
  .Call(c_sql3_close, con)
}

sql3_prepare <- function(con, query) {
  stopifnot(
    is_sql3_connection(con),
    is_string(query)
  )
  query <- enc2utf8(query)
  .Call(c_sql3_prepare, con, query)
}

sql3_exec <- function(con, query, bind = list()) {
  stopifnot(
    is_sql3_connection(con),
    is_prepared(query) || is_string(query),
    is_bind_list(bind)
  )
  if (is.character(query)) query <- enc2utf8(query)
  len <- bind_length(bind)
  .Call(c_sql3_exec, con, query, bind, len)
}

sql3_get_query <- function(con, query, bind = list()) {
  stopifnot(
    is_sql3_connection(con),
    is_prepared(query) || is_string(query),
    is_bind_list(bind)
  )
  if (is.character(query)) query <- enc2utf8(query)
  len <- bind_length(bind)
  if (len > 1) {
    stop("Cannot bind multiple sets of values in `sql3_get_query()`.")
  }
  .Call(c_sql3_get_query, con, query, bind, len)
}

sql3_insert <- function(con, table, df) {
  stopifnot(
    is_sql3_connection(con),
    is_string(table),
    is.data.frame(df)
  )
  table <- enc2utf8(table)
  len <- bind_length(bind)
  .Call(c_sql3_insert, con, table, df, len)
}

sql3_transaction <- function(con, expr) {

}

is_sql3_connection <- function(x) {
  inherits(x, "sqlite3_connection")
}

is_prepared <- function(x) {
  inherits(x, "sqlite3_prepared_statement")
}

# Right now we don't check for duplicate names, to gain some performance.
# If a name appears multiple times, the last one wins.

is_bind_list <- function(x) {
  is.list(x)
}

bind_length <- function(x) {
  ls <- vapply(x, length, integer(1))
  uls <- unique(ls)
  luls <- length(uls)
  if (luls == 1) return(uls)
  uls <- setdiff(uls, 1L)
  if (length(uls) > 1L) {
    stop("Invalid bind data length, must contain vectors of the same ",
         "length, and scalars")
  }
  if (length(uls)) uls else 0L
}

onload_sql3 <- function(libname, pkgname) {
  .Call(c_sql3_set_tempdir, enc2utf8(tempdir()))
}

if (exists(".onLoad", inherits = FALSE)) {
  onload_old_sql3 <- .onLoad
  .onLoad <- function(libname, pkgname) {
    onload_old_sql3(libname, pkgname)
    onload_sql3(libname, pkgname)
  }
} else {
  .onLoad <- onload_sql3
}
