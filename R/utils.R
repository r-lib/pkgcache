
repoman_data <- new.env(parent = emptyenv())

`%||%` <- function(l, r) if (is.null(l)) r else l

vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, FUN.VALUE = character(1), ...)
}

viapply <- function(X, FUN, ...) {
  vapply(X, FUN, FUN.VALUE = integer(1), ...)
}

vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, FUN.VALUE = logical(1), ...)
}

vdapply <- function(X, FUN, ...) {
  vapply(X, FUN, FUN.VALUE = double(1), ...)
}

mapx <- function(...) {
  args <- list(...)
  if (length(args) == 0) stop("No arguments to `mapx()`")
  fun <- args[[length(args)]]
  if (!is.function(fun)) stop("Last `mapx()` argument not a function")
  if (length(args) == 1) stop("No data to `mapx()`")
  data <- args[-length(args)]

  lens <- setdiff(unique(viapply(data, length)), 1L)
  if (any(lens == 0)) {
    data <- lapply(data, function(x) { length(x) <- 0; x })
    lens <- 0
  }
  if (length(lens) > 1) {
    stop(
      "Incompatible data lengths in `mapx()`: ",
       paste(lens, collapse = ", ")
    )
  }

  do.call(
    mapply,
    c(list(FUN = fun, SIMPLIFY = FALSE, USE.NAMES = FALSE), data)
  )
}

lapply_rows <-  function(df, fun, ...) {
  lapply(seq_len(nrow(df)), function(i) fun(df[i,], ...))
}

zip_vecs <- function(...) {
  mapply(c, ..., SIMPLIFY = FALSE, USE.NAMES = FALSE)
}

add_attr <- function(x, attr, value) {
  attr(x, attr) <- value
  x
}

read.dcf.gz <- function(x) {
  con <- gzfile(x, open = "r")
  on.exit(close(con))
  read.dcf(con)
}

get_cran_extension <- function(platform) {
  switch(
    platform,
    "source" = ".tar.gz",
    "macos" = ".tgz",
    "windows" = ".zip",
    stop("Unknown platform: ", sQuote(platform))
  )
}

get_platform <- function() {
  .Platform
}

get_minor_r_version <- function(x) {
  x <- package_version(x)
  vapply(unclass(x), function(x) paste(x[1:2], collapse = "."), character(1))
}

get_all_package_dirs <- function(platforms, rversions) {
  minors <- unique(get_minor_r_version(rversions))
  if (any(package_version(minors) < "3.2")) {
    stop("pkgcache does not support packages for R versions before R 3.2")
  }
  res <- lapply(platforms, function(pl) {
    if (pl == "source") {
      cbind("source", "*", "src/contrib")

    } else if (pl == "windows") {
      cbind("windows", minors, paste0("bin/windows/contrib/", minors))

    } else if (pl == "macos") {
      res1 <- lapply(minors, function(v) {
        pv <- package_version(v)
        if (pv >= "4.0") {
          cbind("macos", v, paste0("bin/macosx/contrib/", v))
        } else if (package_version(v) <= "3.3") {
          cbind("macos", v, paste0("bin/macosx/mavericks/contrib/", v))
        } else {
          cbind("macos", v, paste0("bin/macosx/el-capitan/contrib/", v))
        }
      })
      do.call(rbind, res1)
    }
  })

  mat <- do.call(rbind, res)
  colnames(mat) <- c("platform", "rversion", "contriburl")
  res <- as_tibble(mat)
  res$prefix <- paste0(
    "/",
    ifelse(res$rversion == "*", "*", paste0("R-", res$rversion)),
    "/", res$platform, "/"
  )

  res
}

read_lines <- function(con, ...) {
  if (is.character(con)) {
    con <- file(con)
    on.exit(close(con))
  }
  readLines(con, ...)
}

dep_types_hard <- function() c("Depends", "Imports", "LinkingTo")
dep_types_soft <- function() c("Suggests", "Enhances")
dep_types <- function() c(dep_types_hard(), dep_types_soft())

interpret_dependencies <- function(dp) {
  hard <- dep_types_hard()

  res <- if (isTRUE(dp)) {
    list(c(hard, "Suggests"), hard)

  } else if (identical(dp, FALSE)) {
    list(character(), character())

  } else if (is_na_scalar(dp)) {
    list(hard, hard)

  } else if (is.list(dp) && all(names(dp) == c("direct", "indirect"))) {
    dp

  } else {
    list(dp, dp)
  }

  names(res) <- c("direct", "indirect")
  res
}

## TODO: in theory the set of base packages can change over time,
## so we would need an R version specific vector here.
## Not an issue currently, might be in the future.

#' @importFrom utils installed.packages

base_packages <- function() {
  if (is.null(repoman_data$base_packages)) {
    repoman_data$base_packages <-
      rownames(installed.packages(priority = "base"))
  }
  repoman_data$base_packages
}

is_na_scalar <- function(x) {
  length(x) == 1 && is.na(x)
}

drop_nulls <- function(x)  {
  x[! vlapply(x, is.null)]
}

null2na <- function(x) {
  x %||% NA_character_
}

na_omit <- function(x) {
  x[!is.na(x)]
}

#' @importFrom digest digest

shasum256 <- function(x) {
  digest(algo = "sha256", file = x)
}

file.size <- function (...) {
  file.info(...)$size
}

msg_wrap <- function(..., .space = TRUE) {
  ret <- paste(strwrap(paste0(...)), collapse = "\n")
  if (.space) ret <- paste0("\n", ret, "\n")
  ret
}

try_catch_null <- function(expr) {
  tryCatch(expr, error = function(e) NULL)
}

is_rcmd_check <- function() {
  if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    FALSE
  } else {
    Sys.getenv("_R_CHECK_PACKAGE_NAME_", "") != ""
  }
}

is_online <- function() {
  if (is_rcmd_check()) return(FALSE)
  curl::has_internet()
}
