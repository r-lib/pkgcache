
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

current_r_platform <- function() {
  type <- get_platform()$pkgType
  if (!is_string(type))
    "source"
  else if (grepl("^mac", type)) {
    "macos"
  } else if (grepl("^win", type)) {
    "windows"
  } else {
    "source"
  }
}

default_platforms <- function() unique(c(current_r_platform(), "source"))

default_cran_mirror <- function() {
  mirror <- getOption("repos")["CRAN"]
  if (is.null(mirror) || is.na(mirror) || mirror == "@CRAN@") {
    "https://cran.rstudio.com"
  } else {
    mirror
  }
}

current_r_version <- function() {
  as.character(getRversion())
}

get_minor_r_version <- function(x) {
  x <- package_version(x)
  vapply(unclass(x), function(x) paste(x[1:2], collapse = "."), character(1))
}

get_all_package_dirs <- function(platforms, rversions) {
  minors <- unique(get_minor_r_version(rversions))
  res <- lapply(platforms, function(pl) {
    if (pl == "source") {
      cbind("source", "*", "src/contrib")

    } else if (pl == "windows") {
      cbind("windows", minors, paste0("bin/windows/contrib/", minors))

    } else if (pl == "macos") {
      res1 <- lapply(minors, function(v) {
        if (package_version(v) <= "2.15") {
          cbind("macos", v, paste0("bin/macosx/leopard/contrib/", v))
        } else if (package_version(v) == "3.0") {
          cbind("macos", v, paste0("bin/macosx/contrib/", v))
        } else if (package_version(v) <= "3.2") {
          cbind("macos", v, paste0(c("bin/macosx/contrib/",
                                     "bin/macosx/mavericks/contrib/"), v))
        } else if (package_version(v) == "3.3") {
          cbind("macos", v, paste0("bin/macosx/mavericks/contrib/", v))
        } else {
          cbind("macos", v, paste0("bin/macosx/el-capitan/contrib/", v))
        }
      })
      do.call(rbind, res1)
    }
  })

  res <- as_tibble(do.call(rbind, res), validate = FALSE)
  colnames(res) <- c("platform", "rversion", "contriburl")
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

is_verbose <- function() {
  env <- Sys.getenv("R_PKG_SHOW_PROGRESS", "")
  if (env != "") {
    tolower(env) == "true"
  } else {
    opt <- getOption("pkg.show_progress")
    if (!is.null(opt)) {
      return(isTRUE(opt))
    } else {
      interactive()
    }
  }
}

msg_wrap <- function(..., .space = TRUE) {
  ret <- paste(strwrap(paste0(...)), collapse = "\n")
  if (.space) ret <- paste0("\n", ret, "\n")
  ret
}
