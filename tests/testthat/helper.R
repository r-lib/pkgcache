
check_packages_data <- function(pkgs) {
  cols <- packages_gz_cols()
  p_cols <- cols$pkgs
  d_cols <- cols$deps

  expect_equal(length(pkgs), 2)
  miss <- setdiff(p_cols, names(pkgs$pkgs))
  expect_identical(miss, character(0))

  miss2 <- setdiff(d_cols, names(pkgs$deps))
  expect_identical(miss2, character(0))
  expect_true(is.integer(pkgs$deps$idx))
  pkgs$deps$idx <- as.character(pkgs$deps$idx)
  expect_true(all(vlapply(pkgs$deps, is.character)))
}

get_private <- function(x) x$.__enclos_env__$private

`set_private<-` <- function(x, member, value) {
  pr <- get_private(x)
  pr[[member]] <- value
  invisible(x)
}

oneday <- function() as.difftime(1, units = "days")

oneminute <- function() as.difftime(1, units = "mins")

test_temp_file <- function(fileext = "", pattern = "test-file-",
                           envir = parent.frame(), create = TRUE) {
  tmp <- tempfile(pattern = pattern, fileext = fileext)
  if (identical(envir, .GlobalEnv)) {
    message("Temporary files will _not_ be cleaned up")
  } else {
    withr::defer(
      try(unlink(tmp, recursive = TRUE, force = TRUE), silent = TRUE),
      envir = envir)
  }
  if (create) {
    cat("", file = tmp)
    normalizePath(tmp)
  } else {
    tmp
  }
}

test_temp_dir <- function(pattern = "test-dir-", envir = parent.frame()) {
  tmp <- test_temp_file(pattern = pattern, envir = envir, create = FALSE)
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  normalizePath(tmp)
}

touch <- function(path) {
  mkdirp(dirname(path))
  cat("", file = path)
}

fix_port_number <- function(x) {
  gsub(":[0-9]+", ":3000", x)
}

fix_mtime <- function(x) {
  gsub(
    "\\d\\d\\d\\d[-]\\d\\d[-]\\d\\d \\d\\d:\\d\\d:\\d\\d",
    "2022-09-14 13:28:34",
    x
  )
}
