skip_without_package <- function(package, version = NULL) {
  if (!requireNamespace(package, quietly = TRUE)) {
    skip(paste0("Needs ", package, " package"))
  } else if (!is.null(version) && packageVersion(package) < version) {
    skip(paste0("Needs ", package, ", version ", version, " at least"))
  }
}

get_private <- function(x) x$.__enclos_env__$private

fix_temp_path <- function(x) {
  x <- sub(tempdir(), "<tempdir>", x, fixed = TRUE)
  x <- sub(normalizePath(tempdir()), "<tempdir>", x, fixed = TRUE)
  x <- sub(
    normalizePath(tempdir(), winslash = "/"),
    "<tempdir>",
    x,
    fixed = TRUE
  )
  x <- sub("\\R\\", "/R/", x, fixed = TRUE)
  x <- sub("[\\\\/]file[a-zA-Z0-9]+", "/<tempfile>", x)
  x <- sub("[A-Z]:.*Rtmp[a-zA-Z0-9]+[\\\\/]", "<tempdir>/", x)
  x
}
