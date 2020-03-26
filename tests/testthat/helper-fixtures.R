
fixtures <- list(
  ## Maybe later
)
  
fixture_dir <- function() {
  ## If run from R CMD check, it might give an error,
  ## so fall back to the current directory being tests/testthat
  tryCatch(
    file.path(
      rprojroot::find_package_root_file(),
      "tests", "testthat", "fixtures"
    ),
    error = function(e) "fixtures"
  )
}

get_fixture <- function(file) {
  file.path(fixture_dir(), file)
}

read_fixture <- function(file) {
  readRDS(get_fixture(file))
}

update_fixtures <- function(files = NULL) {
  if (is.null(files)) files <- names(fixtures)
  fdir <- fixture_dir()
  for (f in files) {
    output <- file.path(fdir, f)
    cat(output, sep = "\n")
    saveRDS(fixtures[[f]](), file = output, version = 2)
  }
}
