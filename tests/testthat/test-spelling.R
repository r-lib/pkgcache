
test_package_root <- function() {
  x <- tryCatch(
    rprojroot::find_package_root_file(),
    error = function(e) NULL)

  if (!is.null(x)) return(x)

  pkg <- testthat::testing_package()
  x <- tryCatch(
    rprojroot::find_package_root_file(
      path = file.path("..", "..", "00_pkg_src", pkg)),
    error = function(e) NULL)

  if (!is.null(x)) return(x)

  stop("Cannot find package root")
}

test_that("spell check", {
  skip_on_cran()
  skip_on_covr()
  pkg_dir <- test_package_root()
  results <- spelling::spell_check_package(pkg_dir)

  if (nrow(results)) {
    output <- sprintf(
      "Potential spelling errors: %s\n",
      paste(results$word, collapse = ", "))
    stop(output, call. = FALSE)
  } else {
    expect_true(TRUE)
  }
})
