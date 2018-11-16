
if (!file.exists("../async")) {
  stop("Cannot find async in '../async'")
}

## R files

unlink("R/aaa-async.R")
files <- list.files("../async/R", full.names = TRUE)
for (f in files) file.append("R/aaa-async.R", f)

## test files

unlink("tests/async", recursive = TRUE)
fs::dir_copy("../async/tests/testthat", "tests/async")

## (re)generate docs

devtools::document()
