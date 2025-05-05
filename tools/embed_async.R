if (!file.exists("../async")) {
  stop("Cannot find async in '../async'")
}

## R files

unlink("R/aaa-async.R")
files <- list.files("../async/R", full.names = TRUE)
# We don't need this, we have our own copy, which is slightly different
files <- files[!grepl("aa-assertthat.R", files)]
for (f in files) file.append("R/aaa-async.R", f)

## Do not export anything

l <- readLines("R/aaa-async.R")
l <- gsub("@export", "@noRd", l)
l <- gsub("@name async_debug", "@name async_debug\n#' @noRd", l)
l <- gsub("@name deferred", "@name deferred\n#' @noRd", l)
l <- gsub("@name worker_pool", "@name worker_pool\n#' @noRd", l)
writeLines(l, "R/aaa-async.R")

## test files

unlink("tests/async", recursive = TRUE)
fs::dir_copy("../async/tests/testthat", "tests/async")

## (re)generate docs

devtools::document()
