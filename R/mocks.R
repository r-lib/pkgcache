# Define these objects so they can be mocked in tests.
interactive <- interactive
readline <- function(prompt = "") {
  readline
}
length <- length
Sys.info <- function() {
  Sys.info
}
R_user_dir <- R_user_dir
getRversion <- getRversion
