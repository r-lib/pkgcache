# Define these objects so they can be mocked in tests.
interactive <- base::interactive
readline <- function(prompt = "") {
  base::readline(prompt = prompt)
}
Sys.info <- function() {
  base::Sys.info()
}
getRversion <- base::getRversion
