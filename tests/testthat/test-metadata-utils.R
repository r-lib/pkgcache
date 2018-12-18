
context("metadata-utils")

test_that("type_bioc_get_bioc_repos", {
  r <- type_bioc_get_bioc_repos("3.3.0")
  expect_true("BioCextra" %in% names(r$repos))

  r <- type_bioc_get_bioc_repos("3.6.0")
  expect_false("BioCextra" %in% names(r$repos))
})

test_that("type_bioc_matching_bioc_version", {
  rvers <- c("2.15", "2.16", "3.1.0", "3.1.1", "3.2.0", "3.3.1", "3.4.0",
             "3.5.1")
  expect_silent(vcapply(rvers, type_bioc_matching_bioc_version))
  expect_error(type_bioc_matching_bioc_version("2.14"))
})
