
test_that("bioconductor$get_repos", {
  setup_fake_apps()
  bioc_version <- bioconductor$get_bioc_version("3.3.0")
  repos <- bioconductor$get_repos(bioc_version)
  expect_true("BioCextra" %in% names(repos))

  repos <- bioconductor$get_repos("3.6.0")
  expect_false("BioCextra" %in% names(repos))
})

test_that("bioconductor$.internal$get_matching_bioc_version", {
  setup_fake_apps()
  rvers <- c("2.15", "2.16", "3.1.0", "3.1.1", "3.2.0", "3.3.1", "3.4.0",
             "3.5.1")
  expect_silent(lapply(rvers, bioconductor$get_matching_bioc_version))
})
