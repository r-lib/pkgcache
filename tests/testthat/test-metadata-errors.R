
test_that("invalid PACKAGES file errors", {
  pri <- test_temp_dir()
  rep <- test_temp_dir()

  cmc <- cranlike_metadata_cache$new(
    pri, rep, platforms = "source", bioc = FALSE)

  pri_files <- get_private(cmc)$get_cache_files("primary")
  rep_files <- get_private(cmc)$get_cache_files("replica")

  for (pgz in rep_files$pkgs$path) {
    mkdirp(dirname(pgz))
    writeBin(charToRaw("nope"), pgz)
  }

  expect_snapshot(
    error = TRUE,
    suppressMessages(get_private(cmc)$update_replica_rds())
  )
})
