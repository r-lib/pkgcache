
test_that("invalid PACKAGES file warns", {
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

  warn <- NULL
  expect_error(
    withCallingHandlers(
      suppressMessages(get_private(cmc)$update_replica_rds()),
      warning = function(w) {
        warn <<- w
        invokeRestart("muffleWarning")
      }
    ),
    "No metadata available"
  )

  expect_match(
    conditionMessage(warn),
    "Cannot read metadata information"
  )
})
