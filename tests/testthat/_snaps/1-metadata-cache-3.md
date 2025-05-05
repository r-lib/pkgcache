# cleanup

    Code
      cmc_cleanup(NULL, NULL, FALSE)
    Condition
      Error in `cmc_cleanup()`:
      ! Not cleaning up cache, please specify `force = TRUE`

---

    Code
      cmc_cleanup(cmc, get_private(cmc), FALSE)
    Condition
      Error in `cmc_cleanup()`:
      ! Aborted

# memory cache

    Code
      data3 <- get_private(cmc3)$get_memory_cache(instance)
    Condition
      Error in `cmc__get_memory_cache()`:
      ! Memory cache outdated

# update_memory_cache

    Code
      cmc__copy_to_replica(cmc, get_private(cmc), TRUE, TRUE, TRUE)
    Condition
      Error in `cmc__copy_to_replica()`:
      ! Cannot acquire lock to copy primary cache

# corrupt metadata

    Code
      suppressMessages(cmc$list())
    Condition
      Error in `filelock::lock()`:
      ! Cannot open lock file: Not a directory

