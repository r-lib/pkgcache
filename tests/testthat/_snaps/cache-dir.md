# error in R CMD check

    Code
      get_user_cache_dir()
    Condition
      Error in `get_user_cache_dir()`:
      ! R_USER_CACHE_DIR env var not set during package check, see https://github.com/r-lib/pkgcache#README

# fall back to R_USER_CACHE_DIR via R_user_dir()

    Code
      get_user_cache_dir()
    Condition
      Error in `R_user_dir()`:
      ! wait

# cleanup_old_cache_dir

    Code
      cleanup_old_cache_dir()
    Condition
      Error in `cleanup_old_cache_dir()`:
      ! Set `force = TRUE` in non-interactive sessions

---

    Code
      cleanup_old_cache_dir()
    Condition
      Error in `cleanup_old_cache_dir()`:
      ! Aborted

