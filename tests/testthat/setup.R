withr::local_envvar(
  R_USER_CACHE_DIR = tempfile(),
  .local_envir = teardown_env()
)

withr::defer(
  .Call(pkgcache__gcov_flush),
  teardown_env()
)

withr::defer(
  bioconductor$.internal$clear_cache(),
  teardown_env()
)
