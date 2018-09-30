
#' A simple package cache
#'
#' This is an R6 class that implements a concurrency safe package cache.
#'
#' By default these fields are included for every package:
#' * `fullpath` Full package path.
#' * `path` Package path, within the repository.
#' * `package` Package name.
#' * `url` URL it was downloaded from.
#' * `etag` ETag for the last download, from the given URL.
#' * `md5` MD5 of the file, to make sure if it has not changed.
#'
#' Additional fields can be added as needed.
#'
#' For a simple API to a session-wide instance of this class, see
#' [pkg_cache_summary()] and the other functions listed there.
#'
#' @section Usage:
#' ```
#' pc <- package_cache$new(path = NULL)
#'
#' pc$list()
#' pc$find(..., .list = NULL)
#' pc$copy_to(..., .list = NULL)
#' pc$add(file, path, md5 = tools::md5sum(file)[[1]], ..., .list = NULL)
#' pc$add_url(url, path, ..., .list = NULL, on_progress = NULL)
#' pc$async_add_url(url, path, ..., .list = NULL, on_progress = NULL)
#' pc$copy_or_add(target, urls, path, md5 = NULL, ..., .list = NULL,
#'                on_progress = NULL)
#' pc$async_copy_or_add(target, urls, path, ..., md5 = NULL, ...,
#'                .list = NULL, on_progress = NULL)
#' pc$update_or_add(target, urls, path, ..., .list = NULL,
#'                on_progress = NULL)
#' pc$async_update_or_add(target, urls, path, ..., .list = NULL,
#'                on_progress = NULL)
#' pc$delete(..., .list = NULL)
#' ```
#'
#' @section Arguments:
#' * `path`: For `package_cache$new()` the location of the cache. For other
#'   functions the location of the file inside the cache.
#' * `...`: Extra attributes to search for. They have to be named.
#' * `.list`: Extra attributes to search for, they have to in a named list.
#' * `file`:  Path to the file to add.
#' * `url`: URL attribute. This is used to update the file, if requested.
#' * `md5`: MD5 hash of the file.
#' * `on_progress`: Callback to create progress bard. Passed to
#'   [http_get()].
#' * `target`: Path to copy the (first) to hit to.
#' * `urls`: Character vector or URLs to try to download the file from.
#'
#' @section Details:
#'
#' `package_cache$new()` attaches to the cache at `path`. (By default
#' a platform dependent user level cache directory.) If the cache does
#' not exists, it creates it.
#'
#' `pc$list()` lists all files in the cache, returns a tibble with all the
#' default columns, and potentially extra columns as well.
#'
#' `pc$find()` list all files that match the specified criteria (`fullpath`,
#' `path`, `package`, etc.). Custom columns can be searched for as well.
#'
#' `pc$copy_to()` will copy the first matching file from the cache to
#' `target`. It returns the tibble of _all_ matching records, invisibly.
#' If no file matches, it returns an empty (zero-row) tibble.
#'
#' `pc$add()` adds a file to the cache.
#'
#' `pc$add_url()` downloads a file and adds it to the cache.
#'
#' `pc$async_add_url()` is the same, but it is asynchronous.
#'
#' `pc$copy_or_add()` works like `pc$copy_to()`, but if the file is not in
#' the cache, it tries to download it from one of the specified URLs first.
#'
#' `pc$async_copy_or_add()` is the same, but asynchronous.
#'
#' `pc$update_or_add()` is like `pc$copy_to_add()`, but if the file is in
#' the cache it tries to update it from the urls, using the stored ETag to
#' avoid unnecessary downloads.
#'
#' `pc$async_update_or_add()` is the same, but it is asynchronous.
#'
#' `pc$delete()` deletes the file(s) from the cache.
#'
#' @importFrom R6 R6Class
#' @importFrom filelock lock unlock
#' @importFrom tools md5sum
#' @importFrom async async_constant
#'
#' @export

package_cache <- R6Class(
  "package_cache",
  public = list(
    initialize = function(path = NULL) {
      path <- path %||% get_user_cache_dir()$root
      assert_that(is_path(path))
      private$path <- path
      create_empty_db_file_if_needed(path)
      invisible(self)
    },

    list = function() {
      l <- private$lock(exclusive = FALSE)
      on.exit(unlock(l), add = TRUE)
      dbfile <- get_db_file(private$path)
      readRDS(dbfile)
    },

    find = function(..., .list = NULL) {
      self$copy_to(NULL, ..., .list = .list)
    },

    copy_to = function(target, ..., .list = NULL) {
      l <- private$lock(exclusive = FALSE)
      on.exit(unlock(l), add = TRUE)
      res <- private$find_locked(..., .list = .list)
      if (!is.null(target) && nrow(res) >= 1) {
        mkdirp(dirname(target))
        file.copy(res$fullpath[1], target)
      }
      res
    },

    add = function(file, path, md5 = tools::md5sum(file)[[1]], ...,
                   .list = NULL) {

      assert_that(is_existing_file(file))

      l <- private$lock(exclusive = TRUE)
      on.exit(unlock(l), add = TRUE)
      dbfile <- get_db_file(private$path)
      db <- readRDS(dbfile)

      idx <- find_in_data_frame(
        db, path = path, md5 = md5, ..., .list = .list)

      target <- file.path(private$path, path)
      mkdirp(dirname(target))
      file.copy(file, target, overwrite = TRUE)
      db <- append_to_data_frame(
        db, fullpath = target, path = path, ..., md5 = null2na(md5),
        .list = .list)
      saveRDS(db, dbfile)
      db[nrow(db), ]
    },

    ## Just download a file from an url and add it
    ## Returns a deferred value
    async_add_url = function(url, path, ..., .list = NULL,
                             on_progress = NULL) {
      self; private; url; path; list(...); .list; on_progress
      target <- tempfile()
      download_file(url, target, on_progress = on_progress)$
        then(function(res) {
          self$add(target, path, url = url, etag = res$etag, ...,
                   md5 = md5sum(target)[[1]], .list = .list)
        })$
        finally(function(x) unlink(target, recursive = TRUE))
    },

    add_url = function(url, path, ..., .list = NULL, on_progress = NULL) {
      synchronise(self$async_add_url(url, path, ..., .list = .list,
                                     on_progress = on_progress))
    },

    ## If the file is not in the cache, then download it and add it.
    async_copy_or_add = function(target, urls, path, md5 = NULL, ...,
                                 .list = NULL, on_progress = NULL) {
      self; private; target; urls; path; md5; list(...); .list; on_progress
      etag <- tempfile()
      async_constant()$
        then(~ self$copy_to(target, url = urls[1], ..., .list = .list))$
        then(function(res) {
          if (! nrow(res)) {
            download_one_of(urls, target, on_progress = on_progress)$
              then(function(d) {
                md5 <- md5sum(target)[[1]]
                self$add(target, path, url = d$url, etag = d$etag,
                         md5 = null2na(md5), ..., .list = .list)
              })$
              then(function(x) add_attr(x, "action", "Got"))
          } else {
            add_attr(res, "action", "Had")
          }
        })$
        finally(function(x) unlink(etag, recursive = TRUE))
    },

    copy_or_add = function(target, urls, path, md5 = NULL, ...,
                           .list = NULL, on_progress = NULL) {
      synchronise(self$async_copy_or_add(
                         target, urls, path, md5, ...,
                         .list = .list, on_progress = on_progress))
    },

    ## Like copy_to_add, but we always try to update the file, from
    ## the URL, and if the update was successful, we update the file
    ## in the cache as well
    async_update_or_add = function(target, urls, path, md5 = NULL, ...,
                                   .list = NULL, on_progress = NULL) {
      self; private; target; urls; path; md5; list(...); .list; on_progress
      async_constant()$
        then(~ self$copy_to(target, url = urls[1], path = path, ...,
                            .list = .list))$
        then(function(res) {
          if (! nrow(res)) {
            ## Not in the cache, download and add it
            download_one_of(urls, target, on_progress = on_progress)$
              then(function(d) {
                md5 <- md5sum(target)[[1]]
                self$add(target, path, url = d$url, etag = d$etag,
                         md5 = null2na(md5), ..., .list = .list)
              })$
              then(function(x) add_attr(x, "action", "Got"))
          } else {
            ## In the cache, check if it is current
            cat(res$etag, file = etag <- tempfile())
            download_one_of(urls, target, etag_file = etag,
                            on_progress = on_progress)$
              then(function(d) {
                if (d$response$status_code != 304) {
                  ## No current, update it
                  md5 <- md5sum(target)[[1]]
                  x <- self$add(target, path, url = d$url,
                                etag = d$etag, md5 = null2na(md5), ...,
                                .list = .list)
                  add_attr(x, "action", "Got")
                } else {
                  ## Current, nothing to do
                  add_attr(res, "action", "Current")
                }
              })$
              finally(function(x) unlink(etag, recursive = TRUE))
          }
        })
    },

    update_or_add = function(target, urls, path, ..., .list = NULL,
                             on_progress = NULL) {
      synchronise(self$async_update_or_add(
                         target, urls, path, ...,
                         .list = .list, on_progress = on_progress))
    },

    delete = function(..., .list = NULL) {
      l <- private$lock(exclusive = TRUE)
      on.exit(unlock(l), add = TRUE)
      dbfile <- get_db_file(private$path)

      ex <- private$find_locked(..., .list = .list)
      if (nrow(ex) == 0) stop("Package does not exist in cache")
      unlink(file.path(private$path, ex$path))
      db <- delete_from_data_frame(readRDS(dbfile), ..., .list = .list)
      saveRDS(db, dbfile)
    }
  ),

  ## ----------------------------------------------------------------------

  private = list(
    path = NULL,
    lock = function(exclusive = TRUE, ...) {
      lockfile <- get_lock_file(private$path)
      filelock::lock(lockfile, exclusive = exclusive, ...)
    },
    find_locked = function(..., .list = NULL) {
      dbfile <- get_db_file(private$path)
      db <- readRDS(dbfile)

      idx <- find_in_data_frame(db, ..., .list = .list)
      db[idx, ]
    }
  )
)

## ------------------------------------------------------------------------
## Internal functions

get_db_file <- function(path) {
  file.path(path, ".db.rds")
}

get_lock_file <- function(path) {
  file.path(path, ".db.lock")
}

create_empty_db_file_if_needed <- function(path) {
  mkdirp(path)

  dbfile <- get_db_file(path)
  if (file.exists(dbfile)) return()

  lockfile <- get_lock_file(path)

  df <- make_empty_db_data_frame()

  l <- lock(lockfile)
  on.exit(unlock(l))
  saveRDS(df, file = dbfile)
}

make_empty_db_data_frame <- function() {
  data.frame(
    stringsAsFactors = FALSE,
    fullpath = character(),
    path     = character(),
    package  = character(),
    url      = character(),
    etag     = character(),
    md5      = character()
  )
}
