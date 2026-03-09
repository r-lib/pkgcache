# A simple package cache

This is an R6 class that implements a concurrency safe package cache.

## Details

By default these fields are included for every package:

- `fullpath` Full package path.

- `path` Package path, within the repository.

- `package` Package name.

- `url` URL it was downloaded from.

- `etag` ETag for the last download, from the given URL.

- `sha256` SHA256 hash of the file.

Additional fields can be added as needed.

For a simple API to a session-wide instance of this class, see
[`pkg_cache_summary()`](https://r-lib.github.io/pkgcache/dev/reference/pkg_cache_api.md)
and the other functions listed there.

## Usage

    pc <- package_cache$new(path = NULL)

    pc$list()
    pc$find(..., .list = NULL)
    pc$copy_to(..., .list = NULL)
    pc$add(file, path, sha256 = shasum256(file), ..., .list = NULL)
    pc$add_url(url, path, ..., .list = NULL, on_progress = NULL,
      http_headers = NULL)
    pc$async_add_url(url, path, ..., .list = NULL, on_progress = NULL,
      http_headers = NULL)
    pc$copy_or_add(target, urls, path, sha256 = NULL, ..., .list = NULL,
                   on_progress = NULL, http_headers = NULL)
    pc$async_copy_or_add(target, urls, path, ..., sha256 = NULL, ...,
                   .list = NULL, on_progress = NULL, http_headers = NULL)
    pc$update_or_add(target, urls, path, ..., .list = NULL,
                   on_progress = NULL, http_headers = NULL)
    pc$async_update_or_add(target, urls, path, ..., .list = NULL,
                   on_progress = NULL, http_headers = NULL)
    pc$delete(..., .list = NULL)

## Arguments

- `path`: For `package_cache$new()` the location of the cache. For other
  functions the location of the file inside the cache.

- `...`: Extra attributes to search for. They have to be named.

- `.list`: Extra attributes to search for, they have to in a named list.

- `file`: Path to the file to add.

- `url`: URL attribute. This is used to update the file, if requested.

- `sha256`: SHA256 hash of the file.

- `on_progress`: Callback to create progress bar. Passed to internal
  function `http_get()`.

- `target`: Path to copy the (first) to hit to.

- `urls`: Character vector or URLs to try to download the file from.

- `http_headers`: HTTP headers to add to all HTTP queries.

## Details

`package_cache$new()` attaches to the cache at `path`. (By default a
platform dependent user level cache directory.) If the cache does not
exists, it creates it.

`pc$list()` lists all files in the cache, returns a data frame with all
the default columns, and potentially extra columns as well.

`pc$find()` list all files that match the specified criteria
(`fullpath`, `path`, `package`, etc.). Custom columns can be searched
for as well.

`pc$copy_to()` will copy the first matching file from the cache to
`target`. It returns the data frame of *all* matching records,
invisibly. If no file matches, it returns an empty (zero-row) data
frame.

`pc$add()` adds a file to the cache.

`pc$add_url()` downloads a file and adds it to the cache.

`pc$async_add_url()` is the same, but it is asynchronous.

`pc$copy_or_add()` works like `pc$copy_to()`, but if the file is not in
the cache, it tries to download it from one of the specified URLs first.

`pc$async_copy_or_add()` is the same, but asynchronous.

`pc$update_or_add()` is like `pc$copy_to_add()`, but if the file is in
the cache it tries to update it from the urls, using the stored ETag to
avoid unnecessary downloads.

`pc$async_update_or_add()` is the same, but it is asynchronous.

`pc$delete()` deletes the file(s) from the cache.

## Examples

``` r
## Although package_cache usually stores packages, it may store
## arbitrary files, that can be search by metadata
pc <- package_cache$new(path = tempfile())
pc$list()
#> [1] fullpath path     package  url      etag     sha256  
#> <0 rows> (or 0-length row.names)

cat("foo\n", file = f1 <- tempfile())
cat("bar\n", file = f2 <- tempfile())
pc$add(f1, "/f1")
#>                               fullpath path package url etag
#> 1 /tmp/RtmpnX4e9K/file1a5e201d9722//f1  /f1      NA  NA   NA
#>                                                             sha256
#> 1 b5bb9d8014a0f9b1d61e21e796d78dccdf1352f23cd32812f4850b878ae4944c
pc$add(f2, "/f2")
#>                               fullpath path package url etag
#> 2 /tmp/RtmpnX4e9K/file1a5e201d9722//f2  /f2      NA  NA   NA
#>                                                             sha256
#> 2 7d865e959b2466918c9863afca942d0fb89d7c9ac0c99bafc3749504ded97730
pc$list()
#>                               fullpath path package url etag
#> 1 /tmp/RtmpnX4e9K/file1a5e201d9722//f1  /f1      NA  NA   NA
#> 2 /tmp/RtmpnX4e9K/file1a5e201d9722//f2  /f2      NA  NA   NA
#>                                                             sha256
#> 1 b5bb9d8014a0f9b1d61e21e796d78dccdf1352f23cd32812f4850b878ae4944c
#> 2 7d865e959b2466918c9863afca942d0fb89d7c9ac0c99bafc3749504ded97730
pc$find(path = "/f1")
#>                               fullpath path package url etag
#> 1 /tmp/RtmpnX4e9K/file1a5e201d9722//f1  /f1      NA  NA   NA
#>                                                             sha256
#> 1 b5bb9d8014a0f9b1d61e21e796d78dccdf1352f23cd32812f4850b878ae4944c
pc$copy_to(target = f3 <- tempfile(), path = "/f1")
#>                               fullpath path package url etag
#> 1 /tmp/RtmpnX4e9K/file1a5e201d9722//f1  /f1      NA  NA   NA
#>                                                             sha256
#> 1 b5bb9d8014a0f9b1d61e21e796d78dccdf1352f23cd32812f4850b878ae4944c
readLines(f3)
#> [1] "foo"
```
