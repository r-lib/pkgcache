# viapply

    Code
      viapply(l, identity)
    Condition
      Error in `vapply()`:
      ! values must be length 1,
       but FUN(X[[1]]) result is length 0
    Code
      viapply(letters, identity)
    Condition
      Error in `vapply()`:
      ! values must be type 'integer',
       but FUN(X[[1]]) result is type 'character'

# vcapply

    Code
      vcapply(l, identity)
    Condition
      Error in `vapply()`:
      ! values must be length 1,
       but FUN(X[[1]]) result is length 0
    Code
      vcapply(1:5, identity)
    Condition
      Error in `vapply()`:
      ! values must be type 'character',
       but FUN(X[[1]]) result is type 'integer'

# vlapply

    Code
      vlapply(l, identity)
    Condition
      Error in `vapply()`:
      ! values must be length 1,
       but FUN(X[[1]]) result is length 0
    Code
      vlapply(1:5, identity)
    Condition
      Error in `vapply()`:
      ! values must be type 'logical',
       but FUN(X[[1]]) result is type 'integer'

# vdapply

    Code
      vdapply(l, identity)
    Condition
      Error in `vapply()`:
      ! values must be length 1,
       but FUN(X[[1]]) result is length 0
    Code
      vdapply(letters, identity)
    Condition
      Error in `vapply()`:
      ! values must be type 'double',
       but FUN(X[[1]]) result is type 'character'

# mapx

    Code
      mapx()
    Condition
      Error in `mapx()`:
      ! No arguments to `mapx()`
    Code
      mapx(1)
    Condition
      Error in `mapx()`:
      ! Last `mapx()` argument not a function
    Code
      mapx(identity)
    Condition
      Error in `mapx()`:
      ! No data to `mapx()`
    Code
      mapx(1:2, 1:10, paste)
    Condition
      Error in `mapx()`:
      ! Incompatible data lengths in `mapx()`: 2, 10

# msg_wrap

    Code
      msg <- msg_wrap("some error message", "\n\n",
        "Could not load or update archive cache. If you think your local ",
        "cache is broken, try deleting it with `cran_archive_cleanup()` or ",
        "the `$cleanup()` method.")
      stop(msg)
    Condition
      Error:
      ! 
      some error message
      
      Could not load or update archive cache. If you think your local cache
      is broken, try deleting it with `cran_archive_cleanup()` or the
      `$cleanup()` method.

# last

    Code
      last(list())
    Condition
      Error in `x[[length(x)]]`:
      ! attempt to select less than one element in integerOneIndex

