# msg_wrap

    Code
      msg <- msg_wrap("some error message", "\n\n",
        "Could not load or update archive cache. If you think your local ",
        "cache is broken, try deleting it with `cran_archive_cleanup()` or ",
        "the `$cleanup()` method.")
      stop(msg)
    Error <simpleError>
      
      some error message
      
      Could not load or update archive cache. If you think your local cache
      is broken, try deleting it with `cran_archive_cleanup()` or the
      `$cleanup()` method.

