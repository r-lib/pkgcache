# fail stage if no error callback

    Code
      run_event_loop(do())
    Condition
      Error in `lst$cb()`:
      ! foobar

# error within error callback

    Code
      run_event_loop(do())
    Condition
      Error in `lst$cb()`:
      ! baz

