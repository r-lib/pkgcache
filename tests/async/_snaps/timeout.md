# timed out

    Code
      synchronise(async_timeout(f, 1 / 1000))
    Condition
      Error:
      ! Aync operation timed out

# error before async_timeout

    Code
      synchronise(async_timeout(f, 1 / 10))
    Condition
      Error in `fun()`:
      ! oops

# error after async_timeout

    Code
      synchronise(async_timeout(f, 1 / 1000))
    Condition
      Error:
      ! Aync operation timed out

