# race() rejects (#76)

    Code
      synchronise(async_race(delay(0.1), defer_fail()))
    Condition
      Error:
      ! foo

