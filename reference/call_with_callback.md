# Call `func` and then call `callback` with the result

`callback` will be called with two arguments, the first one will the
error object if `func()` threw an error, or `NULL` otherwise. The second
argument is `NULL` on error, and the result of `func()` otherwise.

## Usage

``` r
call_with_callback(func, callback, info = NULL)
```

## Arguments

- func:

  Function to call.

- callback:

  Callback to call with the result of `func()`, or the error thrown.

- info:

  Extra info to add to the error object. Must be a named list.
