# Query the default CRAN repository for this session

If `options("repos")` (see
[`options()`](https://rdrr.io/r/base/options.html)) contains an entry
called `"CRAN"`, then that is returned. If it is a list, it is converted
to a character vector.

## Usage

``` r
default_cran_mirror()
```

## Value

A named character vector of length one, where the name is `"CRAN"`.

## Details

Otherwise the RStudio CRAN mirror is used.

## Examples

``` r
default_cran_mirror()
#>                       CRAN 
#> "https://cran.rstudio.com" 
```
