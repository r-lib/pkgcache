# Query the version of the graphics API

A package compiled with a certain version of the graphics API will not
work with R installations that use a different version.

## Usage

``` r
get_graphics_api_version()
```

## Value

An integer scalar, the version of the graphics API of this R version.

## Examples

``` r
get_graphics_api_version()
#> [1] 16
```
