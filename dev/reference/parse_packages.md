# Parse a repository metadata `PACAKGES*` file

Parse a repository metadata `PACAKGES*` file

## Usage

``` r
parse_packages(path, type = NULL)
```

## Arguments

- path:

  Path to the `PACKAGES*` file.

- type:

  Type of the file. By default it is determined automatically. Types:

  - `uncompressed`,

  - `gzip` compressed,

  - `bzip2` compressed,

  - `xz` compressed.

  - `rds`, an RDS file, which will be read using
    [`base::readRDS()`](https://rdrr.io/r/base/readRDS.html).

## Value

A data frame, with all columns from the file at `path`.

## Details

Non-existent, unreadable or corrupt `PACKAGES` files with trigger an
error.

`PACKAGES*` files do not usually declare an encoding, but nevertheless
`parse_packages()` works correctly if they do.

## Note

`parse_packages()` cannot currently read files that have very many
different fields (many columns in the result data frame). The current
limit is 1000. Typical `PACKAGES` files contain less than 20 field
types.
