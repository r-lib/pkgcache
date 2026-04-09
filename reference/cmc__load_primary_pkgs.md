# Load metadata from the primary cache's PACKAGES files

If they are not available, or outdated, it throws an error. Otherwise
they are copied to the replica cache, and then used to create the RDS
file. The RDS file is then written back to the primary cache and also
loaded.

## Usage

``` r
cmc__load_primary_pkgs(self, private, max_age)
```

## Arguments

- self:

  self

- private:

  private self

- max_age:

  Max age to consider the files current.

## Value

Metadata.
