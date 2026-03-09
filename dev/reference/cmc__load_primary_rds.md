# Load the metadata from the primary cache's RDS file

If it exists and current, then the replica RDS is updated to it as well,
and the data is returned. Otherwise throws an error.

## Usage

``` r
cmc__load_primary_rds(self, private, max_age)
```

## Arguments

- self:

  Self.

- private:

  Private self.

- max_age:

  Maximum age allowed for the RDS file to be considered as current.

## Value

Metadata.
