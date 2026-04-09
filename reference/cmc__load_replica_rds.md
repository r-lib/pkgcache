# Try to load the package metadata asynchronously, from the replica RDS

If the replica has the RDS data, it is loaded and returned. Otherwise
throws an error.

## Usage

``` r
cmc__load_replica_rds(self, private, max_age)
```

## Arguments

- self:

  Self.

- private:

  Private self.

- max_age:

  Maximum age allowed for the RDS file to be considered as current.

## Value

The metadata.
