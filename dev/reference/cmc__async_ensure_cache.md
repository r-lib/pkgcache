# Load the cache, asynchronously, with as little work as possible

1.  If it is already loaded, and fresh return it.

2.  Otherwise try the replica RDS.

3.  Otherwise try the primary RDS.

4.  Otherwise try the primary PACKAGES files.

5.  Otherwise update the replica PACKAGES files, the replica RDS, and
    then also the primary PACKAGES and RDS.

## Usage

``` r
cmc__async_ensure_cache(self, private, max_age)
```

## Arguments

- self:

  self

- private:

  private self

- max_age:

  Maximum age allowed to consider the data current.

## Value

Metadata.
