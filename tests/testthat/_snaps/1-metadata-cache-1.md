# get_current_data

    Code
      get_private(cmc)$get_current_data(oneday())
    Condition
      Error in `cmc__get_current_data()`:
      ! Loaded data outdated

---

    Code
      get_private(cmc)$get_current_data(oneday())
    Condition
      Error in `cmc__get_current_data()`:
      ! Loaded data outdated

---

    Code
      get_private(cmc)$get_current_data(oneday())
    Condition
      Error in `cmc__get_current_data()`:
      ! No data loaded

# load_replica_rds

    Code
      get_private(cmc)$load_replica_rds(oneday())
    Condition
      Error in `cmc__load_replica_rds()`:
      ! No replica RDS file in cache

---

    Code
      get_private(cmc)$load_replica_rds(oneday())
    Condition
      Error in `cmc__load_replica_rds()`:
      ! Replica RDS cache file outdated

# load_primary_rds

    Code
      get_private(cmc)$load_primary_rds(oneday())
    Condition
      Error in `cmc__load_primary_rds()`:
      ! No primary RDS file in cache

---

    Code
      get_private(cmc)$load_primary_rds(oneday())
    Condition
      Error in `cmc__load_primary_rds()`:
      ! Primary RDS cache file outdated

# locking failures

    Code
      cmc__load_primary_rds(cmc, get_private(cmc), oneday())
    Condition
      Error in `cmc__load_primary_rds()`:
      ! Cannot acquire lock to copy RDS

---

    Code
      cmc__load_primary_pkgs(cmc, get_private(cmc), oneday())
    Condition
      Error in `cmc__load_primary_pkgs()`:
      ! Cannot acquire lock to copy PACKAGES files

# load_primary_rds 3

    Code
      cmc__load_primary_rds(cmc, get_private(cmc), oneday())
    Condition
      Error in `cmc__load_primary_rds()`:
      ! Primary PACKAGES missing or newer than replica RDS, removing

# load_primary_pkgs

    Code
      get_private(cmc)$load_primary_pkgs(oneday())
    Condition
      Error in `cmc__load_primary_pkgs()`:
      ! Some primary PACKAGES files don't exist

---

    Code
      synchronise(get_private(cmc)$load_primary_pkgs(oneday()))
    Condition
      Error in `cmc__load_primary_pkgs()`:
      ! Some primary PACKAGES files don't exist

---

    Code
      synchronise(get_private(cmc)$load_primary_pkgs(oneday()))
    Condition
      Error in `cmc__load_primary_pkgs()`:
      ! Some primary PACKAGES files are outdated

# update_primary 2

    Code
      cmc__update_primary(cmc, get_private(cmc), TRUE, TRUE, TRUE)
    Condition
      Error in `cmc__update_primary()`:
      ! Cannot acquire lock to update primary cache

