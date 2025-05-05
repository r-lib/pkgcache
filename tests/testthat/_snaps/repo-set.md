# repo_add

    Code
      repo_get()[1:3, ]
    Output
      # A data frame: 3 x 5
        name  url                      type     r_version bioc_version
      * <chr> <chr>                    <chr>    <chr>     <chr>       
      1 URL   https://my.url           cranlike *         <NA>        
      2 PATH  /foo/bar                 cranlike *         <NA>        
      3 CRAN  https://cran.rstudio.com cran     *         <NA>        

# with_repo

    Code
      with_repo(c(URL = "https://my.url"), repo_get()[1, ])
    Output
      # A data frame: 1 x 5
        name  url            type     r_version bioc_version
      * <chr> <chr>          <chr>    <chr>     <chr>       
      1 URL   https://my.url cranlike *         <NA>        

# repo_sugar_mran

    Code
      repo_sugar_mran("2017-01-31", NULL)
    Condition
      Error in `repo_sugar_mran()`:
      ! PPM snapshots go back to 2017-10-10 only

# repo_sugar_ppm

    Code
      repo_sugar_ppm(as.Date("2017-10-01"), NULL)
    Condition
      Error in `repo_sugar_ppm()`:
      ! PPM snapshots go back to 2017-10-10 only

# parse_spec_r

    Code
      parse_spec_r("100.0.0")
    Condition
      Error in `parse_spec_r()`:
      ! Unknown R version: '100.0.0'

# get_r_versions

    Code
      ret
    Output
      [[1]]
      [[1]]$version
      [1] "3.6.2"
      
      [[1]]$date
      [1] "2019-12-12T08:05:03.679160Z"
      
      [[1]]$nickname
      [1] "Dark and Stormy Night"
      
      
      [[2]]
      [[2]]$version
      [1] "3.6.3"
      
      [[2]]$date
      [1] "2020-02-29T08:05:16.744223Z"
      
      [[2]]$nickname
      [1] "Holding the Windsock"
      
      
      [[3]]
      [[3]]$version
      [1] "4.0.0"
      
      [[3]]$date
      [1] "2020-04-24T07:05:34.612930Z"
      
      [[3]]$nickname
      [1] "Arbor Day"
      
      
      [[4]]
      [[4]]$version
      [1] "4.0.1"
      
      [[4]]$date
      [1] "2020-06-06T07:05:16.469439Z"
      
      [[4]]$nickname
      [1] "See Things Now"
      
      
      [[5]]
      [[5]]$version
      [1] "4.0.2"
      
      [[5]]$date
      [1] "2020-06-22T07:05:19.236082Z"
      
      [[5]]$nickname
      [1] "Taking Off Again"
      
      
      [[6]]
      [[6]]$version
      [1] "4.0.3"
      
      [[6]]$date
      [1] "2020-10-10T07:05:24.661746Z"
      
      [[6]]$nickname
      [1] "Bunny-Wunnies Freak Out"
      
      

---

    Code
      get_r_versions()
    Condition
      Error in `get_r_versions()`:
      ! Failed to download R versions from 'http://127.0.0.1:<port>/rversionsx'

# parse_spec_pkg

    Code
      parse_spec_pkg("foo-")
    Condition
      Error in `parse_spec_pkg()`:
      ! Invalid package version: 'foo-'
    Code
      parse_spec_pkg("-1.0.1")
    Condition
      Error in `parse_spec_pkg()`:
      ! Invalid package version: '-1.0.1'

---

    Code
      parse_spec_pkg("dplyr-0.0.0")
    Condition
      Error in `parse_spec_pkg()`:
      ! Unknown 'dplyr' version: '0.0.0'

---

    Code
      pkgenv$pkg_versions[["dplyr"]]
    Output
      $`0.1`
      [1] "2014-01-16T22:53:34+00:00"
      
      $`0.1.1`
      [1] "2014-01-29T21:24:54+00:00"
      
      $`0.1.2`
      [1] "2014-02-24T16:36:07+00:00"
      
      $`0.1.3`
      [1] "2014-03-15T00:36:22+00:00"
      
      $`0.2`
      [1] "2014-05-21T08:19:55+00:00"
      
      $`0.3`
      [1] "2014-10-04T06:39:05+00:00"
      
      $`0.3.0.1`
      [1] "2014-10-08T05:21:23+00:00"
      
      $`0.3.0.2`
      [1] "2014-10-11T07:43:41+00:00"
      
      $`0.4.0`
      [1] "2015-01-08T11:12:43+00:00"
      
      $`0.4.1`
      [1] "2015-01-14T07:15:50+00:00"
      
      $`0.4.2`
      [1] "2015-06-16T11:24:05+00:00"
      
      $`0.4.3`
      [1] "2015-09-01T18:15:02+00:00"
      
      $`0.5.0`
      [1] "2016-06-24T15:37:11+00:00"
      
      $`0.7.0`
      [1] "2017-06-09T10:08:18+00:00"
      
      $`0.7.1`
      [1] "2017-06-22T12:31:04+00:00"
      
      $`0.7.2`
      [1] "2017-07-20T22:39:45+00:00"
      
      $`0.7.3`
      [1] "2017-09-09T13:04:29+00:00"
      
      $`0.7.4`
      [1] "2017-09-28T19:43:29+00:00"
      
      $`0.7.5`
      [1] "2018-05-19T06:06:06+00:00"
      
      $`0.7.6`
      [1] "2018-06-29T20:23:20+00:00"
      
      $`0.7.7`
      [1] "2018-10-16T12:20:03+00:00"
      
      $`0.7.8`
      [1] "2018-11-10T06:30:02+00:00"
      
      $`0.8.0`
      [1] "2019-02-14T21:22:14+00:00"
      
      $`0.8.0.1`
      [1] "2019-02-15T14:30:53+00:00"
      
      $`0.8.1`
      [1] "2019-05-14T11:20:03+00:00"
      
      $`0.8.2`
      [1] "2019-06-29T03:50:03+00:00"
      
      $`0.8.3`
      [1] "2019-07-04T14:50:02+00:00"
      
      $`0.8.4`
      [1] "2020-01-31T16:20:08+00:00"
      
      $`0.8.5`
      [1] "2020-03-07T11:20:02+00:00"
      
      $`1.0.0`
      [1] "2020-05-29T14:00:03+00:00"
      
      $`1.0.1`
      [1] "2020-07-31T06:00:05+00:00"
      
      $`1.0.2`
      [1] "2020-08-18T11:30:02+00:00"
      
      $`1.0.3`
      [1] "2021-01-15T12:20:05+00:00"
      
      $`1.0.4`
      [1] "2021-02-02T16:10:03+00:00"
      

---

    Code
      parse_spec_pkg("foobar-1.0.0")
    Condition
      Error in `get_pkg_versions()`:
      ! Cannot find package versions for 'foobar'. Is it a CRAN package?

---

    Code
      parse_spec_pkg("bad-1.0.0")
    Condition
      Error in `get_pkg_versions()`:
      ! Failed to download package versions for 'bad' from 'http://127.0.0.1:<port>/crandb/bad'

