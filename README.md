
<!-- README.md is generated from README.Rmd. Please edit that file -->

# worldmet - Accessing NOAA Global Historical Climatology Network (GHCN) meteorological observations <img src="man/figures/logo.png" align="right" height="200"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/davidcarslaw/worldmet/workflows/R-CMD-check/badge.svg)](https://github.com/davidcarslaw/worldmet/actions)
![](http://cranlogs.r-pkg.org/badges/grand-total/worldmet)
<!-- badges: end -->

`worldmet` provides an easy way to access hourly data from the [Global
Historical Climatology
Network](https://www.ncei.noaa.gov/products/global-historical-climatology-network-hourly)
(GHCNh). The GHCN contains detailed surface meteorological data from
around the world.

The package outputs work very well with the
[openair](https://github.com/davidcarslaw/openair) package.

## Installation

You can install the release version of `{worldmet}` from CRAN with:

``` r
install.packages("worldmet")
```

You can install the development version of `{worldmet}` from GitHub
with:

``` r
# install.packages("pak")
pak::pak("davidcarslaw/worldmet")
```

## Documentation

All functions in `{worldmet}` are thoroughly documented. The [worldmet
website](https://davidcarslaw.github.io/worldmet/) contains all
documentation and a change log of new features. There are also many
examples of `{worldmet}` functionality the [openair
book](https://bookdown.org/david_carslaw/openair/index.html), which goes
into great detail about its various functions.

``` r
library(worldmet)

import_ghcn_stations(country = "FR", return = "table")
#> # A tibble: 10 × 9
#>    id      latitude longitude elevation state name  gsn_flag hcn_crn_flag wmo_id
#>    <chr>      <dbl>     <dbl>     <dbl> <chr> <chr> <lgl>    <lgl>        <chr> 
#>  1 FRW000…     46.9      1.73     161.  <NA>  CHAT… NA       NA           <NA>  
#>  2 FRW000…     48.8      5.98     286.  <NA>  TOUL… NA       NA           <NA>  
#>  3 FRW000…     48.1      5.05     313   AK    CHAU… NA       NA           <NA>  
#>  4 FRW000…     49.6      3.55      83.2 AK    LAON  NA       NA           <NA>  
#>  5 FRW000…     49.0      1.22     141.  <NA>  EVRE… NA       NA           <NA>  
#>  6 FRW000…     49.2      5.68     237.  EN    ETAIN NA       NA           <NA>  
#>  7 FRW000…     48.6      1.1      183.  AK    DREUX NA       NA           <NA>  
#>  8 FRW000…     49.8      0.65      82.9 <NA>  ORLE… NA       NA           <NA>  
#>  9 FRW000…     48.8      4.18     179.  <NA>  BRIE… NA       NA           <NA>  
#> 10 FRW000…     49.1      5.47     372.  AK    VERD… NA       NA           <NA>

import_ghcn_hourly("FRW00034048", 2021)
#> # A tibble: 17,345 × 36
#>    station_id  station_name date                latitude longitude elevation
#>    <chr>       <chr>        <dttm>                 <dbl>     <dbl>     <dbl>
#>  1 FRW00034048 CHATEAUROUX  2021-01-01 00:00:00     46.9      1.73      161.
#>  2 FRW00034048 CHATEAUROUX  2021-01-01 00:30:00     46.9      1.73      161.
#>  3 FRW00034048 CHATEAUROUX  2021-01-01 01:00:00     46.9      1.73      161.
#>  4 FRW00034048 CHATEAUROUX  2021-01-01 01:30:00     46.9      1.73      161.
#>  5 FRW00034048 CHATEAUROUX  2021-01-01 02:00:00     46.9      1.73      161.
#>  6 FRW00034048 CHATEAUROUX  2021-01-01 02:30:00     46.9      1.73      161.
#>  7 FRW00034048 CHATEAUROUX  2021-01-01 03:00:00     46.9      1.73      161.
#>  8 FRW00034048 CHATEAUROUX  2021-01-01 03:30:00     46.9      1.73      161.
#>  9 FRW00034048 CHATEAUROUX  2021-01-01 04:00:00     46.9      1.73      161.
#> 10 FRW00034048 CHATEAUROUX  2021-01-01 04:30:00     46.9      1.73      161.
#> # ℹ 17,335 more rows
#> # ℹ 30 more variables: temperature <dbl>, dew_point_temperature <dbl>,
#> #   station_level_pressure <dbl>, sea_level_pressure <dbl>,
#> #   wind_direction <chr>, wind_speed <dbl>, wind_gust <dbl>,
#> #   precipitation <dbl>, relative_humidity <dbl>, wet_bulb_temperature <dbl>,
#> #   pres_wx_mw1 <chr>, pres_wx_mw2 <lgl>, pres_wx_mw3 <lgl>, pres_wx_au1 <lgl>,
#> #   pres_wx_au2 <lgl>, pres_wx_au3 <lgl>, pres_wx_aw1 <chr>, …
```

## The `{openair}` toolkit

- [`{openair}`](https://davidcarslaw.github.io/openair/): Import,
  analyse, and visualise air quality and atmospheric composition data.

- [`{worldmet}`](https://davidcarslaw.github.io/worldmet/): Access world
  meteorological data from NOAA’s Integrated Surface Database.

- [`{openairmaps}`](https://davidcarslaw.github.io/openairmaps/):
  Visualise air quality data on interactive and static maps.

- [`{deweather}`](https://davidcarslaw.github.io/deweather/): Use
  machine learning to remove the effects of meteorology on air quality
  time series.
