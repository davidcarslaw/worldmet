
<!-- Edit the README.Rmd only!!! The README.md is generated automatically from README.Rmd. -->
worldmet - R package for accessing NOAA Integrated Surface Database (ISD) meteorological observations
=====================================================================================================

[![Travis-CI Build Status](https://travis-ci.org/davidcarslaw/worldmet.svg?branch=master)](https://travis-ci.org/davidcarslaw/worldmet)

`worldmet` provides an easy way to access data from the [NOAA Integrated Surface Database](https://www.ncdc.noaa.gov/isd) (ISD). The ISD contains detailed surface meteorological data from around the world for over 35,000 locations. See also the [map](https://gis.ncdc.noaa.gov/map/viewer/#app=cdo&cfg=cdo&theme=hourly&layers=1).

The package outputs (typically hourly meteorological data) work very well with the [openair](https://github.com/davidcarslaw/openair) package.

Installation
------------

Installation of `worldmet` from GitHub is easy using the `devtools` package.

``` r
require(devtools)
install_github('davidcarslaw/worldmet')
```

Brief examples
--------------

To search for meteorological sites the user can search by the name or partial name of the site in upper or lower case. The `getMeta` function will return all site names that match the search string. The most important information returned is the `code`, which can then be supplied to the `importNOAA` function that downloads the data.

For example, to search for site "heathrow":

``` r
library(worldmet)
## user getMeta function to search for sites
## note code to be used in importNOAA

getMeta(site = "heathrow")
##        USAF  WBAN  STATION CTRY ST CALL latitude longitude ELEV(M)
## 1669 037720 99999 HEATHROW   UK    EGLL   51.478    -0.461    25.3
##           BEGIN        END         code dist
## 1669 1948-12-01 2017-12-15 037720-99999   NA
```

Often we have a latitude / longitude of interest. A search can be made based on supplied decimal coordinates and the top `n` nearest sites are returned. The map shows the location searched by the user (red dot) and markers showing the nearest meteorological stations. Click on a station marker to obtain the code and other basic information.

``` r
## search for near a specified lat/lon - near Beijing airport
## returns 'n' nearest by default
info <- getMeta(lat = 40, lon = 116.9)
```

<img src="inst/images/map.png" alt="map of Beijing area" width="75%" />

To obtain the data the user must supply a `code` (see above) and year or years of interest. For example, to download data for Heathrow Airport in 2010 (code 037720-99999):

``` r
dat <- importNOAA(code = "037720-99999", year = 2010)
head(dat)
## # A tibble: 6 x 23
##                  date   usaf  wban         code  station    lat   lon
##                <dttm>  <chr> <int>        <chr>    <chr>  <dbl> <dbl>
## 1 2010-01-01 00:00:00 037720 99999 037720-99999 HEATHROW 51.483 -0.45
## 2 2010-01-01 01:00:00 037720 99999 037720-99999 HEATHROW 51.483 -0.45
## 3 2010-01-01 02:00:00 037720 99999 037720-99999 HEATHROW 51.483 -0.45
## 4 2010-01-01 03:00:00 037720 99999 037720-99999 HEATHROW 51.483 -0.45
## 5 2010-01-01 04:00:00 037720 99999 037720-99999 HEATHROW 51.483 -0.45
## 6 2010-01-01 05:00:00 037720 99999 037720-99999 HEATHROW 51.483 -0.45
## # ... with 16 more variables: elev <dbl>, wd <dbl>, ws <dbl>,
## #   ceil_hgt <dbl>, visibility <dbl>, air_temp <dbl>, dew_point <dbl>,
## #   atmos_pres <dbl>, RH <dbl>, cl_1 <dbl>, cl_1_height <dbl>, cl_2 <dbl>,
## #   cl_2_height <dbl>, cl_3 <dbl>, cl_3_height <dbl>, cl <dbl>
```
