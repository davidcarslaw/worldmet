# worldmet - R package for accessing NOAA Integrated Surface Database (ISD) meteorological observations

[![Travis-CI Build Status](https://travis-ci.org/davidcarslaw/worldmet.svg?branch=master)](https://travis-ci.org/davidcarslaw/worldmet)

`worldmet` provides an easy way to access data from the [NOAA Integrated
Surface Database](https://www.ncdc.noaa.gov/isd) (ISD). The ISD contains detailed surface
meteorological data from around the world for over 35,000
locations. See also the
[map](https://gis.ncdc.noaa.gov/map/viewer/#app=cdo&cfg=cdo&theme=hourly&layers=1).

The package outputs (typically hourly meteorological data) work very
well with the [openair](https://github.com/davidcarslaw/openair) package.

## Installation

Installation of `worldmet` from GitHub is easy using the `devtools`
package.

```R
require(devtools)
install_github('davidcarslaw/worldmet')
```

## Brief examples

To search for meteorological sites the user can search by the name or partial name of the site in upper or lower case. The `getMeta` function will return all site names that match the search string. The most important information returned is the `code`, which can then be supplied to the `importNOAA` function that downloads the data.

For example, to search for site "heathrow":

```R
## user getMeta function to search for sites
## note code to be used in importNOAA

getMeta(site = "heathrow")
       USAF  WBAN  STATION CTRY ST CALL    LAT    LON ELEV(M)      BEGIN        END
1687 037720 99999 HEATHROW   UK    EGLL 51.478 -0.461    25.3 1948-12-01 2015-06-12
             code
1687 037720-99999
```

Often we have a latitude / longitude of interest. A search can be made based on supplied decimal coordinates and the top `n` nearest sites are returned.

```R
## search for near a specified lat/lon - near Beijing airport
## returns 'n' nearest by default
getMeta(lat = 40, lon = 116.9)
        USAF  WBAN                                 STATION CTRY ST CALL    LAT     LON
11705 545110 99999 BEIJING - CAPITAL INTERNATIONAL AIRPORT   CH    ZBAA 40.080 116.585
11711 545270 99999                                 TIANJIN   CH         39.100 117.167
11712 545273 99999                                  BINHAI   CH    ZBTJ 39.124 117.346
11715 545340 99999                                TANGSHAN   CH         39.650 118.100
11687 544050 99999                                 HUAILAI   CH         40.417 115.500
11666 543080 99999                                FENGNING   CH         41.200 116.633
11689 544230 99999                                 CHENGDE   CH         40.967 117.917
11691 544360 99999                                QINGLONG   CH         40.400 118.950
11716 545390 99999                                  LETING   CH         39.433 118.900
11724 546020 99999                                 BAODING   CH         38.733 115.483
      ELEV(M)      BEGIN        END         code    longR      latR      dist
11705    35.4 1945-10-31 2015-06-12 545110-99999 2.034792 0.6995280  28.25299
11711     5.0 1956-08-20 2015-06-11 545270-99999 2.044950 0.6824237 102.66020
11712     3.0 1981-11-25 2015-06-12 545273-99999 2.048074 0.6828426 104.64104
11715    29.0 1956-08-20 2015-06-11 545340-99999 2.061234 0.6920230 109.61783
11687   538.0 1956-08-20 2015-06-12 544050-99999 2.015855 0.7054097 127.60780
11666   661.0 1957-06-01 2015-06-12 543080-99999 2.035630 0.7190757 135.32440
11689   423.0 1956-08-20 2015-06-12 544230-99999 2.058040 0.7150090 137.69215
11691   228.0 1957-06-02 2015-06-12 544360-99999 2.076069 0.7051130 179.69364
11716    12.0 1957-06-01 2015-06-11 545390-99999 2.075196 0.6882357 182.30889
11724    17.0 1956-08-20 2015-06-12 546020-99999 2.015559 0.6760184 186.23783
```

To obtain the data the user must supply a `code` (see above) and year or years of interest. For example, to download data for Heathrow Airport in 2010 (code 037720-99999):

```R
dat <- importNOAA(code = "037720-99999", year = 2010)
head(dat)
Source: local data frame [6 x 19]

                 date       ws          wd   air_temp sea_lev_press visibility dew_point
1 2010-01-01 00:00:00 3.266667  17.3515933 1.03333333        1001.1   16055.00 -1.900000
2 2010-01-01 01:00:00 3.100000   6.1317598 0.96666667        1001.4   14266.67 -1.866667
3 2010-01-01 02:00:00 3.100000  15.5928825 1.03333333        1001.5   15600.00 -1.866667
4 2010-01-01 03:00:00 2.933333  17.0497878 1.00000000        1001.6   16843.33 -2.000000
5 2010-01-01 04:00:00 2.766667   0.6056525 0.26666667        1001.9   15600.00 -2.433333
6 2010-01-01 05:00:00 2.433333 356.4417142 0.06666667        1002.1   15600.00 -2.866667
Variables not shown: RH (dbl), sky_ceiling (dbl), lat (dbl), long (dbl), elev (dbl), cl_1
  (dbl), cl_2 (dbl), cl_3 (dbl), cl (dbl), cl_1_height (dbl), cl_2_height (dbl), cl_3_height
  (dbl)
```
