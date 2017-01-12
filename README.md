
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
##        USAF  WBAN  STATION CTRY ST CALL    LAT    LON ELEV(M)      BEGIN
## 1687 037720 99999 HEATHROW   UK    EGLL 51.478 -0.461    25.3 1948-12-01
##             END         code
## 1687 2016-07-04 037720-99999
```

Often we have a latitude / longitude of interest. A search can be made based on supplied decimal coordinates and the top `n` nearest sites are returned. The map shows the location searched by the user (red dot) and markers showing the nearest meteorological stations. Click on a station marker to obtain the code and other basic information.

``` r
## search for near a specified lat/lon - near Beijing airport
## returns 'n' nearest by default
info <- getMeta(lat = 40, lon = 116.9)
```

<!--html_preserve-->

<script type="application/json" data-for="htmlwidget-4aae08a8dcbdfe001f3a">{"x":{"calls":[{"method":"addTiles","args":["http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"maxNativeZoom":null,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"continuousWorld":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":null,"unloadInvisibleTiles":null,"updateWhenIdle":null,"detectRetina":false,"reuseTiles":false,"attribution":"&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap\u003c/a> contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA\u003c/a>"}]},{"method":"addMarkers","args":[[40.08,39.1,39.124,39.65,40.417,41.2,40.967,40.4,39.433,38.733],[116.585,117.167,117.346,118.1,115.5,116.633,117.917,118.95,118.9,115.483],null,null,null,{"clickable":true,"draggable":false,"keyboard":true,"title":"","alt":"","zIndexOffset":0,"opacity":1,"riseOnHover":false,"riseOffset":250},["BEIJING - CAPITAL INTERNATIONAL AIRPORT<br/>Code: 545110-99999<br/>Start: 1945-10-31<br/>End: 2016-07-04<br/>Distance (km) 28.3","TIANJIN<br/>Code: 545270-99999<br/>Start: 1956-08-20<br/>End: 2016-07-04<br/>Distance (km) 102.7","BINHAI<br/>Code: 545273-99999<br/>Start: 1981-11-25<br/>End: 2016-07-04<br/>Distance (km) 104.6","TANGSHAN<br/>Code: 545340-99999<br/>Start: 1956-08-20<br/>End: 2016-07-04<br/>Distance (km) 109.6","HUAILAI<br/>Code: 544050-99999<br/>Start: 1956-08-20<br/>End: 2016-07-04<br/>Distance (km) 127.6","FENGNING<br/>Code: 543080-99999<br/>Start: 1957-06-01<br/>End: 2016-07-04<br/>Distance (km) 135.3","CHENGDE<br/>Code: 544230-99999<br/>Start: 1956-08-20<br/>End: 2016-07-04<br/>Distance (km) 137.7","QINGLONG<br/>Code: 544360-99999<br/>Start: 1957-06-02<br/>End: 2016-07-04<br/>Distance (km) 179.7","LETING<br/>Code: 545390-99999<br/>Start: 1957-06-01<br/>End: 2016-07-04<br/>Distance (km) 182.3","BAODING<br/>Code: 546020-99999<br/>Start: 1956-08-20<br/>End: 2016-07-04<br/>Distance (km) 186.2"],null,null,null,null]},{"method":"addCircles","args":[40,116.9,200,null,null,{"lineCap":null,"lineJoin":null,"clickable":true,"pointerEvents":null,"className":"","stroke":true,"color":"red","weight":20,"opacity":0.5,"fill":true,"fillColor":"red","fillOpacity":0.2,"dashArray":null},"Search location<br/>Lat = 40<br/>Lon = 116.9",null,null]}],"limits":{"lat":[38.733,41.2],"lng":[115.483,118.95]}},"evals":[],"jsHooks":[]}</script>
<!--/html_preserve-->
To obtain the data the user must supply a `code` (see above) and year or years of interest. For example, to download data for Heathrow Airport in 2010 (code 037720-99999):

``` r
dat <- importNOAA(code = "037720-99999", year = 2010)
head(dat)
## # A tibble: 6 × 23
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
