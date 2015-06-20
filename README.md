# worldmet - R package for accessing NOAA Integrated Surface Database (ISD) meteorological observations

[![Travis-CI Build Status](https://travis-ci.org/davidcarslaw/worldmet.svg?branch=master)](https://travis-ci.org/davidcarslaw/worldmet)

`worldmet` provides and easy way to access data from the [NOAA Integrated
Surface Database](https://www.ncdc.noaa.gov/isd) (ISD). The ISD contains detailed surface
meteorological data from around the world for over 30,000
locations. See also the
[map](https://gis.ncdc.noaa.gov/map/viewer/#app=cdo&cfg=cdo&theme=hourly&layers=1).

The package outputs (typically hourly meteorogical data) work very
well with the [openair](https://github.com/davidcarslaw/openair) package.

## Installation

Installation of `worldmet` from GitHub is easy using the `devtools`
package.

```R
require(devtools)
install_github('davidcarslaw/worldmet')
```

