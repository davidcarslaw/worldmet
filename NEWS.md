# worldmet (development version)

## Breaking Changes

- Removed `getMeta(plot = , returnMap = )`. These have been superseded by the `return` argument. `return` allows users to define whether they'd like to view a tabular view (the default) or a map view, which requires `{leaflet}`.

## New Features

- Numerous packages have been moved from Imports to Suggests or have been replaced with alternatives. This allows `{worldmet}` to be more lightweight by default.

    - `{leaflet}` has been moved from Imports to Suggests. Users will be prompted to install it if using `getMeta(return = "map")`.
    
    - `{foreach}`, `{doParallel}` and `{parallel}` have been moved from Imports to Suggests. Users will be prompted to install them if using `importNOAA(n.cores = )`.
    
    - `{readr}` has been replaced by its dependency `{vroom}`. There is no user-facing change associated with this.

## Bug Fixes

- fix join problem due to `{dplyr}` updates.

- fix spelling mistakes

# worldmet 0.9.6

## New Features

- `importNOAA()` will now display a progress bar when multiple years of met data are to be imported.

- Improved the formatting of the popups in `getMeta()` in line with `openairmaps::networkMap()`.

- Added the `provider` argument to `getMeta()`, allowing for users to define one or more `leaflet` basemaps. Uses the same default as `openairmaps::networkMap()`.

- `exportADMS()` now invisibly returns the input data, in line with, e.g., `readr::write_csv()`.

# worldmet 0.9.5

## Bug Fixes

- do not add precip to `exportADMS()`

- Cloud cover data mostly spans 1 to 8 Oktas and no zero (clear sky); use `ceil_hgt = 220000` (unlimited) to indicate clear sky when cloud cover is reported as missing.

- Do not use ftp for site information.

# worldmet 0.9.4

## Bug Fixes

- Fix precipitation. The `precip_12` gives the sum of the precipitation over the past 12 hours and the sum of this column should be the annual total in mm. `precip` spreads the 12-hour total evenly across the previous 12 hours. `{worldmet}` no longer tries to combine 12 and 6 hour totals.

# worldmet 0.9.3

## Bug Fixes

- Catch missing data error and report missing when importing data using `importNOAA()`

# worldmet 0.9.2

## New Features

- Exporting `weatherCodes` so that everything works when used by other 
packages through "explicit namespacing"" (_e.g._ `worldmet::importNOAA()`)
without having to `library(worldmet)`.

# worldmet 0.9.1

## Bug Fixes

- fix bug when lat and lon provided in `getMeta`
- fix bug when several years are selected and columns are different lengths when `n.core = 1`

# worldmet 0.9.1

## New Features

- Significant changes due to NOAA storage formats and different storage locations
- Remove options for precipitation and present weather in `importNOAA()`; just return everything
- Return data as `{tibble}`

# worldmet 0.8.8

## New Features

- export `getMetaLive()` to allow users direct access to all meta data and easy re-use without re-downloading.
- add option `path` to allow users to save met data as an rds file.
- deprecate `fresh` option in `getMeta()`.
- Assume `9999` is missing for visibility (was `999999`)

## Bug Fixes

- fix short WBAN codes.
- make sure all meta data are used and not only sites with most recent year

# worldmet 0.8.4

## New Features

- query live meta data when using `getMeta()`
- parallel processing for sites and years
- use `read_csv()` for meta data (`read.csv()` seems very slow in R 3.4.3)

## Bug Fixes

- fix bug when data not available for a year when using parallel processing

# worldmet 0.8.0

## New Features

- downloads now from webserver rather than ftp. Should be faster and allow more downloads. Thanks to Stuart Grange.
- add parallel processing using `{foreach}`

# worldmet 0.7.4

## Bug Fixes

- don't use `closeAllConnections()`

# worldmet 0.7.3

## New Features

- default to downloading fresh meta data each time
- update meta data

## Bug Fixes

- fix current year problem (base on meta data available in package)

# worldmet 0.7.2

## Bug Fixes

- make sure data are returned with `NA` when missing and not `NaN`

# worldmet 0.6 

## New Features

- Add ability to return precipitation measurements, if available.
- Add precipitation to `exportADMS()`

