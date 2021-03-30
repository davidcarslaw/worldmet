# worldmet 0.9.99

- Fix precipitation. The `precip_12` gives the sum of the precipitation over the past 12 hours and the sum of this column should be the annual total in mm. `precip` spreads the 12-hour total evenly across the previous 12 hours. `worldmet` no longer tries to combine 12 and 6 hour totals.

# worldmet 0.9.3

- Catch missing data error and report missing when importing data using `importNOAA`

# worldmet 0.9.2

- Exporting `weatherCodes` so that everything works when used by other 
packages through "explicit namespacing"" (_e.g._ `worldmet::importNOAA()`)
without having to `library(worldmet)`.

# worldmet 0.9.1

- fix bug when lat and lon provided in `getMeta`
- fix bug when several years are selected and columns are different lengths when `n.core = 1`

# worldmet 0.9.1

- Significant changes due to NOAA storage formats and different storage locations
- Remove options for precipitation and present weather in `importNOAA`; just return everything
- Return data as tibble

# worldmet 0.8.8

- export `getMetaLive` to allow users direct access to all meta data and easy re-use without re-downloading.
- deprecate `fresh` option in `getMeta`.
- Assume 9999 is missing for visibility (was 999999)
- add option `path` to allow users to save met data as an rds file.
- fix short WBAN codes.
- make sure all meta data are used and not only sites with most recent year

# worldmet 0.8.4

- query live meta data when using `getMeta`
- fix bug when data not available for a year when using parallel processing
- parallel processing for sites and years
- use `read_csv` for meta data (read.csv seems very slow in R 3.4.3)

# worldmet 0.8.0

- downloads now from webserver rather than ftp. Should be faster and allow more downloads. Thanks to Stuart Grange.
- add parallel processing using foreach

# worldmet 0.7.4

- don't use closeAllConnections()

# worldmet 0.7.3

- default to downloading fresh meta data each time
- fix current year problem (base on meta data available in package)
- update meta data

# worldmet 0.7.2

- make sure data are returned with `NA` when missing and not `NaN`

# worldmet 0.6 

- Add ability to return precipitation measurements, if available.
- Add precipitation to `exportADMS`

