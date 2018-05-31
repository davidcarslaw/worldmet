# Changes in worldmet 0.8.99

- export `getMetaLive` to allow users direct access to all meta data and easy re-use without re-downloading.
- deprecate `fresh` option in `getMeta`.
- Assume 9999 is missing for visibility (was 999999)

# Changes in worldmet 0.8.4

- query live meta data when using `getMeta`
- fix bug when data not available for a year when using parallel processing
- parallel processing for sites and years
- use `read_csv` for meta data (read.csv seems very slow in R 3.4.3)

# Changes in worldmet 0.8.0

- downloads now from webserver rather than ftp. Should be faster and allow more downloads. Thanks to Stuart Grange.
- add parallel processing using foreach

# Changes in worldmet 0.7.4

- don't use closeAllConnections()

# Changes in worldmet 0.7.3

- default to downloading fresh meta data each time
- fix current year problem (base on meta data available in package)
- update meta data

# Changes in worldmet 0.7.2

- make sure data are returned with `NA` when missing and not `NaN`

# Changes in worldmet 0.6 

- Add ability to return precipitation measurements, if available.
- Add precipitation to `exportADMS`

