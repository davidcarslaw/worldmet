# Changes in worldmet 0.8.099

- query live meta data when using `getMeta`
- fix bug when data not available for a year when using parallel processing

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

