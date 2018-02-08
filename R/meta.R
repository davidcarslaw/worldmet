##' ISD meta data
##'
##' This data frame consists of the meta data from the ISD from
##' \url{ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv}. It is
##' updated in the \code{worldmet} package for each release. Users can
##' access specific meta data from the most up to date file by using
##' the \code{\link{getMeta}} function. This data frame exists for
##' fast access.
##'
##' \code{meta} is supplied with the \code{worldmet} package and
##' consists of the following information.
##'
##' @name meta
##' @docType data
##' @format Data frame with 65533 observations (rows) on the following 10
##'   variables: \describe{
##' \item{USAF}{USAF site id.}
##'
##'  \item{WBAN}{WBAN id.}
##'
##'  \item{STATION}{Name of measurement site.}
##'
##' \item{CTRY}{Country code.}
##'
##' \item{ST}{State.}
##'
##' \item{CALL}{Airport id if available.}
##'
##' \item{LAT}{Latitude in decimal degrees.}
##'
##'   \item{LON}{Longitude in decimal degrees.}
##'
##' \item{ELEV(M)}{Elevation of site in metres.}
##'
##' \item{BEGIN}{Date the measurements started, in \code{Date} format.}
##'
##' \item{END}{Date the measurements ended, in \code{Date} format.}
##'
##' \item{code}{The unique code that is used in
##' \code{\link{importNOAA}} that is a combination of the USAF and
##' WBAN codes.}
##' }
##' @source \code{meta} was compiled from
##' \url{ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv}.
##'
##' @keywords datasets
##' @examples
##'
##'
##' ## basic structure
##' head(meta)
##'
##'
NULL
