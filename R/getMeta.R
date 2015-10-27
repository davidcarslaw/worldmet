##' Get information on meteorological sites
##'
##' This function is primarily used to find a site code that can be
##' used to access data using \code{\link{importNOAA}}. Sites searches
##' of approximately 30,000 sites can be carried out based on the site
##' name and based on the nearest locations based on user-supplied
##' latitude and logitude.
##' @title Find a ISD site code and other meta data
##' @param site A site name search string e.g. \code{site =
##' "heathrow"}. The search strings and be partial and can be upper or
##' lower case e.g. \code{site = "HEATHR"}.
##' @param lat A latitude in decimal degrees to search. Takes the
##' values -90 to 90.
##' @param lon A longitude in decimal degrees to search. Takes values
##' -180 to 180. Negative numbers are west of the Greenwich meridian.
##' @param country The country code. This is a two letter code. For a
##' full listing see
##' \url{ftp://ftp.ncdc.noaa.gov/pub/data/noaa/country-list.txt}.
##' @param state The state code. This is a two letter code. 
##' @param n The number of nearest sites to search based on
##' \code{latitude} and \code{longitude}.
##' @param current If \code{current = TRUE} (the default) only sites
##' that have data up to the current year are included.
##' @param fresh Should the meta data be read from the NOAA server or
##' the \code{worldmet} package?. If \code{FALSE} (the default) it is
##' read from the package version, which is fast. If \code{TRUE} the
##' data are read from the NOAA server. Most of the time the default
##' should be acceptable as it is updated with each release of the
##' package.
##' @return A data frame is returned with all available meta data,
##' mostly importantly including a \code{code} that can be supplied to
##' \code{\link{importNOAA}}. If latitude and longitude searches are
##' made an approximate distance, \code{dist} in km is also returned.
##' @export
##' @author David Carslaw
##' @examples 
##' 
##' \dontrun{
##' ## search for sites with name beijing
##' getMeta(site = "beijing")
##' }
##' 
##' \dontrun{
##' ## search for near a specified lat/lon - near Beijing airport
##' ## returns 'n' nearest by default
##' getMeta(lat = 40, lon = 116.9)
##' }
getMeta <- function(site = "heathrow", lat = NA, lon = NA, country = NA, state = NA, n = 10,
                    current = TRUE, fresh = FALSE) {
    ## read the meta data
 
    ## download the file, else use the package version
    if (fresh) meta <- getMetaLive()

    ## if current year
    if (current) {
        id <- which(format(meta$END, "%Y") == format(Sys.Date(), "%Y"))
        meta <- meta[id, ]
    }
    
    ## search based on name of site
    if (!missing(site)) {
        ## search for station
        meta <- meta[grep(site, meta$STATION, ignore.case = TRUE), ]
        
    }

     ## search based on country codes
    if (!missing(country)) {
        ## search for country
        id <- which(meta$CTRY %in% country)
        meta <- meta[id, ]
               
    }
    
         ## search based on state codes
    if (!missing(state)) {
        ## search for state
        id <- which(meta$ST %in% state)
        meta <- meta[id, ]
               
    }
    
    ## approximate distance to site
    if (!missing(lat) && !missing(lon)) {
        r <- 6371 # radius of the Earth
        
        ## Coordinates need to be in radians
        meta$longR <- meta$LON * pi / 180
        meta$latR <- meta$LAT * pi / 180
        lon <- lon * pi / 180
        lat <- lat * pi / 180
        meta$dist <- acos(sin(lat) * sin(meta$latR) + cos(lat) * 
                              cos(meta$latR) * cos(meta$longR - lon)) * r

        ## sort and retrun top n nearest
        meta <- head(openair:::sortDataFrame(meta, key = "dist"), n)
        
    }
    
    return(meta)
    
}

getMetaLive <- function(...) {

    ## downloads the whole thing fresh
    meta <- read.csv("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv",
                     header = FALSE, skip = 21)

    closeAllConnections()

    ## names in the meta file
    names(meta) <- c("USAF", "WBAN","STATION", "CTRY", "ST", "CALL", "LAT",
                     "LON", "ELEV(M)", "BEGIN", "END")

    ## full character string of site id
    meta$USAF <- formatC(meta$USAF, width = 6, format = "d", flag = "0")

    ## start/end date of measurements
    meta$BEGIN <- as.Date(as.character(meta$BEGIN), format = "%Y%m%d")
    meta$END <- as.Date(as.character(meta$END), format = "%Y%m%d")

    ## code used to query data 
    meta$code <- paste(meta$USAF, meta$WBAN, sep = "-")
    
    return(meta)
}
