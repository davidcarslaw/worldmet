
importNOAA <- function(code = "037720-99999", year = 2014) {

    ## main web site https://www.ncdc.noaa.gov/isd

    ## formats document ftp://ftp.ncdc.noaa.gov/pub/data/noaa/ish-format-document.pdf

    ## gis map https://gis.ncdc.noaa.gov/map/viewer/#app=cdo&cfg=cdo&theme=hourly&layers=1
    
    require(openair)
    require(plyr)
    
    ## go through each of the years selected
    dat <- ldply(year, getDat, code = code)
    
    return(dat)
    
}

getDat <- function(code, year) {

    ## location of data
    file.name <- paste0("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/",
                        year, "/", code, "-", year, ".gz")
    
    z <- gzcon(url(file.name))

    ## read.table can only read from a text-mode connection.
    ## note the last set of data 'V32' is the additional data that contains cloud cover etc.
    raw <- textConnection(readLines(z))
    close(z)
    dat <- read.fwf(raw, header = FALSE, widths = c(4, 6, 5, 8, 4, 1, 6, 7, 5, 
                                                    5, 5, 4, 3, 1, 1, 4, 1, 5, 1, 
                                                    1, 1, 6, 1, 1, 1, 5, 1, 5, 1, 
                                                    5, 1, 1000))
    close(raw)

    ## make a POSIXct date, all dates are UTC
    dat$date <- paste(dat$V4, formatC(dat$V5, width = 4, format = "d", flag = "0"))
    dat$date <- as.POSIXct(strptime(dat$date, format = "%Y%m%d %H%M", tz = "GMT"),
                           tz = "GMT")

    ## names of variables
    names(dat)[1:31] <- c('var_length', 'usaf_id', 'wban', 'dateX', 'gmt', 'data_source',
                          'lat', 'long', 'report_type', 'elev', 'call_letters', 'qc_level',
                          'wd', 'wind_dir_flag', 'wind_type', 'ws', 'wind_speed_flag',
                          'sky_ceiling', 'sky_ceil_flag', 'sky_ceil_determ', 'sky_cavok',
                          'visibility', 'vis_flag', 'vis_var', 'vis_var_flag', 'air_temp',
                          'air_temp_flag', 'dew_point', 'dew_point_flag', 'sea_lev_press',
                          'sea_levp_flag')
    
    ## find and set missing
    id <- which(dat$wind_dir_flag == 9)
    dat$wd[id] <- NA
    
    id <- which(dat$wind_speed_flag == 9)
    dat$ws[id] <- NA
    
    id <- which(dat$sky_ceil_flag == 9)
    dat$sky_ceiling[id] <- NA
    
    id <- which(dat$vis_flag == 9)
    dat$visibility[id] <- NA
    
    id <- which(dat$air_temp_flag == 9)
    dat$air_temp[id] <- NA
    
    id <- which(dat$dew_point_flag == 9)
    dat$dew_point[id] <- NA
    
    id <- which(dat$sea_levp_flag == 9)
    dat$sea_lev_press[id] <- NA
    
    ## used for calms in openair
    id <- which(dat$ws == 0)
    dat$wd[id] <- 0
    
    ## sort out the units
    dat$lat <- dat$lat / 1000
    dat$long <- dat$long / 1000
    dat$ws <- dat$ws / 10
    dat$air_temp <- dat$air_temp / 10
    dat$sea_lev_press <- dat$sea_lev_press / 10
    dat$dew_point <- dat$dew_point / 10

    ## relative humidity - general formula based on T and dew point
    dat$RH <- 100 * ((112 - 0.1 * dat$air_temp + dat$dew_point) /
                       (112 + 0.9 * dat$air_temp)) ^ 8

    ## process the additional data separately
    dat <- procAddit(dat)
    
    ## select the variables we want
    dat <- subset(dat, select = c(date, ws, wd, air_temp, sea_lev_press, visibility,
                                  dew_point, RH, sky_ceiling, lat, long, elev,
                                  cl_1, cl_2, cl_3, cl_1_height, cl_2_height, cl_3_height))
    
    ## average to hourly
    dat <- timeAverage(dat, avg.time = "hour")

    return(dat)

}

getMeta <- function(site = "heathrow", lat = NA, lon = NA, n = 10) {
    ## read the meta data
 
    meta <- read.csv("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv", header = FALSE,
                     skip = 21)

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
    
    ## search based on name of site
    if (!missing(site)) {
        ## search for station
        sub <- meta[grep(site, meta$STATION, ignore.case = TRUE), ]
        
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
        sub <- head(openair:::sortDataFrame(meta, key = "dist"), n)
        
    }
    
    return(sub)
    
}

procAddit <- function(dat) {

    ## function to process additional data such as cloud cover

    ## consider first 3 layers of cloud GA1, GA2, GA3
    dat <- extractCloud(dat, "GA1", "cl_1")
    dat <- extractCloud(dat, "GA2", "cl_2")
    dat <- extractCloud(dat, "GA3", "cl_3")
    
}

extractCloud <- function(dat, field = "GA1", out = "cl_1") {

    ## 3 fields are used: GA1, GA2 and GA3

    height <- paste0(out, "_height") ## cloud height field

    ## fields that contain search string
    id <- grep(field, dat[ , "V32"])

    ## variables for cloud amount (oktas) and cloud height
    dat[[out]] <- NA
    dat[[height]] <- NA
    
    if (length(id) > 1) {
        
        ## location of begining of GA1 etc
        loc <- sapply(id, function (x) regexpr(field, dat[x, "V32"]))

        ## extract the variable
        cl <- sapply(seq_along(id), function (x)
            substr(dat$V32[id[x]], start = loc[x] + 3, stop = loc[x] + 4))
        cl <- as.numeric(cl)
        
        miss <- which(cl > 8) ## missing or obscured in some way
        if (length(id) > 0) cl[miss] <- NA

        ## and height of cloud
        h <- sapply(seq_along(id), function (x)
            substr(dat$V32[id[x]], start = loc[x] + 6, stop = loc[x] + 11))
        h <- as.numeric(h)

        dat[[out]][id] <- cl
        dat[[height]][id] <- h

    }
    
    return(dat)
    
}

