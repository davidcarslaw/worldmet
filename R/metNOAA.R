##' Main function for importing meteorological data
##'
##' This is the main function to import data from the NOAA Integrated Surface
##' Database (ISD). The ISD contains detailed surface meteorological data from
##' around the world for over 30,000 locations. For general information of the
##' ISD see \url{https://www.ncdc.noaa.gov/isd} and the map here
##' \url{https://gis.ncdc.noaa.gov/maps/ncei}.
##'
##' Note the following units for the main variables:
##'
##' \describe{
##'
##' \item{date}{Date/time in POSIXct format. \strong{Note the time zone is GMT
##' (UTC) and may need to be adjusted to merge with other local data. See
##' details below.}}
##'
##' \item{latitude}{Latitude in decimal degrees (-90 to 90).}
##'
##' \item{longitude}{Longitude in decimal degrees (-180 to 180). Negative
##' numbers are west of the Greenwich Meridian.}
##'
##' \item{elevation}{Elevention of site in metres.}
##'
##' \item{wd}{Wind direction in degrees. 90 is from the east.}
##'
##' \item{ws}{Wind speed in m/s.}
##'
##' \item{ceil_hgt}{The height above ground level (AGL) of the lowest cloud or
##' obscuring phenomena layer aloft with 5/8 or more summation total sky cover,
##' which may be predominantly opaque, or the vertical visibility into a
##' surface-based obstruction.}
##'
##' \item{visibility}{The visibility in metres.}
##'
##' \item{air_temp}{Air temperature in degrees Celcius.}
##'
##' \item{dew_point}{The dew point temperature in degrees Celcius.}
##'
##' \item{atmos_pres}{The sea level pressure in millibars.}
##'
##' \item{RH}{The relative humidity (\%).}
##'
##' \item{cl_1,  ...,  cl_3}{Cloud cover for different layers in Oktas (1-8).}
##'
##' \item{cl}{Maximum of cl_1 to cl_3 cloud cover in Oktas (1-8).}
##'
##' \item{cl_1_height, ..., cl_3_height}{Height of the cloud base for each later
##' in metres.}
##'
##' \item{precip_12}{12-hour precipitation in mm.}
##'
##' \item{precip_6}{6-hour precipitation in mm.}
##'
##' \item{precip}{Based on the 12 hourly and 6 hourly totals, \code{precip}}
##' spreads the 6-hourly totals across the previous 6-hours to provide an
##' indication of hourly precipitation.
##'
##' \item{pwc}{The description of the present weather description (if
##' available).}
##'
##' }
##'
##' The data are returned in GMT (UTC). It may be necessary to adjust the time
##' zone when comining with other data. For example, if air quality data were
##' available for Beijing with time zone set to "Etc/GMT-8" (note the negative
##' offset even though Beijing is ahead of GMT. See the \code{openair} package
##' and manual for more details), then the time zone of the met data can be
##' changed to be the same. One way of doing this would be \code{attr(met$date,
##' "tzone") <- "Etc/GMT-8"} for a meteorological data frame called \code{met}.
##' The two data sets could then be merged based on \code{date}.
##'
##' @title Import meteorological data
##'
##' @param code The identifying code as a character string. The code is a
##'   combination of the USAF and the WBAN unique identifiers. The codes are
##'   separated by a \dQuote{-} e.g. \code{code = "037720-99999"}.
##' @param year The year to import. This can be a vector of years e.g.
##'   \code{year = 2000:2005}.
##' @param hourly Should hourly means be calculated? The default is \code{TRUE}.
##'   If \code{FALSE} then the raw data are returned.
##' @param n.cores Number of cores to use for parallel processing. Default is 1
##'   and hence no parallelism.
##' @param quiet If FALSE, print missing sites / years to the screen.
##' @param path If a file path is provided, the data are saved as an rds file at
##'   the chosen location e.g.  \code{path = "C:/Users/David"}. Files are saved
##'   by year and site.
##' @export
##' @import openair
##' @import readr
##' @import tidyr
##' @import doParallel parallel foreach dplyr
##' @importFrom purrr pmap_dfr
##' @importFrom utils head write.table download.file
##' @importFrom leaflet addCircles addMarkers addTiles leaflet
##'   markerClusterOptions
##' @importFrom dplyr `%>%`
##' @return Returns a data frame of surface observations. The data frame is
##'   consistent for use with the \code{openair} package. NOTE! the data are
##'   returned in GMT (UTC) time zone format. Users may wish to express the data
##'   in other time zones e.g. to merge with air pollution data.
##' @seealso \code{\link{getMeta}} to obtain the codes based on various site
##'   search approaches.
##' @author David Carslaw
##' @examples
##'
##' \dontrun{
##' ## use Beijing airport code (see getMeta example)
##' dat <- importNOAA(code = "545110-99999", year = 2010:2011)
##' }
importNOAA <- function(code = "037720-99999", year = 2014,
                       hourly = TRUE,
                       n.cores = 1, quiet = FALSE, path = NA) {

  ## main web site https://www.ncdc.noaa.gov/isd

  ## formats document https://www.ncei.noaa.gov/data/global-hourly/doc/isd-format-document.pdf
  
  # brief csv file description https://www.ncei.noaa.gov/data/global-hourly/doc/CSV_HELP.pdf

  ## gis map https://gis.ncdc.noaa.gov/map/viewer/#app=cdo&cfg=cdo&theme=hourly&layers=1

  ## go through each of the years selected, use parallel processing

  i <- station <- . <- NULL

  # sites and years to process
  site_process <- expand.grid(
    code = code,
    year = year,
    stringsAsFactors = FALSE
  )

  if (n.cores > 1) {
    cl <- makeCluster(n.cores)
    registerDoParallel(cl)

    dat <- foreach(
      i = 1:nrow(site_process),
      .combine = "bind_rows",
      .export = "getDat",
      .errorhandling = "remove"
    ) %dopar%
      getDat(
        year = site_process$year[i],
        code = site_process$code[i],
        hourly = hourly
      )

    stopCluster(cl)
  } else {
    
    dat <- pmap_dfr(site_process, getDat, hourly = hourly)
    
  }

  if (is.null(dat) || nrow(dat) == 0) {
    print("site(s) do not exist.")
    return()
  }

  # check to see what is missing and print to screen
  actual <- select(dat, code, date, station) %>%
    mutate(year = as.numeric(format(date, "%Y"))) %>%
    group_by(code, year) %>%
    slice(1)

  actual <- left_join(site_process, actual, by = c("code", "year"))

  if (length(which(is.na(actual$date))) > 0 && !quiet) {
    print("The following sites / years are missing:")
    print(filter(actual, is.na(date)))
  }

  if (!is.na(path)) {
    if (!dir.exists(path)) {
      warning("Directory does not exist, file not saved", call. = FALSE)
      return()
    }

    # save as year / site files
    writeMet <- function(dat) {
      saveRDS(dat, paste0(path, "/", unique(dat$code), "_", unique(dat$year), ".rds"))
      return(dat)
    }

    mutate(dat, year = format(date, "%Y")) %>%
      group_by(code, year) %>%
      do(writeMet(.))
  }

  return(dat)
}

getDat <- function(code, year, hourly) {

  ## location of data
  file.name <- paste0(
    "https://www.ncei.noaa.gov/data/global-hourly/access/",
    year, "/", gsub(pattern = "-", "", code), ".csv"
  )

  # suppress warnings because some fields might be missing in the list
  # Note that not all available data is returned - just what I think is most useful
  dat <- try(suppressWarnings(read_csv(file.name,  
                                       col_types = cols_only(
                                         STATION = col_character(),
                                         DATE = col_datetime(format = ""),
                                         SOURCE = col_double(),
                                         LATITUDE = col_double(),
                                         LONGITUDE = col_double(),
                                         ELEVATION = col_double(),
                                         NAME = col_character(),
                                         REPORT_TYPE = col_character(),
                                         CALL_SIGN = col_double(),
                                         QUALITY_CONTROL = col_character(),
                                         WND = col_character(),
                                         CIG = col_character(),
                                         VIS = col_character(),
                                         TMP = col_character(),
                                         DEW = col_character(),
                                         SLP = col_character(),
                                         AA1 = col_character(),
                                         AW1 = col_character(),
                                         GA1 = col_character(),
                                         GA2 = col_character(),
                                         GA3 = col_character()
                                       )
  )
  ), silent = TRUE)
  
  if (class(dat)[1] == "try-error") {
    
    message(paste0("Missing data for site ", code, " and year ", year))
    dat <- NULL
    return()
    
  }

  dat <- rename(dat,
    code = STATION,
    station = NAME,
    date = DATE,
    latitude = LATITUDE,
    longitude = LONGITUDE,
    elev = ELEVATION
  )

  dat$code <- code

  # separate WND column

  if ("WND" %in% names(dat)) {
    dat <- separate(dat, WND, into = c("wd", "x", "y", "ws", "z"))

    dat <- mutate(dat,
      wd = as.numeric(wd),
      wd = ifelse(wd == 999, NA, wd),
      ws = as.numeric(ws),
      ws = ifelse(ws == 9999, NA, ws),
      ws = ws / 10
    )
  }

  # separate TMP column
  if ("TMP" %in% names(dat)) {
    dat <- separate(dat, TMP, into = c("air_temp", "flag_temp"), sep = ",")

    dat <- mutate(dat,
      air_temp = as.numeric(air_temp),
      air_temp = ifelse(air_temp == 9999, NA, air_temp),
      air_temp = air_temp / 10
    )
  }

  # separate VIS column
  if ("VIS" %in% names(dat)) {
    dat <- separate(dat, VIS,
      into = c("visibility", "flag_vis1", "flag_vis2", "flag_vis3"),
      sep = ",", fill = "right"
    )

    dat <- mutate(dat,
      visibility = as.numeric(visibility),
      visibility = ifelse(visibility %in% c(9999, 999999), NA, visibility)
    )
  }

  # separate DEW column
  if ("DEW" %in% names(dat)) {
    dat <- separate(dat, DEW, into = c("dew_point", "flag_dew"), sep = ",")

    dat <- mutate(dat,
      dew_point = as.numeric(dew_point),
      dew_point = ifelse(dew_point == 9999, NA, dew_point),
      dew_point = dew_point / 10
    )
  }
  # separate SLP column
  if ("SLP" %in% names(dat)) {
    dat <- separate(dat, SLP,
      into = c("atmos_pres", "flag_pres"), sep = ",",
      fill = "right"
    )

    dat <- mutate(dat,
      atmos_pres = as.numeric(atmos_pres),
      atmos_pres = ifelse(atmos_pres %in% c(99999, 999999), NA, atmos_pres),
      atmos_pres = atmos_pres / 10
    )
  }

  # separate CIG (sky condition) column
  if ("CIG" %in% names(dat)) {
    dat <- separate(dat, CIG,
      into = c("ceil_hgt", "flag_sky1", "flag_sky2", "flag_sky3"),
      sep = ",", fill = "right"
    )

    dat <- mutate(dat,
      ceil_hgt = as.numeric(ceil_hgt),
      ceil_hgt = ifelse(ceil_hgt == 99999, NA, ceil_hgt)
    )
  }


  ## relative humidity - general formula based on T and dew point
  dat$RH <- 100 * ((112 - 0.1 * dat$air_temp + dat$dew_point) /
    (112 + 0.9 * dat$air_temp))^8

  if ("GA1" %in% names(dat)) {

    # separate GA1 (cloud layer 1 height, amount) column
    dat <- separate(dat, GA1,
      into = c("cl_1", "code_1", "cl_1_height", "code_2", "cl_1_type", "code_3"),
      sep = ","
    )

    dat <- mutate(dat,
      cl_1 = as.numeric(cl_1),
      cl_1 = ifelse(cl_1 == 99, NA, cl_1),
      cl_1_height = as.numeric(cl_1_height),
      cl_1_height = ifelse(cl_1_height == 99999, NA, cl_1_height)
    )
  }

  if ("GA2" %in% names(dat)) {
    dat <- separate(dat, GA2,
      into = c("cl_2", "code_1", "cl_2_height", "code_2", "cl_2_type", "code_3"),
      sep = ","
    )

    dat <- mutate(dat,
      cl_2 = as.numeric(cl_2),
      cl_2 = ifelse(cl_2 == 99, NA, cl_2),
      cl_2_height = as.numeric(cl_2_height),
      cl_2_height = ifelse(cl_2_height == 99999, NA, cl_2_height)
    )
  }

  if ("GA3" %in% names(dat)) {
    dat <- separate(dat, GA3,
      into = c("cl_3", "code_1", "cl_3_height", "code_2", "cl_3_type", "code_3"),
      sep = ","
    )

    dat <- mutate(dat,
      cl_3 = as.numeric(cl_3),
      cl_3 = ifelse(cl_3 == 99, NA, cl_3),
      cl_3_height = as.numeric(cl_3_height),
      cl_3_height = ifelse(cl_3_height == 99999, NA, cl_3_height)
    )
  }

  ## for cloud cover, make new 'cl' max of 3 cloud layers
  if ("cl_3" %in% names(dat)) {
    dat$cl <- pmax(dat$cl_1, dat$cl_2, dat$cl_3, na.rm = TRUE)
  }

  # PRECIP AA1
  if ("AA1" %in% names(dat)) {
    dat <- separate(dat, AA1,
      into = c("precip_code", "precip", "code_1", "code_2"),
      sep = ","
    )

    dat <- mutate(dat,
      precip = as.numeric(precip),
      precip = ifelse(precip == 9999, NA, precip)
    )

    # deal with 6 and 12 hour precip
    id <- which(dat$precip_code == "06")

    if (length(id) > 0) {
      dat$precip_6 <- NA
      dat$precip_6[id] <- dat$precip[id]
    }

    id <- which(dat$precip_code == "12")

    if (length(id) > 0) {
      dat$precip_12 <- NA
      dat$precip_12[id] <- dat$precip[id]
    }
  }

  ## add precipitation

  ## spread out precipitation across each hour
  ## met data gives 12 hour total and every other 6 hour total

  ## only do this if precipitation exists
  if (all(c("precip_6", "precip_12") %in% names(dat))) {

    ## make new precip variable
    dat$precip <- NA

    ## id where there is 6 hour data
    id <- which(!is.na(dat$precip_6))
    id <- id[id < (nrow(dat) - 6)] ## make sure we don't run off end

    ## calculate new 6 hour based on 12 hr total - 6 hr total
    dat$precip_6[id + 6] <- dat$precip_12[id + 6] - dat$precip_6[id]

    ## ids for new 6 hr totals
    id <- which(!is.na(dat$precip_6))
    id <- id[id > 6]

    ## Divide 6 hour total over each of 6 hours
    for (i in seq_along(id)) {
      dat$precip[(id[i] - 5):id[i]] <- dat$precip_6[id[i]] / 6
    }
  }


  # weather codes, AW1

  if ("AW1" %in% names(dat)) {
    dat <- separate(dat, AW1,
      into = c("pwc", "code_1"),
      sep = ",", fill = "right"
    )

    dat <- left_join(dat, worldmet::weatherCodes, by = "pwc")
    dat <- select(dat, -pwc) %>%
      rename(pwc = description)
  }

  ## select the variables we want
  dat <- select(dat, any_of(c(
    "date", "code", "station", "latitude", "longitude", "elev",
    "ws", "wd", "air_temp", "atmos_pres",
    "visibility", "dew_point", "RH",
    "ceil_hgt",
    "cl_1", "cl_2", "cl_3", "cl",
    "cl_1_height", "cl_2_height",
    "cl_3_height", "pwc", "precip_12",
    "precip_6", "precip"
  )))


  ## present weather is character and cannot be averaged, take first
  if ("pwc" %in% names(dat) && hourly) {
    pwc <- dat[c("date", "pwc")]
    pwc$date2 <- format(pwc$date, "%Y-%m-%d %H") ## nearest hour
    tmp <- pwc[which(!duplicated(pwc$date2)), ]
    dates <- as.POSIXct(paste0(unique(pwc$date2), ":00:00"), tz = "GMT")

    pwc <- data.frame(date = dates, pwc = tmp$pwc)
    PWC <- TRUE
  }

  ## average to hourly
  if (hourly) {
    dat <- openair::timeAverage(dat, avg.time = "hour", type = c("code", "station"))
  }

  ## add pwc back in
  if (exists("pwc")) {
    dat <- left_join(dat, pwc, by = "date", all = TRUE)
  }



  # replace NaN with NA
  dat[] <- lapply(dat, function(x) {
    replace(x, is.nan(x), NA)
  })


  return(as_tibble(dat))
}
