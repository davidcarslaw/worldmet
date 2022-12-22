#' Main function for importing meteorological data
#'
#' This is the main function to import data from the NOAA Integrated Surface
#' Database (ISD). The ISD contains detailed surface meteorological data from
#' around the world for over 30,000 locations. For general information of the
#' ISD see
#' [https://www.ncei.noaa.gov/products/land-based-station/integrated-surface-database](https://www.ncei.noaa.gov/products/land-based-station/integrated-surface-database)
#' and the map here
#' [https://gis.ncdc.noaa.gov/maps/ncei](https://gis.ncdc.noaa.gov/maps/ncei).
#'
#' Note the following units for the main variables:
#'
#' \describe{
#'
#' \item{date}{Date/time in POSIXct format. **Note the time zone is GMT (UTC)
#' and may need to be adjusted to merge with other local data. See details
#' below.**}
#'
#' \item{latitude}{Latitude in decimal degrees (-90 to 90).}
#'
#' \item{longitude}{Longitude in decimal degrees (-180 to 180). Negative numbers
#' are west of the Greenwich Meridian.}
#'
#' \item{elevation}{Elevention of site in metres.}
#'
#' \item{wd}{Wind direction in degrees. 90 is from the east.}
#'
#' \item{ws}{Wind speed in m/s.}
#'
#' \item{ceil_hgt}{The height above ground level (AGL) of the lowest cloud or
#' obscuring phenomena layer aloft with 5/8 or more summation total sky cover,
#' which may be predominantly opaque, or the vertical visibility into a
#' surface-based obstruction.}
#'
#' \item{visibility}{The visibility in metres.}
#'
#' \item{air_temp}{Air temperature in degrees Celcius.}
#'
#' \item{dew_point}{The dew point temperature in degrees Celcius.}
#'
#' \item{atmos_pres}{The sea level pressure in millibars.}
#'
#' \item{RH}{The relative humidity (%).}
#'
#' \item{cl_1,  ...,  cl_3}{Cloud cover for different layers in Oktas (1-8).}
#'
#' \item{cl}{Maximum of cl_1 to cl_3 cloud cover in Oktas (1-8).}
#'
#' \item{cl_1_height, ..., cl_3_height}{Height of the cloud base for each later
#' in metres.}
#'
#' \item{precip_12}{12-hour precipitation in mm. The sum of this column should
#' give the annual precipitation.}
#'
#' \item{precip_6}{6-hour precipitation in mm.}
#'
#' \item{precip}{This calue of precipitation spreads the 12-hour total across
#' the previous 12 hours.}
#'
#'
#' \item{pwc}{The description of the present weather description (if
#' available).}
#'
#' }
#'
#' The data are returned in GMT (UTC). It may be necessary to adjust the time
#' zone when combining with other data. For example, if air quality data were
#' available for Beijing with time zone set to "Etc/GMT-8" (note the negative
#' offset even though Beijing is ahead of GMT. See the `openair` package and
#' manual for more details), then the time zone of the met data can be changed
#' to be the same. One way of doing this would be `attr(met$date, "tzone") <-
#' "Etc/GMT-8"` for a meteorological data frame called `met`. The two data sets
#' could then be merged based on `date`.
#'
#' @title Import meteorological data
#'
#' @param code The identifying code as a character string. The code is a
#'   combination of the USAF and the WBAN unique identifiers. The codes are
#'   separated by a \dQuote{-} e.g. `code = "037720-99999"`.
#' @param year The year to import. This can be a vector of years e.g. `year =
#'   2000:2005`.
#' @param hourly Should hourly means be calculated? The default is `TRUE`. If
#'   `FALSE` then the raw data are returned.
#' @param n.cores Number of cores to use for parallel processing. Default is 1
#'   and hence no parallelism.
#' @param quiet If FALSE, print missing sites / years to the screen.
#' @param path If a file path is provided, the data are saved as an rds file at
#'   the chosen location e.g.  `path = "C:/Users/David"`. Files are saved by
#'   year and site.
#' @export
#' @import openair
#' @import readr
#' @import tidyr
#' @import doParallel parallel foreach dplyr
#' @importFrom purrr pmap_dfr
#' @importFrom utils head write.table download.file
#' @importFrom leaflet addCircles addMarkers addTiles leaflet
#'   markerClusterOptions
#' @importFrom dplyr `%>%`
#' @return Returns a data frame of surface observations. The data frame is
#'   consistent for use with the [openair][openair::openair-package] package.
#'   NOTE! the data are returned in GMT (UTC) time zone format. Users may wish
#'   to express the data in other time zones, e.g., to merge with air pollution
#'   data. The [lubridate][lubridate::lubridate-package] package is useful in
#'   this respect.
#' @seealso [getMeta()] to obtain the codes based on various site search
#'   approaches.
#' @author David Carslaw
#' @examples
#'
#' \dontrun{
#' ## use Beijing airport code (see getMeta example)
#' dat <- importNOAA(code = "545110-99999", year = 2010:2011)
#' }
importNOAA <- function(code = "037720-99999", year = 2014,
                       hourly = TRUE,
                       n.cores = 1, quiet = FALSE, path = NA) {

  ## main web site https://www.ncei.noaa.gov/products/land-based-station/integrated-surface-database

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
  met_data <- try(suppressWarnings(read_csv(file.name,
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
  )), silent = TRUE)

  if (class(met_data)[1] == "try-error") {
    message(paste0("Missing data for site ", code, " and year ", year))
    met_data <- NULL
    return()
  }

  met_data <- rename(met_data,
    code = STATION,
    station = NAME,
    date = DATE,
    latitude = LATITUDE,
    longitude = LONGITUDE,
    elev = ELEVATION
  )

  met_data$code <- code

  # separate WND column

  if ("WND" %in% names(met_data)) {
    met_data <- separate(met_data, WND, into = c("wd", "x", "y", "ws", "z"))

    met_data <- mutate(met_data,
      wd = as.numeric(wd),
      wd = ifelse(wd == 999, NA, wd),
      ws = as.numeric(ws),
      ws = ifelse(ws == 9999, NA, ws),
      ws = ws / 10
    )
  }

  # separate TMP column
  if ("TMP" %in% names(met_data)) {
    met_data <- separate(met_data, TMP, into = c("air_temp", "flag_temp"), sep = ",")

    met_data <- mutate(met_data,
      air_temp = as.numeric(air_temp),
      air_temp = ifelse(air_temp == 9999, NA, air_temp),
      air_temp = air_temp / 10
    )
  }

  # separate VIS column
  if ("VIS" %in% names(met_data)) {
    met_data <- separate(met_data, VIS,
      into = c("visibility", "flag_vis1", "flag_vis2", "flag_vis3"),
      sep = ",", fill = "right"
    )

    met_data <- mutate(met_data,
      visibility = as.numeric(visibility),
      visibility = ifelse(visibility %in% c(9999, 999999), NA, visibility)
    )
  }

  # separate DEW column
  if ("DEW" %in% names(met_data)) {
    met_data <- separate(met_data, DEW, into = c("dew_point", "flag_dew"), sep = ",")

    met_data <- mutate(met_data,
      dew_point = as.numeric(dew_point),
      dew_point = ifelse(dew_point == 9999, NA, dew_point),
      dew_point = dew_point / 10
    )
  }
  # separate SLP column
  if ("SLP" %in% names(met_data)) {
    met_data <- separate(met_data, SLP,
      into = c("atmos_pres", "flag_pres"), sep = ",",
      fill = "right"
    )

    met_data <- mutate(met_data,
      atmos_pres = as.numeric(atmos_pres),
      atmos_pres = ifelse(atmos_pres %in% c(99999, 999999), NA, atmos_pres),
      atmos_pres = atmos_pres / 10
    )
  }

  # separate CIG (sky condition) column
  if ("CIG" %in% names(met_data)) {
    met_data <- separate(met_data, CIG,
      into = c("ceil_hgt", "flag_sky1", "flag_sky2", "flag_sky3"),
      sep = ",", fill = "right"
    )

    met_data <- mutate(met_data,
      ceil_hgt = as.numeric(ceil_hgt),
      ceil_hgt = ifelse(ceil_hgt == 99999, NA, ceil_hgt)
    )
  }


  ## relative humidity - general formula based on T and dew point
  met_data$RH <- 100 * ((112 - 0.1 * met_data$air_temp + met_data$dew_point) /
    (112 + 0.9 * met_data$air_temp))^8

  if ("GA1" %in% names(met_data)) {

    # separate GA1 (cloud layer 1 height, amount) column
    met_data <- separate(met_data, GA1,
      into = c("cl_1", "code_1", "cl_1_height", "code_2", "cl_1_type", "code_3"),
      sep = ","
    )

    met_data <- mutate(met_data,
      cl_1 = as.numeric(cl_1),
      cl_1 = ifelse((is.na(cl_1) & ceil_hgt == 22000), 0, cl_1),
      cl_1 = ifelse(cl_1 == 99, NA, cl_1),
      cl_1_height = as.numeric(cl_1_height),
      cl_1_height = ifelse(cl_1_height == 99999, NA, cl_1_height)
    )
  }

  if ("GA2" %in% names(met_data)) {
    met_data <- separate(met_data, GA2,
      into = c("cl_2", "code_1", "cl_2_height", "code_2", "cl_2_type", "code_3"),
      sep = ","
    )

    met_data <- mutate(met_data,
      cl_2 = as.numeric(cl_2),
      cl_2 = ifelse(cl_2 == 99, NA, cl_2),
      cl_2_height = as.numeric(cl_2_height),
      cl_2_height = ifelse(cl_2_height == 99999, NA, cl_2_height)
    )
  }

  if ("GA3" %in% names(met_data)) {
    met_data <- separate(met_data, GA3,
      into = c("cl_3", "code_1", "cl_3_height", "code_2", "cl_3_type", "code_3"),
      sep = ","
    )

    met_data <- mutate(met_data,
      cl_3 = as.numeric(cl_3),
      cl_3 = ifelse(cl_3 == 99, NA, cl_3),
      cl_3_height = as.numeric(cl_3_height),
      cl_3_height = ifelse(cl_3_height == 99999, NA, cl_3_height)
    )
  }

  ## for cloud cover, make new 'cl' max of 3 cloud layers
  if ("cl_3" %in% names(met_data)) {
    met_data$cl <- pmax(met_data$cl_1, met_data$cl_2, met_data$cl_3, na.rm = TRUE)
  }

  # PRECIP AA1
  if ("AA1" %in% names(met_data)) {
    met_data <- separate(met_data, AA1,
      into = c("precip_code", "precip_raw", "code_1", "code_2"),
      sep = ","
    )

    met_data <- mutate(met_data,
      precip_raw = as.numeric(precip_raw),
      precip_raw = ifelse(precip_raw == 9999, NA, precip_raw),
      precip_raw = precip_raw / 10
    )

    # deal with 6 and 12 hour precip
    id <- which(met_data$precip_code == "06")

    if (length(id) > 0) {
      met_data$precip_6 <- NA
      met_data$precip_6[id] <- met_data$precip_raw[id]
    }

    id <- which(met_data$precip_code == "12")

    if (length(id) > 0) {
      met_data$precip_12 <- NA
      met_data$precip_12[id] <- met_data$precip_raw[id]
    }
  }


  # weather codes, AW1

  if ("AW1" %in% names(met_data)) {
    met_data <- separate(met_data, AW1,
      into = c("pwc", "code_1"),
      sep = ",", fill = "right"
    )

    met_data <- left_join(met_data, worldmet::weatherCodes, by = "pwc")
    met_data <- select(met_data, -pwc) %>%
      rename(pwc = description)
  }

  ## select the variables we want
  met_data <- select(met_data, any_of(c(
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
  if ("pwc" %in% names(met_data) && hourly) {
    pwc <- met_data[c("date", "pwc")]
    pwc$date2 <- format(pwc$date, "%Y-%m-%d %H") ## nearest hour
    tmp <- pwc[which(!duplicated(pwc$date2)), ]
    dates <- as.POSIXct(paste0(unique(pwc$date2), ":00:00"), tz = "GMT")

    pwc <- data.frame(date = dates, pwc = tmp$pwc)
    PWC <- TRUE
  }

  ## average to hourly
  if (hourly) {
    met_data <- openair::timeAverage(met_data, avg.time = "hour", type = c("code", "station"))
  }

  ## add pwc back in
  if (exists("pwc")) {
    met_data <- left_join(met_data, pwc, by = "date", all = TRUE)
  }

  ## add precipitation - based on 12 HOUR averages, so work with hourly data

  ## spread out precipitation across each hour

  ## only do this if precipitation exists
  if ("precip_12" %in% names(met_data) && hourly) {

    ## make new precip variable
    met_data$precip <- NA

    ## id where there is 12 hour data
    id <- which(!is.na(met_data$precip_12))

    if (length(id) == 0L) {
      return()
    }

    id <- id[id > 11] ## make sure we don't run off beginning

    for (i in seq_along(id)) {
      met_data$precip[(id[i] - 11):id[i]] <- met_data$precip_12[id[i]] / 12
    }
  }

  # replace NaN with NA
  met_data[] <- lapply(met_data, function(x) {
    replace(x, is.nan(x), NA)
  })


  return(as_tibble(met_data))
}
