#' note that the ISD has been deprecated
#' @noRd
deprecate_isd <- function(){
  warning(
    "The Integrated Surface Database (ISD) has been deprecated by NOAA in favour of the Global Historical Climatology Network hourly (GHCNh). Please use `importGHCNh()`."
  )
}

#' Deprecated ISD import functions
#' 
#' @description
#' Up until 2024, NOAA supported the Integrated Surface Database (ISD). This has
#' been superseded by the Global Historical Climatology Network hourly (GHCNh).
#' The following three functions relate to the ISD, but have been superseded.
#' 
#' - `getMeta()` (obtains ISD metadata)
#' - `getMetaLive()` (obtains the "raw" form of the ISD metadata)
#' - `importNOAA()` (imports hourly meteorological data)
#' 
#' @rdname deprecated-isd
#' @param site A site name search string e.g. `site = "heathrow"`. The search
#'   strings and be partial and can be upper or lower case e.g. `site =
#'   "HEATHR"`.
#' @param lat A latitude in decimal degrees to search. Takes the values -90 to
#'   90.
#' @param lon A longitude in decimal degrees to search. Takes values -180 to
#'   180. Negative numbers are west of the Greenwich meridian.
#' @param country The country code. This is a two letter code. For a full
#'   listing see <https://www1.ncdc.noaa.gov/pub/data/noaa/isd-history.csv>.
#' @param state The state code. This is a two letter code.
#' @param n The number of nearest sites to search based on `latitude` and
#'   `longitude`.
#' @param end.year To help filter sites based on how recent the available data
#'   are. `end.year` can be "current", "any" or a numeric year such as 2016, or
#'   a range of years e.g. 1990:2016 (which would select any site that had an
#'   end date in that range. **By default only sites that have some data for the
#'   current year are returned**.
#' @param provider By default a map will be created in which readers may toggle
#'   between a vector base map and a satellite/aerial image. `provider` allows
#'   users to override this default; see
#'   \url{http://leaflet-extras.github.io/leaflet-providers/preview/} for a list
#'   of all base maps that can be used. If multiple base maps are provided, they
#'   can be toggled between using a "layer control" interface.
#' @param plot If `TRUE` will plot sites on an interactive leaflet map.
#' @param returnMap Should the leaflet map be returned instead of the meta data?
#'   Default is `FALSE`.
#' @export
getMeta <- function(site = "heathrow",
                    lat = NA,
                    lon = NA,
                    country = NA,
                    state = NA,
                    n = 10,
                    end.year = "current",
                    provider = c("OpenStreetMap", "Esri.WorldImagery"),
                    plot = TRUE,
                    returnMap = FALSE) {
  ## read the meta data
  
  ## download the file, else use the package version
  meta <- getMetaLive()
  
  # check year
  if (!any(end.year %in% c("current", "all"))) {
    if (!is.numeric(end.year)) {
      stop("end.year should be one of 'current', 'all' or a numeric 4-digit year such as 2016.")
    }
  }
  
  # we base the current year as the max available in the meta data
  if ("current" %in% end.year)
    end.year <-
      max(as.numeric(format(meta$END, "%Y")), na.rm = TRUE)
  if ("all" %in% end.year)
    end.year <- 1900:2100
  
  
  
  ## search based on name of site
  if (!missing(site)) {
    ## search for station
    meta <- meta[grep(site, meta$STATION, ignore.case = TRUE),]
  }
  
  ## search based on country codes
  if (!missing(country) && !is.na(country)) {
    ## search for country
    id <- which(meta$CTRY %in% toupper(country))
    meta <- meta[id,]
  }
  
  ## search based on state codes
  if (!missing(state)) {
    ## search for state
    id <- which(meta$ST %in% toupper(state))
    meta <- meta[id,]
  }
  
  # make sure no missing lat / lon
  id <- which(is.na(meta$LON))
  if (length(id) > 0)
    meta <- meta[-id,]
  
  id <- which(is.na(meta$LAT))
  if (length(id) > 0)
    meta <- meta[-id,]
  
  # filter by end year
  id <- which(format(meta$END, "%Y") %in% end.year)
  meta <- meta[id,]
  
  ## approximate distance to site
  if (!missing(lat) && !missing(lon)) {
    r <- 6371 # radius of the Earth
    
    ## Coordinates need to be in radians
    meta$longR <- meta$LON * pi / 180
    meta$latR <- meta$LAT * pi / 180
    LON <- lon * pi / 180
    LAT <- lat * pi / 180
    meta$dist <- acos(sin(LAT) * sin(meta$latR) + cos(LAT) *
                        cos(meta$latR) * cos(meta$longR - LON)) * r
    
    ## sort and retrun top n nearest
    meta <- utils::head(openair:::sortDataFrame(meta, key = "dist"), n)
  }
  
  dat <- rename(meta, latitude = LAT, longitude = LON)
  
  names(dat) <- tolower(names(dat))
  
  if (plot) {
    content <- paste(
      paste0("<b>", dat$station, "</b>"),
      paste("<hr><b>Code:</b>", dat$code),
      paste("<b>Start:</b>", dat$begin),
      paste("<b>End:</b>", dat$end),
      sep = "<br/>"
    )
    
    if ("dist" %in% names(dat)) {
      content <- paste(
        content,
        paste("<b>Distance:</b>", round(dat$dist, 1), "km"),
        sep = "<br/>"
      )
    }
    
    m <- leaflet::leaflet(dat)
    
    for (i in provider) {
      m <- leaflet::addProviderTiles(map = m, provider = i, group = i)
    }
    
    m <-
      leaflet::addMarkers(
        map = m,
        ~ longitude,
        ~ latitude,
        popup = content,
        clusterOptions = leaflet::markerClusterOptions()
      )
    
    if (!is.na(lat) && !is.na(lon)) {
      m <- leaflet::addCircles(
        map = m,
        lng = lon,
        lat = lat,
        weight = 20,
        radius = 200,
        stroke = TRUE,
        color = "red",
        popup = paste(
          "Search location",
          paste("Lat =", dat$latitude),
          paste("Lon =", dat$longitude),
          sep = "<br/>"
        )
      )
    }
    
    if (length(provider) > 1) {
      m <- 
        leaflet::addLayersControl(
          map = m,
          baseGroups = provider, 
          options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = FALSE)
        )
    }
    
    print(m)
  }
  
  
  if (returnMap)
    return(m)
  else
    return(dat)
}


#' @rdname deprecated-isd
#' 
#' @param ... Currently unused.
#'
#' @export
getMetaLive <- function(...) {
  ## downloads the whole thing fresh
  deprecate_isd()
  url <- "https://www1.ncdc.noaa.gov/pub/data/noaa/isd-history.csv"
  meta <- read_csv(
    url,
    skip = 21,
    col_names = FALSE,
    col_types = cols(
      X1 = col_character(),
      X2 = col_character(),
      X3 = col_character(),
      X4 = col_character(),
      X5 = col_character(),
      X6 = col_character(),
      X7 = col_double(),
      X8 = col_double(),
      X9 = col_double(),
      X10 = col_date(format = "%Y%m%d"),
      X11 = col_date(format = "%Y%m%d")
    ), 
    progress = FALSE
  )
  
  # if not available e.g. due to US Government shutdown, flag and exit
  # some header data may still be read, so check column number
  if (ncol(meta) == 1L)
    stop(
      "File not available, check \nhttps://www1.ncdc.noaa.gov/pub/data/noaa/ for potential server problems.",
      call. = FALSE
    )
  
  ## names in the meta file
  names(meta) <- c(
    "USAF",
    "WBAN",
    "STATION",
    "CTRY",
    "ST",
    "CALL",
    "LAT",
    "LON",
    "ELEV(M)",
    "BEGIN",
    "END"
  )
  
  ## full character string of site id
  meta$USAF <-
    formatC(meta$USAF,
            width = 6,
            format = "d",
            flag = "0")
  
  ## code used to query data
  meta$code <- paste0(meta$USAF, "-", meta$WBAN)
  
  return(meta)
}

# how to update meta data
# meta <- getMeta(end.year = "all")
# usethis::use_data(meta, overwrite = TRUE, internal = TRUE)
# usethis::use_data(meta, overwrite = TRUE)

#' @rdname deprecated-isd
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
#' @import readr tidyr dplyr
importNOAA <- function(code = "037720-99999", year = 2014,
                       hourly = TRUE,
                       n.cores = 1, quiet = FALSE, path = NA) {
  deprecate_isd()
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
    cl <- parallel::makeCluster(n.cores)
    doParallel::registerDoParallel(cl)
    
    dat <- foreach::foreach(
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
    
    parallel::stopCluster(cl)
  } else {
    dat <-
      purrr::pmap(site_process, getDat,
                  hourly = hourly, .progress = "Importing NOAA Data") %>%
      purrr::list_rbind()
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
  
  # function to supress timeAverage printing
  # (can't see option to turn it off)
  quiet <- function(x) { 
    sink(tempfile()) 
    on.exit(sink()) 
    invisible(force(x)) 
  } 
  
  ## location of data
  file.name <- paste0(
    "https://www.ncei.noaa.gov/data/global-hourly/access/",
    year, "/", gsub(pattern = "-", "", code), ".csv"
  )
  
  # suppress warnings because some fields might be missing in the list
  # Note that not all available data is returned - just what I think is most useful
  met_data <- try(suppressWarnings(read_csv(
    file.name,
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
    ),
    progress = FALSE
  )), silent = TRUE
  )
  
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
    met_data <-
      quiet(openair::timeAverage(
        met_data,
        avg.time = "hour",
        type = c("code", "station")
      ))
  }
  
  ## add pwc back in
  if (exists("pwc")) {
    met_data <- left_join(met_data, pwc, by = "date")
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

