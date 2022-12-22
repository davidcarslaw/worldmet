#' Export a meteorological data frame in ADMS format
#'
#' @param dat A data frame imported by [importNOAA()].
#' @param out A file name for the ADMS file. The file is written to the working
#'   directory by default.
#' @param interp Should interpolation of missing values be undertaken? If `TRUE`
#'   linear interpolation is carried out for gaps of up to and including
#'   `maxgap`.
#' @param maxgap The maximum gap in hours that should be interpolated where
#'   there are missing data when `interp = TRUE.` Data with gaps more than
#'   `maxgap` are left as missing.
#'
#' @return Writes a text file to a location of the user's choosing.
#' @export
#' @examples
#'
#' \dontrun{
#' ## import some data then export it
#' dat <- importNOAA(year = 2012)
#' exportADMS(dat, file = "~/temp/adms_met.MET")
#' }
exportADMS <- function(dat, out = "./ADMS_met.MET", interp = FALSE, maxgap = 2) {
  
  # keep R check quiet
  wd <- u <- v <- NULL
  
  ## make sure the data do not have gaps
  all.dates <- data.frame(
    date = seq(ISOdatetime(
      year = as.numeric(format(dat$date[1], "%Y")),
      month = 1, day = 1, hour = 0, min = 0,
      sec = 0, tz = "GMT"
    ),
    ISOdatetime(
      year = as.numeric(format(dat$date[1], "%Y")),
      month = 12, day = 31, hour = 23, min = 0,
      sec = 0, tz = "GMT"
    ),
    by = "hour"
    )
  )
  
  dat <- merge(dat, all.dates, all = TRUE)
  
  ## make sure precipitation is available
  if (!"precip" %in% names(dat)) {
    dat$precip <- NA
  }
  
  if (interp) {
    
    varInterp <- c("ws", "u", "v", "air_temp", "RH", "cl")
    
    # transform wd
    dat <- mutate(dat, u = sin(pi * wd / 180), 
                  v = cos(pi * wd / 180))
    
    for (variable in varInterp) {
      
      # if all missing, then don't interpolate
      if(all(is.na(dat[[variable]])))
        return()
      
      # first fill with linear interpolation
      filled <- stats::approx(dat$date, dat[[variable]], xout = dat$date, 
                              na.rm = TRUE, rule = 2, method = "linear")$y
      
      # find out length of missing data
      is_missing <- rle(is.na(dat[[variable]]))
      
      is_missing <- rep(ifelse(is_missing$values, is_missing$lengths, 0),
                        times = is_missing$lengths)
      
      id <- which(is_missing > maxgap)
      
      # update data frame
      dat[[variable]] <- filled
      dat[[variable]][id] <- NA
      
    }
    
    dat <- mutate(dat, wd = as.vector(atan2(u, v) * 360 / 2 / pi))
    
    ## correct for negative wind directions
    ids <- which(dat$wd < 0) ## ids where wd < 0
    dat$wd[ids] <- dat$wd[ids] + 360
    
    dat <- select(dat, -v, -u)
    
  }
  
  ## exports met data to ADMS format file
  year <- as.numeric(format(dat$date, "%Y"))
  day <- as.numeric(format(dat$date, "%j"))
  hour <- as.numeric(format(dat$date, "%H"))
  station <- "0000"
  
  # check if present
  if (!"cl" %in% names(dat)) dat$cl <- NA
  if (!"precip" %in% names(dat)) dat$precip <- NA
  
  ## data frame of met data needed
  adms <- data.frame(station, year, day, hour,
                     round(dat$air_temp, 1), round(dat$ws, 1),
                     round(dat$wd, 1), round(dat$RH, 1),
                     round(dat$cl), round(dat$precip, 1),
                     stringsAsFactors = FALSE
  )
  
  ## print key data capture rates to the screen
  dc <- round(100 - 100 * (length(which(is.na(dat$ws))) / length(dat$ws)), 1)
  print(paste("Data capture for wind speed:", dc, "%"))
  
  dc <- round(100 - 100 * (length(which(is.na(dat$wd))) / length(dat$wd)), 1)
  print(paste("Data capture for wind direction:", dc, "%"))
  
  dc <- round(100 - 100 * (length(which(is.na(dat$air_temp))) / length(dat$air_temp)), 1)
  print(paste("Data capture for temperature:", dc, "%"))
  
  dc <- round(100 - 100 * (length(which(is.na(dat$cl))) / length(dat$cl)), 1)
  print(paste("Data capture for cloud cover:", dc, "%"))
  
  ## replace NA with -999
  adms[] <- lapply(adms, function(x) replace(x, is.na(x), -999))
  
  ## write the data file
  write.table(adms,
              file = out, col.names = FALSE, row.names = FALSE,
              sep = ",", quote = FALSE
  )
  
  ## add the header lines
  fConn <- file(out, "r+")
  Lines <- readLines(fConn)
  writeLines(c(
    "VARIABLES:\n10\nSTATION DCNN\nYEAR\nTDAY\nTHOUR\nT0C\nU\nPHI\nRHUM\nCL\nP\nDATA:",
    Lines
  ), con = fConn)
  close(fConn)
}
