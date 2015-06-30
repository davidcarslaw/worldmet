#' Export a meteorological data frame in ADMS format
#'
#' @param dat A data frame imported by \code{\link{importNOAA}}.
#' @param out A file name for the ADMS file. The file is written in the working directory by default.
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
exportADMS <- function(dat, out = "./ADMS_met.MET") {
  
    ## exports met data to ADMS format file
    year <- as.numeric(format(dat$date, "%Y"))
    day <- as.numeric(format(dat$date, "%j"))
    hour <- as.numeric(format(dat$date, "%H"))
    station <- "0000"
    
    adms <- data.frame(station, year, day, hour, round(dat$air_temp, 1), 
                       round(dat$ws, 1), round(dat$wd, 1), round(dat$RH, 1),
                       as.integer(dat$cl))
    
    ## replace NA with -999
    adms[] <- suppressWarnings(lapply(adms, function(x) replace(x, is.na(x), -999)))
    
    write.table(adms, file = out, col.names = FALSE, row.names = FALSE, sep = ",", quote = FALSE)
    
    fConn <- file(out, 'r+') 
    Lines <- readLines(fConn) 
    writeLines(c("VARIABLES:\n9\nSTATION DCNN\nYEAR\nTDAY\nTHOUR\nT0C\nU\nPHI\nRH\nCL\nDATA:", Lines), con = fConn) 
    close(fConn) 
  
}