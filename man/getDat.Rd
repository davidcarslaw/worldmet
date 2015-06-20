\name{getDat}
\alias{getDat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getDat(code, year)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{code}{
%%     ~~Describe \code{code} here~~
}
  \item{year}{
%%     ~~Describe \code{year} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (code, year) 
{
    file.name <- paste0("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/", 
        year, "/", code, "-", year, ".gz")
    z <- gzcon(url(file.name))
    raw <- textConnection(readLines(z))
    close(z)
    dat <- read.fwf(raw, header = FALSE, widths = c(4, 6, 5, 
        8, 4, 1, 6, 7, 5, 5, 5, 4, 3, 1, 1, 4, 1, 5, 1, 1, 1, 
        6, 1, 1, 1, 5, 1, 5, 1, 5, 1, 1000))
    close(raw)
    dat$date <- paste(dat$V4, formatC(dat$V5, width = 4, format = "d", 
        flag = "0"))
    dat$date <- as.POSIXct(strptime(dat$date, format = "\%Y\%m\%d \%H\%M", 
        tz = "GMT"), tz = "GMT")
    names(dat)[1:31] <- c("var_length", "usaf_id", "wban", "dateX", 
        "gmt", "data_source", "lat", "long", "report_type", "elev", 
        "call_letters", "qc_level", "wd", "wind_dir_flag", "wind_type", 
        "ws", "wind_speed_flag", "sky_ceiling", "sky_ceil_flag", 
        "sky_ceil_determ", "sky_cavok", "visibility", "vis_flag", 
        "vis_var", "vis_var_flag", "air_temp", "air_temp_flag", 
        "dew_point", "dew_point_flag", "sea_lev_press", "sea_levp_flag")
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
    id <- which(dat$ws == 0)
    dat$wd[id] <- 0
    dat$lat <- dat$lat/1000
    dat$long <- dat$long/1000
    dat$ws <- dat$ws/10
    dat$air_temp <- dat$air_temp/10
    dat$sea_lev_press <- dat$sea_lev_press/10
    dat$dew_point <- dat$dew_point/10
    dat$RH <- 100 * ((112 - 0.1 * dat$air_temp + dat$dew_point)/(112 + 
        0.9 * dat$air_temp))^8
    dat <- procAddit(dat)
    dat <- subset(dat, select = c(date, ws, wd, air_temp, sea_lev_press, 
        visibility, dew_point, RH, sky_ceiling, lat, long, elev, 
        cl_1, cl_2, cl_3, cl_1_height, cl_2_height, cl_3_height))
    dat <- timeAverage(dat, avg.time = "hour")
    return(dat)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
