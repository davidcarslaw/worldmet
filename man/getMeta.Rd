\name{getMeta}
\alias{getMeta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getMeta(site = "heathrow", lat = NA, lon = NA, n = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{site}{
%%     ~~Describe \code{site} here~~
}
  \item{lat}{
%%     ~~Describe \code{lat} here~~
}
  \item{lon}{
%%     ~~Describe \code{lon} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
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
function (site = "heathrow", lat = NA, lon = NA, n = 10) 
{
    meta <- read.csv("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv", 
        header = FALSE, skip = 21)
    closeAllConnections()
    names(meta) <- c("USAF", "WBAN", "STATION", "CTRY", "ST", 
        "CALL", "LAT", "LON", "ELEV(M)", "BEGIN", "END")
    meta$USAF <- formatC(meta$USAF, width = 6, format = "d", 
        flag = "0")
    meta$BEGIN <- as.Date(as.character(meta$BEGIN), format = "\%Y\%m\%d")
    meta$END <- as.Date(as.character(meta$END), format = "\%Y\%m\%d")
    meta$code <- paste(meta$USAF, meta$WBAN, sep = "-")
    if (!missing(site)) {
        sub <- meta[grep(site, meta$STATION, ignore.case = TRUE), 
            ]
    }
    if (!missing(lat) && !missing(lon)) {
        r <- 6371
        meta$longR <- meta$LON * pi/180
        meta$latR <- meta$LAT * pi/180
        lon <- lon * pi/180
        lat <- lat * pi/180
        meta$dist <- acos(sin(lat) * sin(meta$latR) + cos(lat) * 
            cos(meta$latR) * cos(meta$longR - lon)) * r
        sub <- head(openair:::sortDataFrame(meta, key = "dist"), 
            n)
    }
    return(sub)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
