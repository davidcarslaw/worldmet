\name{extractCloud}
\alias{extractCloud}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
extractCloud(dat, field = "GA1", out = "cl_1")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
%%     ~~Describe \code{dat} here~~
}
  \item{field}{
%%     ~~Describe \code{field} here~~
}
  \item{out}{
%%     ~~Describe \code{out} here~~
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
function (dat, field = "GA1", out = "cl_1") 
{
    height <- paste0(out, "_height")
    id <- grep(field, dat[, "V32"])
    dat[[out]] <- NA
    dat[[height]] <- NA
    if (length(id) > 1) {
        loc <- sapply(id, function(x) regexpr(field, dat[x, "V32"]))
        cl <- sapply(seq_along(id), function(x) substr(dat$V32[id[x]], 
            start = loc[x] + 3, stop = loc[x] + 4))
        cl <- as.numeric(cl)
        miss <- which(cl > 8)
        if (length(id) > 0) 
            cl[miss] <- NA
        h <- sapply(seq_along(id), function(x) substr(dat$V32[id[x]], 
            start = loc[x] + 6, stop = loc[x] + 11))
        h <- as.numeric(h)
        dat[[out]][id] <- cl
        dat[[height]][id] <- h
    }
    return(dat)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
