\name{help_parseDateTime}
\alias{help_parseDateTime}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
parse to Date or Date.Time objects
}
\description{
parse character vector to Date (base::as.Date) or Date.Time (base::as.POSIXct or chron::chron) objects.
}
\usage{
help_parseDateTime(x.date, x.time, format,
                               parse.to="chron",
                               sep=" ", timezone="GMT")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x.date}{date character vector
}
  \item{x.time}{time character vector
}
  \item{format}{list containing datetime format, default: list(date=..., time=...), if format$time = NULL date representation (as.Date) will be used for parsing.
}
 \item{parse.to}{specifying datetime representation, that will be used for parsing, either chron (default) or as.POSIXct.
}
  \item{x.time}{timezone only necessary if POSIX representation used, default "GMT"
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
Johannes.Brenner, johannesbrenner@gmail.com
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x.time <- c("00:20:00", "00:40:00", "01:00:00")
x.date <- rep("2012-01-01",3)

# chron representation
help_parseDateTime(x.date, x.time,
                   parse.to="chron")

# POSIX representation
help_parseDateTime(x.date, x.time,
                   parse.to="POSIX",
                   timezone="GMT")

# Date representation
help_parseDateTime(x.date, x.time=NULL)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
