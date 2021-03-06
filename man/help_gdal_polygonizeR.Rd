% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/help_gdal_polygonizeR.R
\name{help_gdal_polygonizeR}
\alias{help_gdal_polygonizeR}
\title{rasters into shape.}
\usage{
help_gdal_polygonizeR(x, outshape = NULL,
  gdalformat = "ESRI Shapefile", pypath = NULL, readpoly = TRUE,
  quiet = TRUE)
}
\arguments{
\item{x}{raster}

\item{outshape}{character, output shape file, default=NULL}

\item{gdalformat}{character, gdal format description, default="ESRI Shapefile"}

\item{pypath}{character, path to python script 'gdal_polygonize.py', default=NULL: R will search for script}

\item{readpoly}{boolean, raed polygon in R, default=TRUE}

\item{quiet}{}
}
\value{
shape
}
\description{
Getting rasters into shape with gdal.
}
\examples{


}
\references{
https://johnbaumgartner.wordpress.com/2012/07/26/getting-rasters-into-shape-from-r/
}
\seealso{

}
\author{
Johannes Brenner \email{johannes.brenner@ufz.de}
}
\keyword{}
