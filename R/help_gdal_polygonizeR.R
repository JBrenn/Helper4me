#' rasters into shape.
#'
#' Getting rasters into shape with gdal.
#'
#' @param x raster
#' @param outshape character, output shape file, default=NULL
#' @param gdalformat character, gdal format description, default="ESRI Shapefile"
#' @param pypath character, path to python script 'gdal_polygonize.py', default=NULL: R will search for script
#' @param readpoly boolean, raed polygon in R, default=TRUE
#' @param quiet 
#'
#' @return shape
#'
#' @examples
#' 
#'
#' @author Johannes Brenner \email{johannes.brenner@ufz.de}
#'
#' @references https://johnbaumgartner.wordpress.com/2012/07/26/getting-rasters-into-shape-from-r/
#'
#' @seealso
#'
#' @keywords
#'
#' @export help_gdal_polygonizeR
#'
## Define the function
help_gdal_polygonizeR <- function(x, outshape=NULL, gdalformat = 'ESRI Shapefile',
                                  pypath=NULL, readpoly=TRUE, quiet=TRUE) {
  if (isTRUE(readpoly)) require(rgdal)
  if (is.null(pypath)) {
    pypath <- Sys.which('gdal_polygonize.py')
  }
  if (!file.exists(pypath)) stop("Can't find gdal_polygonize.py on your system.")
  owd <- getwd()
  on.exit(setwd(owd))
  setwd(dirname(pypath))
  if (!is.null(outshape)) {
    outshape <- sub('\\.shp$', '', outshape)
    f.exists <- file.exists(paste(outshape, c('shp', 'shx', 'dbf'), sep='.'))
    if (any(f.exists))
      stop(sprintf('File already exists: %s',
                   toString(paste(outshape, c('shp', 'shx', 'dbf'),
                                  sep='.')[f.exists])), call.=FALSE)
  } else outshape <- tempfile()
  if (is(x, 'Raster')) {
    require(raster)
    writeRaster(x, {f <- tempfile(fileext='.tif')})
    rastpath <- normalizePath(f)
  } else if (is.character(x)) {
    rastpath <- normalizePath(x)
  } else stop('x must be a file path (character string), or a Raster object.')
  system2('python', args=(sprintf('"%1$s" "%2$s" -f "%3$s" "%4$s.shp"',
                                  pypath, rastpath, gdalformat, outshape)))
  if (isTRUE(readpoly)) {
    shp <- readOGR(dirname(outshape), layer = basename(outshape), verbose=!quiet)
    return(shp)
  }
  return(NULL)
}