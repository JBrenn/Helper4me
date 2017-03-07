#' Add projected coordinates to netCDF.
#' 
#' Add projected coordinates (in meter) to netCDF, by converting lat/lon (degree) representation.
#'
#' @param nc netCDF files.
#' @param to chracter, if "degree": convert from pojected to lat/lon (degree); if "proj" convert from lat/lon to projected (meter).
#' @param coordsys integer, EPSG coordinate system definition, default 23030.
#' @param varnames named character vector containing variable names, either for lat/lon or projected coordinates, default: c(x="lon",y="lat")
#' 
#' @return NULL, saves netCDF file containing projected and lat/lon coordinates
#'
#' @examples
#' 
#' @author Johannes Brenner \email{johannes.brenner@ufz.de}
#'
#' @references
#'
#' @seealso
#'
#' @keywords
#'
#' @export help_ts2netcdf
#' 
help_ncaddproj <- function(nc, to="proj", coordsys=23030, varnames = c(x="lon",y="lat"))
{
  for (i in nc)
  {
    print(paste(i))
    
    # open netCDF connection
    nc_con <- RNetCDF::open.nc(i, write = T)
    
    # retrieve x and y variable
    x <- RNetCDF::var.get.nc(ncfile = nc_con, variable = varnames["x"])
    y <- RNetCDF::var.get.nc(ncfile = nc_con, variable = varnames["y"])
    
    # retrieve proj4 coordinate system definition
    coordsys_ <- sp::CRS(paste("+init=epsg:", coordsys, sep=""))
    coordsys_ <- as.character(coordsys_)
    
    # from projected to lat/lon
    if (to == "degree")
    {
      # inverse projection
      xy <- rgdal::project(xy = cbind(c(x),c(y)), proj = coordsys_, inv=TRUE)
    }
    # from lat/lon to projected x/y
    if (to == "proj")
    {
      # projecting lat/lon
      xy <- rgdal::project(xy = cbind(c(x),c(y)), proj = coordsys_, inv=FALSE)
    }
    
    # convert new x/y in matrix
    x_ <- matrix(data = xy[,1], nrow = dim(x)[1], ncol = dim(x)[2], byrow = F)
    y_ <- matrix(data = xy[,2], nrow = dim(y)[1], ncol = dim(y)[2], byrow = F)
    
    # latitude, longitude
    # variable definition
    if (to == "degree")
    {
      # latitude, dim(x,y)
      RNetCDF::var.def.nc(ncfile = nc_con, varname = "lat", vartype = "NC_DOUBLE", dimensions = c("x","y"))
      # longitude, dim(x,y)
      RNetCDF::var.def.nc(ncfile = nc_con, varname = "lon", vartype = "NC_DOUBLE", dimensions = c("x","y"))
      # put latitude (degree) to netCDF
      RNetCDF::var.put.nc(ncfile = nc_con, variable = "lat", data = y)
      # put longitude (degree) to netCDF
      RNetCDF::var.put.nc(ncfile = nc_con, variable = "lon", data = x)
      
      # latitude attributes
      RNetCDF::att.put.nc(nc_con, "lat", "units", "NC_CHAR", "degrees_north")
      RNetCDF::att.put.nc(nc_con, "lat", "standard_name", "NC_CHAR", "latitude")
      RNetCDF::att.put.nc(nc_con, "lat", "long_name", "NC_CHAR", "latitude")
      RNetCDF::att.put.nc(nc_con, "lat", "_CoordinateAxisType", "NC_CHAR", "Lat")
      
      # longitude attributes
      RNetCDF::att.put.nc(nc_con, "lat", "units", "NC_CHAR", "degrees_east")
      RNetCDF::att.put.nc(nc_con, "lat", "standard_name", "NC_CHAR", "longitude")
      RNetCDF::att.put.nc(nc_con, "lat", "long_name", "NC_CHAR", "longitude")
      RNetCDF::att.put.nc(nc_con, "lat", "_CoordinateAxisType", "NC_CHAR", "Lon")
    }
    
    # x, y
    # variable definition
    if (to == "proj")
    {
      # y, dim(x,y)
      RNetCDF::var.def.nc(ncfile = nc_con, varname = "y", vartype = "NC_DOUBLE", dimensions = c("x","y"))
      # x, dim(x,y)
      RNetCDF::var.def.nc(ncfile = nc_con, varname = "x", vartype = "NC_DOUBLE", dimensions = c("x","y"))
      # put y (meter) in netCDF
      RNetCDF::var.put.nc(ncfile = nc_con, variable = "y", data = y_)
      # put x (meter) in netCDF
      RNetCDF::var.put.nc(ncfile = nc_con, variable = "x", data = x_)
      
      # x attributes
      RNetCDF::att.put.nc(nc_con, "x", "units", "NC_CHAR", "meter")
      RNetCDF::att.put.nc(nc_con, "x", "standard_name", "NC_CHAR", "x")
      RNetCDF::att.put.nc(nc_con, "x", "long_name", "NC_CHAR", "x")
      RNetCDF::att.put.nc(nc_con, "x", "_CoordinateAxisType", "NC_CHAR", "X")
      
      # y attributes
      RNetCDF::att.put.nc(nc_con, "y", "units", "NC_CHAR", "degrees_east")
      RNetCDF::att.put.nc(nc_con, "y", "standard_name", "NC_CHAR", "y")
      RNetCDF::att.put.nc(nc_con, "y", "long_name", "NC_CHAR", "y")
      RNetCDF::att.put.nc(nc_con, "y", "_CoordinateAxisType", "NC_CHAR", "Y")
    }
    
    RNetCDF::close.nc(nc_con)
  }
}