#' Create netCDF from zoo time series object.
#'
#' @param ts zoo time series object, named.
#'
#' @return NULL, saves netCDF file containing all variables of zoo time series in working directory
#'
#' @examples
#' library(zoo)
#'
#' # dummy time series
#' date <- seq(as.Date("2010-01-01"),as.Date("2016-01-01"),1)
#' ts_data <- cbind(rnorm(length(date)), rnorm(length(date)))
#' colnames(ts_data) <- c("A","B")
#'
#' ts_date  <- zoo(ts_data, date)
#' ts_POSIX <- zoo(ts_data, as.POSIXct(date))
#' ts_chron <- zoo(ts_data, as.chron(date))
#'
#' # dummy lat, lon matrix
#' lat = as.matrix(40, ncol=1); dimnames(lat) = list("x","y")
#' lon = as.matrix(5 , ncol=1); dimnames(lon) = list("x","y")
#'
#' help_ts2netcdf(ts = ts_date, unit = c("UNIT varA","UNIT varB"),
#'                latitude = lat, longitude = lon, name.nc="test_date")
#'
#' help_ts2netcdf(ts = ts_POSIX, unit = c("UNIT varA","UNIT varB"),
#'                latitude = lat, longitude = lon, name.nc="test_POSIX")
#'
#' help_ts2netcdf(ts = ts_chron, unit = c("UNIT varA","UNIT varB"),
#'                latitude = lat, longitude = lon, name.nc="test_chron")
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

help_ts2netcdf <- function(ts, unit, var.name, latitude, longitude, name.nc)
{
  # create new netCDF file in working dir
  nc <- RNetCDF::create.nc(filename = paste(name.nc,".nc",sep=""))

  # A dimension length is an arbitrary positive integer,
  # except that one dimension in a classic
  # or 64-bit offset netCDF dataset can have the length UNLIMITED.

  # time
  if (class(time(ts)) == "Date") {
    times <- as.numeric(time(ts))
  } else {
    if (class(time(ts))[2] == "POSIXt") {
      times <- as.numeric(time(ts)) / (3600 *24)
    } else {
      times <- as.numeric(time(ts))
    }
  }

  # set time dimension, unlimited
  RNetCDF::dim.def.nc(ncfile = nc, dimname = "time", unlim=TRUE)
  # set time variable definition
  RNetCDF::var.def.nc(ncfile = nc, varname = "time", vartype = "NC_DOUBLE", dimensions = "time")
  # put numeric/double time steps
  RNetCDF::var.put.nc(ncfile = nc, variable = "time", data = times)
  # set attributes
  # units defining origin, origin can be changed in global settings of zoo - POSIX
  RNetCDF::att.put.nc(nc, "time", "units", "NC_CHAR", paste("days since", as.Date(0, origin = lubridate::origin), "00:00:00", sep=" "))
  RNetCDF::att.put.nc(nc, "time", "standard_name", "NC_CHAR", "time")
  RNetCDF::att.put.nc(nc, "time", "long_name", "NC_CHAR", "time")
  RNetCDF::att.put.nc(nc, "time", "calendar", "NC_CHAR", "standard")
  RNetCDF::att.put.nc(nc, "time", "axis", "NC_CHAR", "T")

  #nv4
  RNetCDF::dim.def.nc(ncfile = nc, dimname = "nv4", dimlength=4)

  # set y-lat dimension, length: dim(lat)
  RNetCDF::dim.def.nc(ncfile = nc, dimname = "y", dimlength = dim(latitude)[1])

  # set x-lon dimension, length: dim(lon)
  RNetCDF::dim.def.nc(ncfile = nc, dimname = "x", dimlength = dim(longitude)[1])


  # latitude, longitude
  # variable definition
  # latitude, dim(x,y)
  RNetCDF::var.def.nc(ncfile = nc, varname = "lat", vartype = "NC_DOUBLE", dimensions = c("x","y"))
  # longitude, dim(x,y)
  RNetCDF::var.def.nc(ncfile = nc, varname = "lon", vartype = "NC_DOUBLE", dimensions = c("x","y"))
  # put latitude in degree
  RNetCDF::var.put.nc(ncfile = nc, variable = "lat", data = latitude)
  # put longitude in degree
  RNetCDF::var.put.nc(ncfile = nc, variable = "lon", data = longitude)



  RNetCDF::att.put.nc(nc, "lat", "units", "NC_CHAR", "degrees_north")
  RNetCDF::att.put.nc(nc, "lat", "standard_name", "NC_CHAR", "latitude")
  RNetCDF::att.put.nc(nc, "lat", "long_name", "NC_CHAR", "latitude")
  RNetCDF::att.put.nc(nc, "lat", "_CoordinateAxisType", "NC_CHAR", "Lat")

  RNetCDF::att.put.nc(nc, "lon", "units", "NC_CHAR", "degrees_east")
  RNetCDF::att.put.nc(nc, "lon", "standard_name", "NC_CHAR", "longitude")
  RNetCDF::att.put.nc(nc, "lon", "long_name", "NC_CHAR", "longitude")
  RNetCDF::att.put.nc(nc, "lon", "_CoordinateAxisType", "NC_CHAR", "Lon")

  # data
  # set na to -9999 in ts data
  data <- ifelse(is.na(coredata(ts)),-9999,coredata(ts))

  if (is.null(dim(data)[2])) {
    # define variable with st.name, dimensions x,y,time
    RNetCDF::var.def.nc(ncfile = nc, varname = var.name, vartype = "NC_DOUBLE",
                        dimensions = c("x","y","time"))
    # put data
    RNetCDF::var.put.nc(ncfile = nc, variable = var.name, data = data,
                        start = c(1,1,1), count=c(1,1,length(data)))
    # define attributes
    RNetCDF::att.put.nc(nc, var.name, "units", "NC_CHAR", unit)
    RNetCDF::att.put.nc(nc, var.name, "standard_name", "NC_CHAR", var.name)
    RNetCDF::att.put.nc(nc, var.name, "long_name", "NC_CHAR", var.name)
    RNetCDF::att.put.nc(nc, var.name, "missing_value", "NC_INT", -9999)
    RNetCDF::att.put.nc(nc, var.name, "coordinates", "NC_CHAR", "lat lon")
  } else {
    for (i in 1:dim(data)[2])
    {
      # define variable with st.name, dimensions x,y,time
      RNetCDF::var.def.nc(ncfile = nc, varname = names(ts)[i], vartype = "NC_DOUBLE",
                          dimensions = c("x","y","time"))
      # put data
      RNetCDF::var.put.nc(ncfile = nc, variable = names(ts)[i], data = data[,i],
                          start = c(1,1,1), count=c(1,1,dim(data)[1]))
      # define attributes
      RNetCDF::att.put.nc(nc, names(ts)[i], "units", "NC_CHAR", unit[i])
      RNetCDF::att.put.nc(nc, names(ts)[i], "standard_name", "NC_CHAR", names(ts)[i])
      RNetCDF::att.put.nc(nc, names(ts)[i], "long_name", "NC_CHAR", names(ts)[i])
      RNetCDF::att.put.nc(nc, names(ts)[i], "missing_value", "NC_INT", -9999)
      RNetCDF::att.put.nc(nc, names(ts)[i], "coordinates", "NC_CHAR", "lat lon")
    }
  }


  # close nc file
  RNetCDF::close.nc(nc)

  print( paste(name.nc,".nc", " created in workng directory.", sep="") )
}


