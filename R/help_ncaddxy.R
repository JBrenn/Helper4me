#' Add projected coordinates to netCDF.
#'
#' Add one-dimensional projected coordinates x(x) and y(y) (in meter) to netCDF using information from header.txt.
#'
#' @param nc netCDF files.
#' @param header header file name, default="header.txt".
#'
#' @return NULL, saves netCDF file containing x(x) and y(y) variables.
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
#' @export help_ncaddxy
#'
help_ncaddxy <- function(nc, header="header.txt")
{
  # read header
    print(paste(nc))

    # open netCDF connection
    nc_con <- RNetCDF::open.nc(nc, write = T)

    # retrieve x and y dimension info
    x <- RNetCDF::dim.inq.nc(ncfile = nc_con, dimension = "x")
    y <- RNetCDF::dim.inq.nc(ncfile = nc_con, dimension = "y")

    # x, y lenght
    x_len <- x$length
    y_len <- y$length

    # retrieve header data info
    head <- readr::read_lines(header)
    ncols_h <- as.integer(substr(head[1], 6, nchar(head[1])))
    rows_h  <- as.integer(substr(head[2], 6, nchar(head[2])))
    xllcorner_h <- as.numeric(substr(head[3], 10, nchar(head[3])))
    yllcorner_h <- as.numeric(substr(head[4], 10, nchar(head[4])))
    cellsize_h  <- as.numeric(substr(head[5], 9, nchar(head[5])))

    # calculate x(x), y(y)
    xx <- seq(from = xllcorner_h+.5*cellsize_h, by = cellsize_h, length.out = x_len)
    yy <- seq(from = yllcorner_h+.5*cellsize_h, by = cellsize_h, length.out = y_len)

    # x(x), y(y)
    # variable definition
    # x, dim(x)
    RNetCDF::var.def.nc(ncfile = nc_con, varname = "x", vartype = "NC_DOUBLE", dimensions = c("x"))
    # y, dim(y)
    RNetCDF::var.def.nc(ncfile = nc_con, varname = "y", vartype = "NC_DOUBLE", dimensions = c("y"))

    # x attributes
    RNetCDF::att.put.nc(nc_con, "x", "_FillValue", "NC_DOUBLE", -9999.)
    RNetCDF::att.put.nc(nc_con, "x", "long_name", "NC_CHAR", "x coordinate of projection")
    RNetCDF::att.put.nc(nc_con, "x", "units", "NC_CHAR", "Meter")
    RNetCDF::att.put.nc(nc_con, "x", "axis", "NC_CHAR", "X")

    # y attributes
    RNetCDF::att.put.nc(nc_con, "y", "_FillValue", "NC_DOUBLE", -9999.)
    RNetCDF::att.put.nc(nc_con, "y", "long_name", "NC_CHAR", "y coordinate of projection")
    RNetCDF::att.put.nc(nc_con, "y", "units", "NC_CHAR", "Meter")
    RNetCDF::att.put.nc(nc_con, "y", "axis", "NC_CHAR", "Y")

    # put xx, yy
    RNetCDF::var.put.nc(ncfile = nc_con, variable = "x", data = xx)
    RNetCDF::var.put.nc(ncfile = nc_con, variable = "y", data = yy)

    # close connection
    RNetCDF::close.nc(nc_con)
}
