#' plot single layer netCDF. 
#' 
#' \code{help_plotNClayer} is plotting a single NetCDF file layer.
#' 
#' @param infile netCDF file.
#' @param var netCDF variable.
#' @param shp shape file, optional.
#' @param proj_utm boolean, is ncfile projected on UTM
#' @param crs_proj4 character, proj4 projection definition
#' @param plot_proj_utm boolean , should out plot be projected
#' @param outfolder output folder
#' @param colorRampPal colorRampPalette
#' @param zlims value limits for ploting
#' @param flip boolean, Flip the values of a Raster* object by inverting the order of the rows (direction=y)
#' 
#' @return plot in pdf.
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
#' @export help_plotNClayer
#'
#'
help_plotNClayer <- function(infile, var="pre", shp=NA, proj_utm=FALSE, crs_proj4="+proj=utm +zone=30 +ellps=intl +units=m +no_defs", plot_proj_utm=TRUE, 
                             outfolder, colorRampPal=colorRampPalette(brewer.pal(9,"Blues"))(100), zlims, flip=TRUE)
{
  # Grab the lat and lon from the data
  lat <- raster(infile, varname="lat")
  lon <- raster(infile, varname="lon")
  
  # Convert to points and match the lat and lon
  plat <- rasterToPoints(lat)
  plon <- rasterToPoints(lon)
  lonlat <- cbind(plon[,3], plat[,3])
  
  # Specify the lonlat as spatial points with projection as long/lat
  if (proj_utm) {
    lonlat <- SpatialPoints(lonlat, proj4string = CRS(crs_proj4))
  } else {
    lonlat <- SpatialPoints(lonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))
  }
  
  mycrs <- CRS(crs_proj4)
  plonlat <- spTransform(lonlat, CRSobj = mycrs)
  
  # Yay! Now we can properly set the coordinate information for the raster
  pr <- raster::raster(infile, var=var)
  # flip on y axis 
  flip(pr, "y")
  
  # Fix the projection and extent
  projection(pr) <- mycrs
  extent(pr) <- extent(plonlat)
  
  # Project to long lat grid
  r <- raster::projectRaster(pr, crs=CRS("+proj=longlat +datum=WGS84"))
  if (!is.na(shp)) {
    # add shapefile
    shape <- raster::shapefile(x = shp)
    # Shapefile reprojection
    shp_utm     <- spTransform(shape, crs(pr))
    shp_longlat <- spTransform(shape, crs(r))
  }

    if (plot_proj_utm) {
      pdf(file.path(outfolder,paste(var,"_utm.pdf",sep="")))
      op <- par(cex=1.5, lwd=1.5)
      # plot raster
      plot(pr, col=colorRampPal, zlim=zlims)
      # overplot shape
      if (!is.na(shp)) plot(shp_utm, border=rgb(1,0,0,.75), add=TRUE)
      #Add contours
      contour(pr, add=TRUE, labcex = 1.2)
      par(op)
      dev.off()
    } else {
      pdf(file.path(outfolder,paste(var,"_longlat.pdf",sep="")))
      op <- par(cex=1.5, lwd=1.5)
      plot(r, col=colorRampPal, zlim=zlims)
      #Add contours
      contour(r, add=TRUE, labcex = 1.2)
      # Add country lines
      # library("maps")
      # map(add=TRUE, col=rgb(1,0,0,.5))
      if (!is.na(shp)) plot(shp_longlat, border=rgb(1,0,0,.75), add=TRUE)
      par(op)
      dev.off()
    }
  
}