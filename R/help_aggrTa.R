#' Aggregate air temperature data.
#'
#' Daily aggregation of air temperature data to mean, minimum and maximum air temperature. 
#' Native data time resolution is sub-daily.
#'
#' @param P zoo object, subdaily precipitation data in mm
#' @param perc_thresh threshold (percentage of NAs [0;1]) defining if specific day fulfills data quality
#' @param fun aggregation function, e.g. min, max, mean, etc.
#'
#' @return zoo object, containing quality daily air temperature data
#'
#' @examples
#' library(zoo)
#' library(chron)
#' # load eddy covariance data
#' data(eses1)
#' # extract precipitation
#' Ta <- eses1$Ta
#' # aggregate air temperature, convert to min, max, mean air temperature
#' Ta <- help_aggrTa(Ta = Ta, perc_thresh = .1, fun=c(min, mean, max))
#' # plot time series
#' plot(Ta)
#'
#' @author Johannes Brenner \email{johannes.brenner@ufz.de}
#'
#' @references
#'
#' @seealso
#'
#' @keywords
#'
#' @export help_aggrT
#'
help_aggrT <- function(Ta, perc_thresh = 0.1) {
  
  # extract date
  date <- as.Date(time(Ta))
  
  # dummy vector
  x <- data.frame()
  # loop over unique dates
  for (i in unique(date))
  {
    # data for single day
    datadate <- Ta[date==i,]
    # percent of NAs in day
    na_perc <- sum(is.na(datadate))/length(datadate)
    if (na_perc < perc_thresh) {
      # if percent of NAs < perc_thresh
      # calculate min, max, mean Ta
      x <- rbind(x,c(min(datadate, na.rm=TRUE), 
                     mean(datadate, na.rm=TRUE), max(datadate, na.rm=TRUE)))
    } else {
      # if percent of NAs >= perc_thresh
      # ommit data, write out NA
      x <- rbind(x,c(NA,NA,NA))
    }
  }
  # create zoo object
  out <- zoo::zoo(x, unique(date))
  # name zoo object
  names(out) <- c("min", "mean", "max")
  # return zoo object
  return(out)
}