#' Aggregate precipitation data.
#'
#' Daily aggregation of precipitation data. Native data time resolution is sub-daily.
#'
#' @param P zoo object, subdaily precipitation data in mm
#' @param perc_thresh threshold (percentage of NAs [0;1]) defining if specific day fulfills data quality
#'
#' @return zoo object, containing quality daily precipitation data
#'
#' @examples
#' library(zoo)
#' library(chron)
#' # load eddy covariance data
#' data(eses1)
#' # extract precipitation
#' P <- eses1$Precip
#' # aggregate precipitation, convert to daily sum
#' P <- help_aggrP(LE = P, perc_thresh = .1)
#' # plot time series
#' plot(P)
#'
#' @author Johannes Brenner \email{johannes.brenner@ufz.de}
#'
#' @references
#'
#' @seealso
#'
#' @keywords
#'
#' @export help_aggrP
#'
help_aggrP <- function(P, perc_thresh = 0.1) {
  
  # convert negtive P to 0
  zoo::coredata(P) <- ifelse(zoo::coredata(P)<0, 0, zoo::coredata(P))
  # extract date
  date <- as.Date(time(P))
  # loop over unique dates
  x <- sapply(X = unique(date),   function (x) {
    # data for single day
    datadate <- P[date==x,]
    # percent of NAs in day
    na_perc <- sum(is.na(datadate))/length(datadate)
    if (na_perc < perc_thresh) {
      # if percent of NAs < perc_thresh
      # calculate daily P [mm/day]
      y <- sum(datadate)
    } else {
      # if percent of NAs >= perc_thresh
      # ommit data, write out NA
      y <- NA
    }
  })
  # create zoo object
  out <- zoo::zoo(x, unique(date))
  # return zoo object
  return(out)
}