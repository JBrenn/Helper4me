#' Aggregate latent heat data.
#'
#' Daily aggregation of latent heat data from eddy covariance systems.
#' Native data time resolution is sub-daily.
#' Conversion to evapotranspiration flux (mm/d) is possible.
#'
#' @param LE zoo object, subdaily latent heat data in W/m2
#' @param perc_thresh threshold (percentage of NAs [0;1]) defining if specific day fulfills data quality
#' @param ET boolean, if TRUE latent heat (W/m2) will be converted to evapotranspiration (mm/d)
#'
#' @return zoo object, containing quality daily latent heat or evapotranspiration data
#'
#' @examples
#' library(zoo)
#' library(chron)
#' # load eddy covariance data
#' data(eses1)
#' # extract latent heat
#' LE <- eses1$ufz_lE
#' # spline interpolation
#' LE_spline <- zoo::na.spline(LE, maxgap=10)
#' # aggregate latent heat, convert to evapotranspiration
#' ET <- help_aggrLE(LE = LE_spline, perc_thresh = .1, ET = TRUE)
#' # plot time series
#' plot(ET)
#'
#' @author Johannes Brenner \email{johannes.brenner@ufz.de}
#'
#' @references
#'
#' @seealso
#'
#' @keywords
#'
#' @export help_aggrLE
#'
help_aggrLE <- function(LE, perc_thresh = 0.1, ET = TRUE) {

  # convert negtive LE to 0
  zoo::coredata(LE) <- ifelse(zoo::coredata(LE)<0, 0, zoo::coredata(LE))
  # extract date
  date <- as.Date(time(LE))
  x <- sapply(X = unique(date),   function (x) {
    # data for single day
    datadate <- LE[date==x,]
    # percent of NAs in day
    na_perc <- mean(is.na(datadate))/length(datadate)
    if (na_perc < perc_thresh) {
      # if percent of NAs < perc_thresh
      # calculate daily LE [mm/day]
      y <- mean(datadate)
    } else {
      # if percent of NAs >= perc_thresh
      # ommit data, write out NA
      y <- NA
    }
  })
  # calculate evapotranspiration (mm/d) from latent heat (W/m2 = J/(m2 s))
  if (ET) x <- x / 2454000 *60 *60 *24
  # create zoo object
  out <- zoo::zoo(x, unique(date))
  # return zoo object
  return(out)
}
