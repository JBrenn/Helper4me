#' @title bundle of error measures
#' @description error measures comparing two datasets (simulation vs. observation).
#' @param obs observation time series or vector
#' @param sim observation time series or vector
#' @return named vector of error meassures
#' @details
#' r: Pearson correlation
#' rmse: Root Mean Square Error
#' rsd: ratio of standard deviation
#' nme: Normalized mean error
#' qu05.5%: 05th percentile statistical measure
#' qu95.95%: 95th percentile statistical measure
#' KGE: Kling-Gupta efficiency
#' @examples
#' errormeas(1:10,1:10)
#' @seealso
#'  \code{\link[hydroGOF]{KGE}}
#' @rdname errormeas
#' @export
#' @importFrom hydroGOF KGE
errormeas <- function(obs, sim)
{
  # length of data
  n <- length(sim)
  # out dummy vector
  gof <- c()
  # Pearson correlation (r)
  gof <- c( gof, r = 1 - abs(cor(x=sim, y=obs)) )
  # Mean bias error (MBE)
  # gof <- c( gof, mbe = sum(sim-obs) / n )
  # Root Mean Square Error (RMSE)
  gof <- c( gof, rmse = sqrt(sum(sim-obs)^2 / n) )
  # ratio of standard deviation (rSD)
  gof <- c( gof, rsd = abs(1 - sd(sim) / sd(obs)) )
  # Normalized mean error (MME)
  gof <- c( gof, nme = sum(abs(sim-obs)) / sum(abs(mean(obs, na.rm=T)-obs)) )
  # extremes of the distribution
  # 05th percentile statistical measure
  gof <- c( gof, qu05 = abs(quantile(sim, .05) - quantile(obs, .05)))
  # 95th percentile statistical measure
  gof <- c( gof, qu95 = abs(quantile(sim, .95) - quantile(obs, .95)))
  # KGE
  gof <- c( gof, kge = hydroGOF::KGE(sim = sim, obs=obs))
  #
  return(gof)
}
