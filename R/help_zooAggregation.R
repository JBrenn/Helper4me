#' Aggregation for zoo object with chron datetime representation.
#' 
#' Aggregation for zoo object with chron datetime representation.
#' 
#' @param x zoo object with chron datetime representation.
#' @param aggr_days time over which to aggregate in days, by default 1 hour (1/24 days).
#' 
#' @return aggregated zoo object
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
#' @export help_zooAggregation
#' 

help_zooAggregation <- function(data, aggr_days=1/24, na.rm)
{
  aggr_vec <- floor(as.numeric(time(data)) / aggr_days)
  data <- aggregate(x=data, by=aggr_vec, FUN=mean, na.rm=na.rm)
  data <- zoo(x =  coredata(data), order.by = chron(time(data) * aggr_days))
  
  return(data)
}