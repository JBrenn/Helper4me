help_zooAggregation <- function(data, aggr_days=1/24)
{
  aggr_vec <- floor(as.numeric(time(data)) / aggr_days)
  data <- aggregate(x=data, by=aggr_vec, FUN=mean, na.rm=F)
  data <- zoo(x =  coredata(data), order.by = chron(time(data) * aggr_days))
  
  return(data)
}