#' Load CarboEurope station data.
#'
#' @param datapath full path to pre-processed data files
#' @param station station descrption, e.g. "ESES1"
#'
#' @return zoo object, containing all CarboEU station data, chron datetime representation
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
#' @export help_loadCarboEU

help_loadCarboEU <- function(datapath = "/Users/brennerj/tmp/eve_f2_data/data/processed/eddy_carbo_europe",
                             station = "ESES1")
{
  files <- dir(path = datapath, full.names = T)
  ESES1_files <- files[grepl(pattern = station, x = files)]

  for (i in ESES1_files)
  {
    dat <- read.csv(file = i, na.strings = c("-9999.0000"))
    date <- paste(dat$Day,dat$Month,dat$Year,sep="-")
    time <- paste(dat$Hour,dat$Minute,"00",sep=":")
    date_time <- chron::chron(dates. = date, times. = time, format = list(dates="d-m-y", times="h:m:s"),
                              out.format = list(dates="d-m-y", times="h:m:s"))

    if (i == ESES1_files[1]) {
      dummy <- dat
    } else {
      dummy <- rbind(dummy, dat)
    }

  }

  dat_zoo <- zoo::zoo(dummy[,-c(1:5)], date_time)

  return(dat_zoo)
}


