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
  # get filenames from datapth dir
  files <- dir(path = datapath, full.names = T)
  # extract station files
  station_files <- files[grepl(pattern = station, x = files)]

  # loop over true_files
  for (i in station_files)
  {
    # read station data
    dat <- read.csv(file = i, na.strings = c("-9999.0000"))
    # extract date
    date <- paste(dat$Day,dat$Month,dat$Year,sep="-")
    # extract time
    time <- paste(dat$Hour,dat$Minute,"00",sep=":")
    # converte date and time into chon object
    date_time_ <- chron::chron(dates. = date, times. = time, format = list(dates="d-m-y", times="h:m:s"),
                              out.format = list(dates="d-m-y", times="h:m:s"))

    # for the first file create data and datetime dummy
    if (i == true_files[1]) {
      dummy_date_time <- date_time
      dummy <- dat
    # append following data to dummies
    } else {
      dummy_date_time <- c(dummy_date_time, date_time)
      dummy <- rbind(dummy, dat)
    }

  }
  # create zoo object
  dat_zoo <- zoo::zoo(dummy[,-c(1:5)], dummy_date_time)
  # return zoo object
  return(dat_zoo)
}


