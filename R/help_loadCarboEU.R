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
  
  if (length(station_files)==0) {
    print(paste("Data for station ", station," not available. Returning NULL."))
    return(NULL)
  } else {
    print("reading ...")
    # loop over station_files
    for (i in station_files)
    {
      print(i)
      # read station data
      dat <- read.csv(file = i, na.strings = c("-9999.0000"))
      
      # for the first file create data and datetime dummy
      if (i == station_files[1]) {
        dummy <- dat
        # append following data to dummies
      } else {
        dummy <- rbind(dummy, dat)
      }
    }
    
    # extract date
    date <- paste(dummy$Day,dummy$Month,dummy$Year,sep="-")
    # extract time
    time <- paste(dummy$Hour,dummy$Minute,"00",sep=":")
    # converte date and time into chon object
    date_time <- chron::chron(dates. = date, times. = time, format = list(dates="d-m-y", times="h:m:s"),
                              out.format = list(dates="d-m-y", times="h:m:s"))
    
    # create zoo object
    dat_zoo <- zoo::zoo(dummy[,-c(1:5)], date_time)
    
    # finished
    print(paste("finished reading station", station, sep=" "))
    
    # return zoo object
    return(dat_zoo)
  }
}


