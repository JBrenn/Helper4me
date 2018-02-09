#' Load CarboEurope station data.
#'
#' @param datapath full path to pre-processed data files
#' @param station station descrption, e.g. "ESES1"
#' @param stationlist gather information on FLUXnet sites, if FALSE normal proceeding of function, if charater or NA site information output, e.g. "ES" for sites in Spain.
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

help_loadCarboEU <- function(datapath = "/Users/brennerj/tmp/eve_f2_data/data/processed/eddy_carbo_europe_201709/",
                             station = "ESES1", stationlist=FALSE)
{
  if (class(stationlist[1])=="character" | is.na(stationlist[1])) {
    # get filenames from datapth dir for stationlist
    files <- dir(path = datapath, full.names = F)
    files <- files[!grepl(".zip", files) | !grepl(".txt", files)]
    
    # get accronyms
    stationaccrym <- unique(substr(files,1,5))
    
    id    <- c()
    end   <- c()
    start <- c()
    station_loop <- c()
    
    if (is.na(stationlist[1])) {
      station_loop <- stationaccrym
    } else {
      stationaccrym_pl <- paste("_", stationaccrym, sep="")
      for (i in stationlist) {
        station_loop <- c(station_loop, stationaccrym[grepl(paste("_",i,sep=""), stationaccrym_pl)])
      }
    }
    
    for (i in 1:length(station_loop)) {
      # get start/end year of data
      rgyears <- range(as.integer(substr(files,7,10)[grepl(station_loop[i], substr(files,1,5))]))
      end[i] <- rgyears[2]; start[i] <- rgyears[1]
      id[i]  <- station_loop[i]
    }
    
    id_ <- paste(substr(id,1,2), substr(id,3,5), sep="-")
    df <- data.frame(ID=id, "Site Code"=id_, START=start, END=end)
    
    # join with site table
    file <- file.path(datapath, "SitesList.txt")
    sitelist <- read.csv(file)
    df <- merge(x = df, y = sitelist)
    
    return(df)
    
  } else {
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
}


