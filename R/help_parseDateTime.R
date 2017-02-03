#' parse to Date or Date.Time objects.
#' 
#' parse character vector to Date (base::as.Date) or Date.Time (base::as.POSIXct or chron::chron) objects.
#' 
#' @param x.date date character vector.
#' @param x.time time character vector.
#' @param format list containing datetime format, default: list(date=..., time=...), 
#' if format$time = NULL date representation (as.Date) will be used for parsing.
#' @param parse.to specifying datetime representation, that will be used for parsing, 
#' either chron (default) or as.POSIXct.
#' @param timezone timezone only necessary if POSIX representation used, default "GMT"
#' 
#' @return date or date.time object
#' 
#' @examples
#' x.time <- c("00:20:00", "00:40:00", "01:00:00")
#' x.date <- rep("2012-01-01", 3)
#' 
#' # chron representation
#' help_parseDateTime(x.date, x.time, parse.to="chron")
#' 
#' # POSIX representation
#' help_parseDateTime(x.date, x.time, parse.to="POSIX",
#'                    timezone="GMT")
#' 
#' # Date representation
#' help_parseDateTime(x.date, x.time=NULL)
#' 
#' @author Johannes Brenner \email{johannes.brenner@ufz.de}
#' 
#' @references
#' 
#' @seealso
#' 
#' @keywords
#' 
#' @export help_parseDateTime

help_parseDateTime <- function(x.date, x.time, format=list(date="%Y-%m-%d", time="%H:%M:%S"),
                               parse.to="chron", fasttime = FALSE,
                               timezone="GMT")
{
  # for chron parsing
  if (format$time=="%H:%M" & parse.to == "chron") 
  {
    x.time <- paste(x.time, "00", sep=":")
    format$time <- "%H:%M:%S"
  }
   
  
  # if is date 
  if (is.null(x.time)) {
    out <- as.Date(x = x.date, format=format$date)
  } else {
  # if is datetime

    if (parse.to!="chron") {
      # POSIXct with fasttime
      if (fasttime) {
        out <- fastPOSIXct(x = paste(x.date, x.time, sep=" "), required.components=5)
      } else {
        # POSIXct
        out <- as.POSIXct(strftime(x = paste(x.date, x.time, sep=" "),
                                   format = paste(format$date, format$time, sep=" "),
                                   tz = timezone))
      }
    } else {
      # chron
      new.format <- lapply(format, function(x) gsub(x=x, pattern="%", replacement=""))
      out <- chron(dates. = x.date, times. = x.time, format = new.format,
                   out.format = list(date="Y-m-d", time="H:M:S"))
    }

  }

  return(out)
}
