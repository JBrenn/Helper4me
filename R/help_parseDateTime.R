help_parseDateTime <- function(x.date, x.time, format=list(date="%Y-%m-%d", time="%H:%M:%S"),
                               parse.to="chron", fasttime = FALSE,
                               timezone="GMT")
{
  # for chron parsing
  if (format$time=="%H:%M" & parse.to == "chron") x.time <- paster(x.time, "00", sep=":")
  
  # if is date 
  if (is.null(x.time)) {
    out <- as.Date(x = x.date, format=format$date)
  } else {
  # if is datetime

    if (parse.to!="chron") {
      # POSIXct with fasttime
      if (fasttime) {
        out <- fastPOSIXct(x = paste(x.date, x.time, sep=" "), required.components=5))
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
