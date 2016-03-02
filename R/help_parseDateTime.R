help_parseDateTime <- function(x.date, x.time, format=list(date="%Y-%m-%d", time="%H:%M:%S"),
                               parse.to="chron",
                               timezone="GMT")
{
  # if is date
  if (is.null(x.time)) {
    out <- as.Date(x = x.date, format=format$date)
  } else {
  # if is datetime

    if (parse.to!="chron") {
      # POSIXct
      out <- as.POSIXct(strftime(x = paste(x.date, x.time, sep=" "),
                                 format = paste(format$date, format$time, sep=" "),
                                 tz = timezone))
    } else {
      # chron
      new.format <- lapply(format, function(x) gsub(x=x, pattern="%", replacement=""))
      out <- chron(dates. = x.date, times. = x.time, format = new.format,
                   out.format = list(date="Y-m-d", time="H:M:S"))
    }

  }

  return(out)
}
