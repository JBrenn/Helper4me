#' Allocate path to root directories at EVE server.
#'
#' Allocate path to root directories (/home, /work, /data) at EVE server.
#'
#' @param dir directory at EVE serever, 1: home, 2: work, 3: data, 4: data/edge
#' @param setwdir boolean, if TRUE set working directory to dir
#'
#' @return path to directory
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
#' @export help_2EVE
#'
help_2EVE <- function(dir = 1, setwdir = TRUE)
{
  sysinfo <- Sys.info()

  # dir: home
  if (dir == 1)
  {
    if (grepl("ces251",sysinfo["nodename"])) pre <- "/Users/brennerj/tmp/eve_f2_home/"
    if (grepl("frontend",sysinfo["nodename"])) pre <- "/home/brennerj/"
    if (grepl("node",sysinfo["nodename"])) pre <- "/home/brennerj/"
  }
  # dir: work
  if (dir == 2)
  {
    if (grepl("ces251",sysinfo["nodename"])) pre <- "/Users/brennerj/tmp/eve_f2_work/"
    if (grepl("frontend",sysinfo["nodename"])) pre <- "/work/brennerj/"
    if (grepl("node",sysinfo["nodename"])) pre <- "/work/brennerj/"
  }
  # dir: data
  if (dir == 3)
  {
    if (grepl("ces251",sysinfo["nodename"])) pre <- "/Users/brennerj/tmp/eve_f2_data/"
    if (grepl("frontend",sysinfo["nodename"])) pre <- "/data/"
    if (grepl("node",sysinfo["nodename"])) pre <- "/data/"
  }
  # dir: edge
  if (dir == 4)
  {
    if (grepl("ces251",sysinfo["nodename"])) pre <- "/Users/brennerj/tmp/eve_f2_edge/"
    if (grepl("frontend",sysinfo["nodename"])) pre <- "/data/edge/"
    if (grepl("node",sysinfo["nodename"])) pre <- "/data/edge/"
  }

  if (setwdir) setwd(pre)

  return(pre)
}
