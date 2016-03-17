help_updateGITlib <- function(lib, user="JBrenn", branch="master")
{
  if (any(installed.packages()[,1]==lib))
    remove.packages(lib)

  # devtools::install_github(repo = lib, username = user)
  if (branch=="master") {
    devtools::install_github(paste(user,lib, sep="/"))
  } else {
    devtools::install_github(paste(user,"/",lib,"@",branch, sep=""))
  }

}
