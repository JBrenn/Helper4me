help_updateGITlib <- function(lib, user="JBrenn", branch="master", bld_vignettes=FALSE)
{
  if (any(installed.packages()[,1]==lib))
    remove.packages(lib)

  # devtools::install_github(repo = lib, username = user)
  if (branch=="master") {
    if (bld_vignettes) {
      devtools::install_github(paste(user,lib, sep="/"), build_vignettes = TRUE)
    } else {
      devtools::install_github(paste(user,lib, sep="/"))
    }

  } else {
    devtools::install_github(paste(user,"/",lib,"@",branch, sep=""))
  }

}
