#' Update GitHub library.
#' 
#' Update GitHub library, by default from user repository "JBrenn"
#' 
#' @param lib character, GitHub repository to update.
#' @param user character, GitHub user name.
#' @param branch character, GitHub repository branch
#' @param bld_vignettes boolean, build vignettes? default = FALSE
#' 
#' @return 
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
#' @export help_updateGITlib
#' 
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
