help_updateGITlib <- function(lib, user="JBrenn")
{
  if (any(installed.packages()[,1]==lib))
    remove.packages(lib)

  devtools::install_github(repo = lib, username = user)

  require(lib)
}
