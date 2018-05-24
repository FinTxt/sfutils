# --------------------------------------------------------------------------
#
# zzz.R is evaluated when the package is loaded using 'library(sfutils)'
# It is used to call the reticulate library, which forms a bridge between
# R and python.
#
# --------------------------------------------------------------------------

# global reference to retinasdk (will be initialized in .onLoad)
retinasdk <- NULL

# When the package is loaded, do ...
.onLoad <- function(libname = find.package("sfutils"), pkgname="sfutils") {

  # use superassignment to update global reference to retinasdk
  # Delay loading the package (https://rstudio.github.io/reticulate/articles/package.html)
  retinasdk <<- reticulate::import("retinasdk", delay_load = TRUE)

  # Set options for retinaSDK python module
  if(Sys.getenv("CORTICAL_SERVER") == "") {
    Sys.setenv("CORTICAL_SERVER" = "http://api.cortical.io/rest")
  }
  if(Sys.getenv("CORTICAL_RETINA") == "") {
    Sys.setenv("CORTICAL_RETINA" = "en_associative")
  }
  if(Sys.getenv("CORTICAL_API_KEY") == "") {
    message(paste0("No cortical key registered. Execute ",
                  "'","Sys.setenv('CORTICAL_API_KEY' = '<your-key>')" , "'",
                  " to set your credentials."))
  }

}
