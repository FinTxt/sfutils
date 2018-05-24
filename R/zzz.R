# --------------------------------------------------------------------------
#
# zzz.R is evaluated when the package is loaded using 'library()'
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
  options(
    sfutils = list(
      "apiServer" = "http://api.cortical.io/rest",
      "retina" = "en_associative",
      "apiKey" = ""
    )
  )

}
