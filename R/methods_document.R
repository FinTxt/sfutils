# --------------------------------------------------------------------------
#
# Methods for the Document class
#
# Standard methods are an extension of generics that already exist in R
# (e.g. print, plot etc).
#
# Specific methods are specific to sfutils (we create the generic in
# generics.R)
#
# API methods are methods that call the Cortical API / interact with Python
#
# --------------------------------------------------------------------------

# API METHODS ----

#' @rdname get_keywords-methods

setMethod("get_keywords",
          "Document",
          function(object) {

            # Get text
            text <- object@text

            # Get key
            key <- Sys.getenv("CORTICAL_API_KEY")
            server <- Sys.getenv("CORTICAL_SERVER")
            retina <- Sys.getenv("CORTICAL_RETINA")

            # Register
            conn <- retinasdk$FullClient(key,
                                         apiServer = server,
                                         retinaName = retina)

            # Get keywords
            kw <- conn$getKeywordsForText(text)

            # Return
            return(kw)

          })
