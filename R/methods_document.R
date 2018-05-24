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
            key_rand <- sample_cortical_key()

            # Register
            conn <- retinasdk$FullClient(key_rand)

            # Get keywords
            kw <- conn$getKeywordsForText(text)

            # Return
            return(kw)

          })
