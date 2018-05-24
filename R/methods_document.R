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

#' @rdname do_tokenize-methods

setMethod("do_tokenize",
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
            tok <- conn$getTokensForText(text)

            # Return
            return(tok)

          })

#' @rdname do_slice_document-methods

setMethod("do_slice_document",
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

            # Encode text
            txt_encode <- reticulate::r_to_py(text)
            txt_encode <- txt_encode$encode("utf-8")

            # Get slices
            slic <- conn$getSlicesForText(txt_encode,
                                          getFingerprint = TRUE)

            # Lapply each slice
            slic_proc <- lapply(slic, function(x) {
              list(
                "text" = x$text,
                "fingerprint" = x$fingerprint
              )

            })

            # Return
            return(slic_proc)

          })
