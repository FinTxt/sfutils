# --------------------------------------------------------------------------
#
# Methods for the Term class
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

#' @rdname get_context-methods
#'
#' @seealso \href{http://documentation.cortical.io/working_with_terms.html}{This} page for more information about contexts and terms
#'
#' @export
#'

setMethod("get_context",
          "Term",
          function(object) {

            # Get key
            key <- Sys.getenv("CORTICAL_API_KEY")
            server <- Sys.getenv("CORTICAL_SERVER")
            retina <- Sys.getenv("CORTICAL_RETINA")

            # Register
            conn <- retinasdk$FullClient(key,
                                         apiServer = server,
                                         retinaName = retina)

            # Get fingerprint
            res <- conn$getContextsForTerm(term(object),
                                           getFingerprint = TRUE,
                                           maxResults = 10L)

            # Unlist
            res_unlist <- lapply(res, function(x) {
              list(
                "context_id" = x$context_id,
                "context_label" = x$context_label,
                "fingerprint" = x$fingerprint$positions
              )
            })

            # Assign names
            res_names <- vapply(res_unlist, function(x) x$context_label, "character")
            names(res_unlist) <- res_names

            # Return
            res_unlist

          })

#' @rdname get_similar_terms-methods
#'
#' @seealso \href{http://documentation.cortical.io/working_with_terms.html}{This} page for more information about contexts and terms
#'
#' @export
#'

setMethod("get_similar_terms",
          "Term",
          function(object, contextId = NULL,
                   posType = NULL, startIndex = 0L) {

            # Get key
            key <- Sys.getenv("CORTICAL_API_KEY")
            server <- Sys.getenv("CORTICAL_SERVER")
            retina <- Sys.getenv("CORTICAL_RETINA")

            # Register
            conn <- retinasdk$FullClient(key,
                                         apiServer = server,
                                         retinaName = retina)

            # Get fingerprint
            res <- conn$getSimilarTermsForTerm(term(object),
                                               contextId = contextId,
                                               posType = posType,
                                               getFingerprint = TRUE,
                                               startIndex = startIndex,
                                               maxResults = 10L)

            # Unlist
            res_unlist <- lapply(res, function(x) {
              list(
                "df" = x$df,
                "fingerprint" = x$fingerprint$positions,
                "pos_types" = x$pos_types,
                "score" = x$score,
                "term" = x$term
              )
            })

            # Assign names
            res_names <- vapply(res_unlist, function(x) x$term,
                                "character")
            names(res_unlist) <- res_names

            # Return
            res_unlist

          })
