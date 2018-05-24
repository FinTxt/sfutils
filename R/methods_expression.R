# --------------------------------------------------------------------------
#
# Methods for the Expression class
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
#' @seealso \href{http://documentation.cortical.io/the_power_of_expressions.html}{This} page for more information about contexts and expressions
#'
#' @importFrom jsonlite toJSON
#'
#' @examples
#' \dontrun{
#'
#' ###### FOR TERMS
#'
#' # Fingerprint a term
#' trm_fp <- do_fingerprint_term("finance")
#'
#' # Get contexts
#' trm_fp_cont <- get_context(trm_fp)
#'
#' ###### FOR EXPRESSIONS
#'
#' # Get data
#' data("company_descriptions")
#'
#' # Put text in a list to be fingerprinted as expression
#' txt <- lapply(company_descriptions, function(x) list("text" = x$desc))
#' # Must be an unnamed list
#' txt <- unname(txt)
#'
#' # Fingerprint as expression
#' txt_fp <- do_fingerprint_expression(txt)
#'
#' # Get contexts
#' txt_fp_cont <- get_context(txt_fp[[1]])
#'
#' ###### FOR COLLECTIONS
#'
#' # Get data
#' data("company_descriptions")
#'
#' # Put text in a list
#' txt <- lapply(company_descriptions, function(x) list("text" = x$desc))
#' # Must be an unnamed list
#' txt <- unname(txt)
#'
#' # Fingerprint as expression
#' txt_fp <- do_fingerprint_expression(txt)
#'
#' # Get contexts
#' # This takes quite a while
#' txt_fp_cont <- get_context(txt_fp, type = "Expression")
#'
#' }

setMethod("get_context",
          "Expression",
          function(object) {

            # Get key
            key <- Sys.getenv("CORTICAL_API_KEY")
            server <- Sys.getenv("CORTICAL_SERVER")
            retina <- Sys.getenv("CORTICAL_RETINA")

            # Register
            conn <- retinasdk$FullClient(key,
                                         apiServer = server,
                                         retinaName = retina)

            # Dump to json
            body_json <- jsonlite::toJSON(sfexpression(object),
                                          auto_unbox = TRUE)

            # Get fingerprint
            res <- conn$getContextsForExpression(
              body_json,
              getFingerprint = TRUE,
              maxResults = 10L
            )

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
          "Expression",
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

            # To json
            body_json <- jsonlite::toJSON(sfexpression(object),
                                          auto_unbox = TRUE)

            # Get fingerprint
            res <- conn$getSimilarTermsForExpression(body_json,
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
