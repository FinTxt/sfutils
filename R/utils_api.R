# --------------------------------------------------------------------------
#
# This file contains utility functions that:
#
#  1. Take as their input a term, expression, text or filter
#  2. Send this information to the Cortical API
#  3. Return the resulting semantic fingerprint
#
# All of the functions below use the reticulate package to interact with
# the retinasdk module in Python.
#
# The retinasdk object used in these functions is created in zzz.R. This
# way, we only have to call the module once (at startup).
#
# --------------------------------------------------------------------------

#' Call to python to fingerprint text
#'
#' @param text text to be fingerprinted.
#'
#' @return fingerprint for text
#'
#' @seealso See the \href{http://documentation.cortical.io/intro.html#what-is-a-semantic-fingerprint}{Cortical documentation} for more information about semantic fingerprinting. See the \href{online API tool}{http://api.cortical.io} for more information about the API.
#'
#' @noRd

fingerprint_single_text <- function(text) {

  # Get key
  key <- Sys.getenv("CORTICAL_API_KEY")
  server <- Sys.getenv("CORTICAL_SERVER")
  retina <- Sys.getenv("CORTICAL_RETINA")

  # Register
  conn <- retinasdk$FullClient(key,
                               apiServer = server,
                               retinaName = retina)


  # This needs to be encoded ...
  pp <- reticulate::r_to_py(text)
  encoded <- pp$encode("utf-8")

  # Get fingerprint
  res <- conn$getFingerprintForText(encoded)

  # Return
  return(res$positions)

}

#' Call to python to fingerprint multiple texts
#'
#' @param text_list list of texts to be fingerprinted
#'
#' @return list of fingerprints for texts
#'
#' @seealso See the \href{http://documentation.cortical.io/intro.html#what-is-a-semantic-fingerprint}{Cortical documentation} for more information about semantic fingerprinting. See the \href{online API tool}{http://api.cortical.io} for more information about the API.
#'
#' @noRd

fingerprint_multiple_texts <- function(text_list) {

  # List must be unnamed
  if(!is.null(names(text_list))) {
    text_list <- unname(text_list)
  }
  # Make sure that all are characters
  text_types <- vapply(text_list, function(x) is.character(x), TRUE)
  if(!all(text_types)) {
    stop("Not all documents in the list are characters")
  }
  # Minimum length is 50 characters
  text_length <- vapply(text_list, function(x) nchar(x) >= 50, TRUE)
  if(!all(text_length)) {
    warning(paste0("Not all documents in the list are at least 50 characters (elements ",
                   paste(which(!text_length), collapse = ", "),
                   "). Run 'remove_short_texts()' to remove documents that are too short to fingerprint."))
  }

  # Get key
  key <- Sys.getenv("CORTICAL_API_KEY")
  server <- Sys.getenv("CORTICAL_SERVER")
  retina <- Sys.getenv("CORTICAL_RETINA")

  # Register
  conn <- retinasdk$FullClient(key,
                               apiServer = server,
                               retinaName = retina)

  # Encode each article
  lst_encoded <- lapply(lst, function(x) reticulate::r_to_py(x))
  text_list_py <- reticulate::r_to_py(lst_encoded)

  # Get fingerprint
  res <- conn$getFingerprintsForTexts(text_list_py)

  # Get positions
  res_pos <- lapply(res, function(x) x$positions)

  # Return
  return(res_pos)

}

#' Create Cortical filter
#'
#' @param name name of the filter
#' @param positive positive examples. Must consist of a vector with at least two strings.
#' @param ... optional negative examples. Call using 'negative = <vector-with-negative-examples>'
#'
#' @return a fingerprint for the category filter
#'
#' @seealso See the \href{http://documentation.cortical.io/classification.html}{classification documentation} for more information about filters.
#'
#' @noRd

create_categoryfilter <- function(name, positive, ...) {

  # Get optional args
  opts <- list(...)
  if("negative" %in% names(opts)) {
    negative <- opts$negative
  } else {
    negative <- NULL
  }

  # Get key
  key <- Sys.getenv("CORTICAL_API_KEY")
  server <- Sys.getenv("CORTICAL_SERVER")
  retina <- Sys.getenv("CORTICAL_RETINA")

  # Register
  conn <- retinasdk$FullClient(key,
                               apiServer = server,
                               retinaName = retina)

  # Create filter
  if(is.null(negative)) {
    res <- conn$createCategoryFilter(filterName = name,
                                     positiveExamples = positive)
  } else {
    res <- conn$createCategoryFilter(filterName = name,
                                     positiveExamples = positive,
                                     negativeExamples = negative)
  }

  # Return positions
  return(res$positions)

}

#' Get fingerprint for a term
#'
#' @param term specific term you want to fingerprint
#'
#' @return fingerprint, pos_type, score and df
#'
#' @seealso See the \href{http://documentation.cortical.io/classification.html}{classification documentation} for more information about filters.
#'
#' @noRd

fingerprint_term <- function(term) {

  # Get key
  key <- Sys.getenv("CORTICAL_API_KEY")
  server <- Sys.getenv("CORTICAL_SERVER")
  retina <- Sys.getenv("CORTICAL_RETINA")

  # Register
  conn <- retinasdk$FullClient(key,
                               apiServer = server,
                               retinaName = retina)

  # Register
  conn <- retinasdk$FullClient(key)

  # Get fingerprint
  res <- conn$getTerms(term, getFingerprint = TRUE)

  # Get metrics & fingerprint
  list(
    "fingerprint" = res[[1]]$fingerprint$positions,
    "df" = res[[1]]$df,
    "pos_types" = ifelse(is.list(res[[1]]$pos_types),
                         res[[1]]$pos_types,
                         list(res[[1]]$pos_types)),
    "score" = res[[1]]$score
  )

}

#' Get fingerprint for an expression
#'
#' @param expression Unnamed list containing a combination of terms, texts or positions and boolean operators ('sub', 'and' etc). See 'see also' section for url to documentation.
#'
#' @return fingerprint positions
#'
#' @seealso See the \href{http://documentation.cortical.io/the_power_of_expressions.html}{expression documentation} for more information about expressions and boolean operators.
#'
#' @importFrom jsonlite toJSON
#'
#' @noRd

fingerprint_single_expression <- function(expression) {

  # Get key
  key <- Sys.getenv("CORTICAL_API_KEY")
  server <- Sys.getenv("CORTICAL_SERVER")
  retina <- Sys.getenv("CORTICAL_RETINA")

  # Register
  conn <- retinasdk$FullClient(key,
                               apiServer = server,
                               retinaName = retina)

  # Register
  conn <- retinasdk$FullClient(key)

  # cast to json
  body_json <- jsonlite::toJSON(expression, auto_unbox = TRUE)

  # Get fingerprint
  res <- conn$getFingerprintForExpression(body=body_json)

  # Return positions
  res$positions

}

#' Get fingerprint for multiple expressions
#'
#' @param expression list of unnamed lists containing a combination of terms, texts or positions and boolean operators ('sub', 'and' etc). See 'see also' section for url to documentation.
#'
#' @return fingerprint positions
#'
#' @seealso See the \href{http://documentation.cortical.io/the_power_of_expressions.html}{expression documentation} for more information about expressions and boolean operators.
#'
#' @importFrom jsonlite toJSON
#'
#' @noRd

fingerprint_multiple_expressions <- function(expression_list) {

  # Get key
  key <- Sys.getenv("CORTICAL_API_KEY")
  server <- Sys.getenv("CORTICAL_SERVER")
  retina <- Sys.getenv("CORTICAL_RETINA")

  # Register
  conn <- retinasdk$FullClient(key,
                               apiServer = server,
                               retinaName = retina)

  # Cast to json
  bdjson <- jsonlite::toJSON(expression_list,
                             auto_unbox = TRUE)

  # Get fingerprints
  res <- conn$getFingerprintsForExpressions(bdjson)

  # Retrieve positions
  res_pos <- lapply(res, function(x) x$positions)

  # Return
  res_pos

}

