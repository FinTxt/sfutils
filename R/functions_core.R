# --------------------------------------------------------------------------
#
# This file contains functions that offer the user high-level interaction
# with the Cortical API.
#
# For example, if a user passes a list of expressions, they will be sent
# to the 'bulk' endpoint. If the user passes a single expression, they will
# be sent to the normal (single) endpoint.
#
# --------------------------------------------------------------------------

#' Fingerprint one or multiple texts
#'
#' This function is a wrapper. This should be the only function used by the end user to fingerprint texts ('documents')
#'
#' @param x single text or list of texts to be fingerprinted.
#'
#' @return either a single instance of Document class (if single text is passed) or an instance of Collection class containing multiple Documents (if a list of texts is passed)
#'
#' @seealso See the \code{\link{Document-class}} documentation for examples and more information
#'
#' @export

do_fingerprint_document <- function(x) {

  # Type of input
  input_type <- is(x)[1]

  # If list, then batch
  if(input_type == "list") {

    # Fingerprint documents
    res <- fingerprint_multiple_texts(x)

    # Create a list of Document classes
    res_doc <- mapply(function(fp, text) Document(text = text,
                                                  fingerprint = fp),
                      res, x)

    # Pass to collection
    res_doc <- Collection(res_doc)

  } else { # Else single document

    res_doc <- Document(x)

  }

  # Return
  return(res_doc)

}

#' Fingerprint a term
#'
#' This function is a wrapper. This should be the only function used by the end user to fingerprint terms
#'
#' @param x a term to fingerprint
#'
#' @return an object of class Term
#'
#' @seealso See the \code{\link{Term-class}} documentation for examples and more information
#'
#' @export

do_fingerprint_term <- function(x) {

  # Make sure length of x is one
  assertthat::assert_that(length(x) == 1,
                          msg = "Cannot provide multiple terms")

  # Call API
  trm <- Term(x)

  # Return
  return(trm)

}

#' Create a category filter
#'
#' This function is a wrapper. This should be the only function used by the end user to create a category filter
#'
#' @param name name of the filter
#' @param positive positive examples. Must consist of a vector with at least two strings.
#' @param negative optional negative examples. If you don't want to use these leave them empty.
#'
#' @return an object of class Filter
#'
#' @seealso See the \code{\link{Filter-class}} documentation for examples and more information
#'
#' @export

do_create_filter <- function(name, positive, negative = c()) {

  # Create class
  if(length(negative > 0)) {

    Filter(name, positive, negative = negative)

  } else {

    Filter(name, positive)

  }

}

#' Create an expression
#'
#' This function is a wrapper. This should be the only function used by the end user to create an expression
#'
#' @param x an expression or list of expressions to be fingerprinted
#'
#' @return Either an object of class Expression (if user passes character) or an object of class Collection containing multiple expressions (if user passes list)
#'
#' @examples
#' \dontrun{
#' # Get a single expression from API. Use subtract param
#'  body <- list(
#'   "sub" = list(
#'     "term" = "jaguar",
#'     "positions" = list(2,3,4,5,6)
#'   )
#' )
#'
#' # Get multiple expressions from API.
#' body <- list(
#'   list("sub" = list(
#'    "term" = "jaguar",
#'    "positions" = list(2,3,4,5,6)
#'   )),
#'   list("text" = "moodie moodle education whatever I am cool",
#'     "sub" = list(
#'       "term" = "online",
#'       "positions" = list(2,3,9,54)
#'     )),
#'   list("text" = "example text I am")
#' )
#' }
#'
#' @seealso See the \code{\link{Expression-class}} documentation for examples and more information
#'
#' @export

do_fingerprint_expression <- function(x) {

  # If names of x are NULL
  if(is.null(names(x))) {

    # Fingerprint documents
    res <- fingerprint_multiple_expressions(x)

    # Create a list of Expression classes
    res_exp <- mapply(function(fp, exp) {
                        Expression(expression = exp,
                                   fingerprint = fp)
                      }, res, x)

    # Pass to collection
    res_exp <- Collection(entry_list = res_exp)

  } else { # Else single document

    res_exp <- Expression(x)

  }

  # Return
  return(res_exp)

}

#' Retrieve all available retinas from the Cortical server
#'
#' @return Named list with retina name, description, number of terms, number of rows and columns.
#'
#' @seealso See the \href{http://www.cortical.io/resources_apidocumentation.html}{Cortical documentation} for more information about the retinas.
#'
#' @importFrom httr GET
#' @importFrom httr content
#' @export

get_retinas <- function() {

  # Send request
  req <- httr::GET(paste0(Sys.getenv("CORTICAL_SERVER"), "/retinas"))

  # Unpack
  resp <- httr::content(req)

  # Take names
  nams <- vapply(resp, function(x) x$retinaName, "string")

  # Apply names
  names(resp) <- nams

  # Return
  resp

}
