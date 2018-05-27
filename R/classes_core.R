# --------------------------------------------------------------------------
#
# R S4 core classes go here.
#
# These classes represent the four core classes of the semantic folding API:
# terms, expressions, texts and category filters. They all inherit from the
# "Fingerprint" class.
#
# This script contains:
#
# 1. Core classes
# 2. Constructors for these classes
#
# --------------------------------------------------------------------------

# CLASSES ----

#' Fingerprint class
#'
#' This class forms the basis for the core cortical API classes (Document, Term, Expression, Filter). It should not be called by the user directly.
#'
#' @slot uuid unique id for the fingerprint. Can be passed, else generated.
#' @slot type type of fingerprint (e.g. document if text, filter if filter)
#'
#' @seealso See the \href{http://documentation.cortical.io/intro.html}{Cortical documentation} for more information about semantic fingerprinting
#'
#' @importFrom methods is
#' @importFrom methods new
#' @importFrom methods slot
#'
#' @name Fingerprint-class

.Fingerprint <- setClass(

  # Name
  "Fingerprint",
  # Slots
  slots = c(
    uuid = "character",
    type = "character"
  )

)

#' @slot text text to be fingerprinted
#' @slot fingerprint numeric vector of the fingerprint
#'
#' @seealso See the \href{http://documentation.cortical.io/working_with_text.html}{Cortical documentation} for more information about semantic fingerprinting and text
#'
#' @name Document-class

.Document <- setClass(

  # Name
  "Document",
  # data
  slots = c(
    text = "character",
    fingerprint = "numeric"
  ),
  # Inherits
  contains = "Fingerprint"

)

#' @slot name Name of the filter
#' @slot positive positive examples (should be contained in text)
#' @slot negative negative examples (should be contained in text)
#' @slot fingerprint numeric vector of the fingerprint
#'
#' @seealso See the \href{http://documentation.cortical.io/classification.html}{classification documentation} for more information about filters.
#'
#' @name Filter-class

.Filter <- setClass(

  # Name
  "Filter",
  # data
  slots = c(
    name = "character",
    positive = "character",
    negative = "character",
    fingerprint = "numeric"
  ),
  # Inherits
  contains = "Fingerprint"

)

#' @slot term term to be fingerprinted
#' @slot df the df value of the term
#' @slot score the score of this term
#' @slot pos_types the position type of this term
#' @slot fingerprint numeric vector of the fingerprint
#'
#' @seealso See the \href{http://documentation.cortical.io/working_with_terms.html}{Cortical documentation} for more information about semantic fingerprinting and terms
#'
#' @name Term-class

.Term <- setClass(

  # Name
  "Term",
  # data
  slots = c(
    term = "character",
    df = "numeric",
    score = "numeric",
    pos_types = "list",
    fingerprint = "numeric"
  ),
  # Inherits
  contains = "Fingerprint"

)

#' @slot expression expression to be fingerprinted
#' @slot fingerprint numeric vector of the fingerprint
#'
#' @seealso See the \href{http://documentation.cortical.io/the_power_of_expressions.html}{Cortical documentation} for more information about semantic fingerprinting and expressions
#'
#' @name Expression-class

.Expression <- setClass(

  # Name
  "Expression",
  # data
  slots = c(
    expression = "list",
    fingerprint = "numeric"
  ),
  # Inherits
  contains = "Fingerprint"

)


# CONSTRUCTORS ----

#' Document class
#'
#' The Document class is one of the four core classes in the sfutils package. A Document is a (large) body of text.
#'
#' (From \href{Cortical documentation}{http://documentation.cortical.io/working_with_text.html}) The functionality we offer for text is a little more elaborate than for terms, given the more complex nature of texts. Besides getting a semantic fingerprint (semantic representation) for a given text (the /text endpoint), one can also get a list of keywords extracted from the text, or get the text split up into smaller consecutive chunks, based on information content. We also provide functionality for extracting terms from a text based on part of speech tags. There is also a bulk endpoint for merging several /text requests into just one http request. Finally there is a detect_language endpoint capable of detecting 50 languages.
#'
#' @param text text to be fingerprinted
#' @param ... other options to be passed (uuid, fingerprint)
#'
#' @importFrom uuid UUIDgenerate
#'
#' @rdname Document-class
#' @export
#' @examples
#' \dontrun{
#' # Get data
#' data("company_descriptions")
#'
#' # Get a single text
#' txt <- company_descriptions$unilever$desc
#'
#' # Fingerprint document
#' txt_fp <- do_fingerprint_document(txt)
#' # This is equivalent to above but above is more convenient
#' # Because it can fingerprint documents in bulk
#' txt_fp <- Document(txt)
#'
#' # You can also pass a fingerprint to the Document constructor
#' # In which case the API won't be called
#' txt_fp_3 <- Document(txt, fingerprint = fingerprint(txt_fp_1))
#' }

Document <- function(text, ...) {

  # Check that length is one
  assertthat::assert_that(length(text) ==1,
                          msg = "Only one document may be passed at a time.")
  # Check if length of text long enough
  assertthat::assert_that(nchar(text) >= 50,
                          msg = "Text is too short to fingerprint. Pass a longer text")

  # If (...)
  opts <- list(...)
  uuid <- ifelse("uuid" %in% names(opts), opts$uuid, uuid::UUIDgenerate())

  # If fingerprint is passed to Document (this happens when e.g. documents are printed in bulk)
  # then take this fingerprint. Else, query API
  if("fingerprint" %in% names(opts)) {

    text_fp <- opts$fingerprint

  } else {

    text_fp <- fingerprint_single_text(text)

  }

  # Set up fingerprint class
  fp <- .Fingerprint(type = "document",
                     uuid = uuid)

  # Create class and return
  .Document(
    fp,
    text = text,
    fingerprint = (text_fp + 1) # SF positions run from 0:16383 but R indexes from 1
  )

}

#' Filter class
#'
#' The Filter class is one of the four core classes in the sfutils package. A Filter works as a classification device because the user passes positive & negative examples to construct it.
#'
#' (From \href{Cortical documentation}{http://documentation.cortical.io/classification.html}) This endpoint allows the user to create category filters, simply and easily, using only a small number of example texts. The newly created category filters can then be used as part of a classification process, where items to be classified can be compared against the category filters using our /compare and/or /compare/bulk endpoints (which you can see in our interactive API documentation). See our similarity metrics` guide for information on how to interpret the output from the compare endpoints.
#'
#' @param name Name of the filter
#' @param positive positive example.
#' @param ... other options to be passed: uuid (character) or negative (vector with negative examples)
#'
#' @importFrom uuid UUIDgenerate
#'
#' @export
#'
#' @rdname Filter-class
#'
#' @examples
#' \dontrun{
#' # Create a filter (without negative examples)
#' filt_without_negative <- do_create_filter(
#'   name = "filt1",
#'   positive = c(
#'     "Shoe with a lining to help keep your feet dry and comfortable on wet terrain.",
#'     "running shoes providing protective cushioning."
#'   )
#' )
#'
#' # Create a filter (with negative examples)
#' filt_with_negative <- do_create_filter(
#'   name = "filt2",
#'   positive = c(
#'     "Shoe with a lining to help keep your feet dry and comfortable on wet terrain.",
#'     "running shoes providing protective cushioning."
#'   ),
#'   negative = c(
#'     "The most comfortable socks for your feet.",
#'     "6 feet USB cable basic white"
#'   )
#' )
#' }

Filter <- function(name, positive, ...) {

  # If (...)
  opts <- list(...)
  uuid <- ifelse("uuid" %in% names(opts), opts$uuid, uuid::UUIDgenerate())
  if("negative" %in% names(opts)) {
    negative <- opts$negative
  } else {
    negative <- NULL
  }

  # Check if params valid (length etc)
  assertthat::assert_that(!all(c(length(positive), length(negative)) == 0) ,
                          msg = "Either 'positive' or 'negative' must at least have one term")
  assertthat::assert_that(name > 0,
                          msg = "'name' may not be of length 0")
  # Positive must be at least of length 2
  assertthat::assert_that(length(positive) >= 2,
                          msg = "'positive' examples must contain at least two elements")
  # All entries must be characters
  assertthat::assert_that(all(vapply(positive, function(x) is.character(x), TRUE)),
                          msg = "All entries passed to 'positive' must be characters")
  if(length(negative) > 0) {
    assertthat::assert_that(all(vapply(negative, function(x) is.character(x), TRUE)),
                            msg = "All entries passed to 'negative' must be characters")
  }

  # Call fingerprint class
  fp <- .Fingerprint(type = "filter",
                     uuid = uuid)

  # Create filter
  if(is.null(negative)) {
    filter_fp <- create_categoryfilter(name, positive)
  } else {
    filter_fp <- create_categoryfilter(name, positive, negative = negative)
  }

  # Create class and return
  if(is.null(negative)) {
    .Filter(
      fp,
      name = name,
      positive = positive,
      fingerprint = (filter_fp + 1)
    )
  } else {
    .Filter(
      fp,
      name = name,
      positive = positive,
      negative = negative,
      fingerprint = (filter_fp + 1)
    )
  }
}

#' Term class
#'
#' The Term class is one of the four core classes in the sfutils package. A Term is a single word.
#'
#' (From \href{Cortical documentation}{http://documentation.cortical.io/working_with_terms.html}) The basic building blocks for performing semantic computations are the representations for single terms. Each Retina contains semantic representations (fingerprints) for a large number of terms, and this page describes how to retrieve these from the API. Furthermore we describe how to query the Retina for semantically similar terms, and retrieve a list of contexts in which a given term can occur.
#'
#' @param term term to be fingerprinted
#' @param ... other options to be passed (uuid)
#'
#' @importFrom uuid UUIDgenerate
#'
#' @rdname Term-class
#' @export
#' @examples
#' \dontrun{
#' # Fingerprint a term
#' trm_fp <- do_fingerprint_term("Finance")
#' }

Term <- function(term, ...) {

  # If (...)
  opts <- list(...)
  uuid <- ifelse("uuid" %in% names(opts), opts$uuid, uuid::UUIDgenerate())

  # Call fingerprint class
  fp <- .Fingerprint(type = "term",
                     uuid = uuid)

  # Fingerprint article
  term_resp <- fingerprint_term(term)

  # Create class and return
  .Term(
    fp,
    term = term,
    df = term_resp$df,
    score = term_resp$score,
    pos_types = term_resp$pos_types,
    fingerprint = (term_resp$fingerprint + 1) # SF positions run from 0:16383 but R indexes from 1
  )
}

#' Expression-class
#'
#' The Expression class is one of the four core classes in the sfutils package. An Expression is more complicated than a Term or a Text because one can define operations (subtract, add) on them.
#'
#' (From \href{Cortical documentation}{http://documentation.cortical.io/the_power_of_expressions.html}) As briefly mentioned in the introduction, semantically meaningful operations can be carried out on fingerprints by performing simple binary operations on the positions of the fingerprints. Semantic relationships between fingerprints can be discovered by looking at their overlapping positions in the semantic space. This allows us, for example, to subtract the meaning of one term from the meaning of another term to obtain a more specific representation. In the /expressions endpoint, we offer these binary operations on the fingerprints and, along with this, a flexible way of specifying the input data.
#'
#' @param expression expression to be fingerprinted
#' @param ... other options to be passed (uuid, fingerprint)
#'
#' @importFrom uuid UUIDgenerate
#'
#' @rdname Expression-class
#' @export
#' @examples
#' \dontrun{
#' ##### Single expression
#'
#' # Expressions work with texts and terms
#' # They require a specific format
#'
#' # For a single expression, the input must be a list
#' # With named entries, for example:
#' ex1_body <- list(
#'   "term" = "Finance"
#' )
#' # Fingerprint
#' ex1_fp <- do_fingerprint_expression(ex1_body)
#'
#' # You can combine this with e.g. subtractions:
#' ex2_body <- list(
#'   "term" = "Finance",
#'   "sub" = list(
#'     "term" = "Market",
#'     "positions" = c(2,6,4)
#'   )
#' )
#' # Fingerprint
#' ex2_body <- do_fingerprint_expression(ex2_body)
#'
#' # Plot the difference
#' plot(ex1_fp, ex2_fp)
#'
#' ###### MULTIPLE EXPRESSIONS
#'
#' # The input must be an unnamed list, each entry
#' # Of which is also a list with named entries.
#'
#' # For example, if you want to fingerprint a term
#' # As an expression, do:
#' ex3_body <- list(
#'   list(
#'     "term" = "Finance"
#'   ),
#'   list(
#'     "term" = "Finance",
#'     "sub" = list(
#'       "term" = "Market",
#'       "positions" = c(2,6,4)
#'     )
#'   )
#' )
#'
#' # This will return a Collection
#' # Fingerprint
#' ex3_fp <- do_fingerprint_expression(ex3_body)
#' }

Expression <- function(expression, ...) {

  # If (...)
  opts <- list(...)
  uuid <- ifelse("uuid" %in% names(opts), opts$uuid, uuid::UUIDgenerate())
  # If fingerprint is passed to Expression (this happens when e.g. expressions are printed in bulk)
  # then take this fingerprint. Else, query API
  if("fingerprint" %in% names(opts)) {
    exp_fp <- opts$fingerprint
  } else {
    exp_fp <- fingerprint_single_expression(expression)
  }

  # Set up fingerprint class
  fp <- .Fingerprint(type = "expression",
                     uuid = uuid)

  # Create class and return
  .Expression(
    fp,
    expression = expression,
    fingerprint = (exp_fp + 1) # SF positions run from 0:16383 but R indexes from 1
  )

}
