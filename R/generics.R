# --------------------------------------------------------------------------
#
# Generics for S4 classes that are not yet defined in R go here.
#
# The generics for getters can be found in 'getters.R' and are not included
# in this file.
#
# The generics are subdivided into the following sections:
#
# GENERICS API
#
# Generics that have to do with calling the Cortical API (e.g. retrieving)
# similar terms
#
# GENERICS OTHER
#
# Generics used for internal (R) functions
#
# --------------------------------------------------------------------------

# GENERICS API ----

#' Retrieve a list of keywords for a text
#'
#' This function retrieves 10 keywords for the text
#'
#' @param object a Document class object
#'
#' @return list of keywords
#'
#' @export
#' @docType methods
#' @rdname get_keywords-methods
#'
#' @examples
#' \dontrun{
#' # Get data
#' data("company_descriptions")
#'
#' # Get one text
#' txt <- company_descriptions$unilever$desc
#'
#' # Fingerprint document
#' txt_fp <- do_fingerprint_document(txt)
#'
#' # Get keywords
#' kw <- get_keywords(txt_fp)
#' }

setGeneric("get_keywords", function(object) {

  standardGeneric("get_keywords")

})

#' Get contexts for a fingerprint
#'
#' Retrieve context for a Term or an Expression context
#'
#' @param object Either: a single Term or Expression class object OR a Collection containing Term or Expression class objects
#'
#' @return list containing different contexts
#'
#' @rdname get_context-methods
#' @export
#' @docType methods
#'

setGeneric("get_context", function(object, ...) {

  standardGeneric("get_context")

})

#' Get similar terms for a fingerprint
#'
#' Retrieve similar terms for a Term or an Expression object
#'
#' @param object Either: a single Term or Expression class object OR a Collection containing Term or Expression class objects
#'
#' @return list containing different contexts
#'
#' @rdname get_similar_terms-methods
#' @export
#' @docType methods
#'

setGeneric("get_similar_terms", function(object, ...) {

  standardGeneric("get_similar_terms")

})

# GENERICS OTHER ----

#' Similarity/Distance between fingerprints
#'
#' Calculate a variety of similarity and distance metrics
#'
#' You can compare either a sparse binary matrix obtained by turning a Collection object into a matrix with another fingerprint (a Document, Expression, Term or Filter), or by simply passing two Fingerprint objects.
#'
#' @param x either an object of class Filter, Expression, Term or Document or an object of class 'cdgMatrix' for which you want to calculate similarities. This matrix can be obtained by calling 'as.matrix()' on a Collection class.
#' @param y reference fingerprint. Can be: a Filter, Expression, Term or Document class
#' @param method one of the following: "cosine", "jaccard", "dice", "gilbertwells", "dennis", "sorgenfrei" (similarity) or "lancewilliams", "euclid", "hamming" (distance)
#'
#' @return similarity or distance metric between two fingerprints or a matrix of length n of similarity/distance metrics between documents and reference fingerprint
#'
#' @seealso \href{http://www.iiisci.org/journal/CV$/sci/pdfs/GS315JG.pdf}{This} paper with similarity metrics
#'
#' @export
#' @docType methods
#' @rdname do_compare-methods
#'
#' @examples
#' \dontrun{
#' # Get data
#' data("company_descriptions")
#'
#' # Put text in a list
#' txt <- lapply(company_descriptions, function(x) x$desc)
#'
#' # Fingerprint documents
#' txt_fp <- do_fingerprint_document(txt)
#'
#' # Fingerprint a term
#' trm_fp <- do_fingerprint_term("finance")
#'
#' # We can compare:
#' #  - a document with a document
#' do_compare(txt_fp[[1]], txt_fp[[2]])
#' #  - a term with a document
#' do_compare(txt_fp[[1]], trm_fp)
#' #  - an expression with a document
#' #  ... anything with a fingerprint
#'
#' # We can also compare a sparse binary matrix
#' # with another fingeprint
#'
#' # Convert the fingerprinted documents to a matrix
#' txt_fp_mat <- as.matrix(txt_fp)
#' # Compare to term
#' do_compare(txt_fp_mat, trm_fp)
#' }

setGeneric("do_compare", function(x,y, method=c("cosine", "jaccard", "dice",
                                                "gilbertwells", "dennis",
                                                "sorgenfrei", "lancewilliams",
                                                "euclid", "hamming")) {

  standardGeneric("do_compare")

})

