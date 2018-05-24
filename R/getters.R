# --------------------------------------------------------------------------
#
# These functions all deal with retrieving specific slots from S4 objects
# (e.g. fingerprint, uuid etc.).
#
# --------------------------------------------------------------------------

# GENERICS -----

#' Get slots from FinTxtUtuls classes
#'
#' These functions retrieve slots from FinTxtUtuls classes (Fingerprint, Term, Document, Expression, Collection).
#'
#' @param object a class object containing the slot
#'
#' @rdname getters
#' @docType methods
#' @export

setGeneric("uuid", function(object) {

  standardGeneric("uuid")

})

#' @rdname getters
#' @export

setGeneric("type", function(object) {

  standardGeneric("type")

})

#' @rdname getters
#' @export

setGeneric("fingerprint", function(object) {

  standardGeneric("fingerprint")

})

#' @rdname getters
#' @export

setGeneric("text", function(object) {

  standardGeneric("text")

})

#' @rdname getters
#' @export

setGeneric("sfexpression", function(object) {

  standardGeneric("sfexpression")

})

#' @rdname getters
#' @export

setGeneric("term", function(object) {

  standardGeneric("term")

})

#' @rdname getters
#' @export

setGeneric("df", function(object) {

  standardGeneric("df")

})

#' @rdname getters
#' @export

setGeneric("score", function(object) {

  standardGeneric("score")

})

#' @rdname getters
#' @export

setGeneric("pos_types", function(object) {

  standardGeneric("pos_types")

})

#' @rdname getters
#' @export

setGeneric("name", function(object) {

  standardGeneric("name")

})

#' @rdname getters
#' @export

setGeneric("positive", function(object) {

  standardGeneric("positive")

})

#' @rdname getters
#' @export

setGeneric("negative", function(object) {

  standardGeneric("negative")

})

#' @rdname getters
#' @export

setGeneric("entries", function(object) {

  standardGeneric("entries")

})


# METHODS ----

#' @rdname getters

setMethod("uuid",
          "Fingerprint",
          function(object) {
            object@uuid
          })

#' @rdname getters

setMethod("fingerprint",
          "Fingerprint",
          function(object) {
            object@fingerprint
          })

#' @rdname getters

setMethod("type",
          "Fingerprint",
          function(object) {
            object@type
          })

#' @rdname getters

setMethod("text",
          "Document",
          function(object) {
            object@text
          })

#' @rdname getters

setMethod("sfexpression",
          "Expression",
          function(object) {
            object@expression
          })

#' @rdname getters

setMethod("term",
          "Term",
          function(object) {
            object@term
          })

#' @rdname getters

setMethod("df",
          "Term",
          function(object) {
            object@df
          })

#' @rdname getters

setMethod("score",
          "Term",
          function(object) {
            object@score
          })

#' @rdname getters

setMethod("pos_types",
          "Term",
          function(object) {
            object@pos_types
          })

#' @rdname getters

setMethod("name",
          "Filter",
          function(object) {
            object@name
          })

#' @rdname getters

setMethod("positive",
          "Filter",
          function(object) {
            object@positive
          })

#' @rdname getters
#'

setMethod("negative",
          "Filter",
          function(object) {
            object@negative
          })

#' @rdname getters

setMethod("entries",
          "Collection",
          function(object) {
            object@entries
          })
