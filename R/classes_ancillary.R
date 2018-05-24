# --------------------------------------------------------------------------
#
# R S4 ancillary classes go here.
#
# These classes are useful constructs in R to work with e.g. multiple documents
# or to represent the documents in a different form (e.g. a sparse binary matrix)
#
# --------------------------------------------------------------------------

#' @slot intensity vector with numeric data. Intensities over all fingerprints supplied.
#'
#' @name Intensity-class

.Intensity <- setClass(
  # Name
  "Intensity",
  # data
  slots = c(
    intensity = "numeric"
  )
)

#' @slot entries list of documents, terms or expressions
#'
#' @name Collection-class

.Collection <- setClass(
  # Name
  "Collection",
  # data
  slots = c(
    entries = "list"
  ),
  contains = "Intensity"
)
