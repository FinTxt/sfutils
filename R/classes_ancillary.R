# --------------------------------------------------------------------------
#
# R S4 ancillary classes go here.
#
# These classes are useful constructs in R to work with e.g. multiple documents
# or to represent the documents in a different form (e.g. a sparse binary matrix)
#
# This script contains:
#
# 1. Ancillary classes
# 2. Constructors for these classes
#
# --------------------------------------------------------------------------

# ANCILLARY CLASSES -----

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


# CONSTRUCTORS -----

#' Collection class
#'
#' A Collection is simply a grouping of one or multiple Documents, Expressions or Terms. You can create a Collection on any combination of these.
#'
#' @param entry_list list of objects containing one of the following classes: Term, Document or Expression
#'
#' @author Jasper Ginn
#' @export
#'
#' @rdname Collection-class
#' @export
#' @examples
#' \dontrun{
#' # Get reuters subset data
#' data("fps_train")
#'
#' # Retrieve documents
#' docs <- fps_train$fingerprints
#'
#' # This is a list of fingerprinted documents
#' # They can be turned into a collection as follows
#' col <- Collection(docs)
#'
#' # You can turn them back into a list
#' col_list <- as.list(col)
#'
#' # Or you can turn them into a sparse binary matrix
#' sbm <- as.matrix(col)
#' }

Collection <- function(entry_list) {

  # Make sure that only proper classes are passed
  prop_class <- unlist(lapply(entry_list, function(x) {
    any(c("Term", "Expression", "Document") %in% is(x))
  }))
  # If not all, then error
  if(!all(prop_class)) {
    stop("All entries must belong to either the 'Term', 'Document' or 'Expression' class.")
  }

  # Make intensity
  int <- Intensity(entry_list)

  # Make class
  .Collection(
    int,
    entries = entry_list
  )

}

#' Constructor for Intensity class
#'
#' @param fp_list list of objects with a fingerprint
#'
#' @author Jasper Ginn
#' @export

Intensity <- function(fp_list) {

  # Calculate intensity
  conv <- function(positions, fingerprints, counter) {
    # Take counter
    fp_now <- fingerprints[[counter]]
    # Add to positions
    positions[fp_now] <- positions[fp_now] + 1
    # If counter == length fingerprints, return
    if(counter == length(fingerprints)) {
      # Take total number of positions where != 0
      n <- sum(which(positions > 0))
      return((positions / n) * 100)
    } else # Else, continue
      conv(positions, fingerprints, counter + 1)
  }
  # Calculate
  pos_filled <- conv(rep(0, 16384),
                     lapply(fp_list, function(x) fingerprint(x)),
                     1)

  # Make class
  .Intensity(
    intensity = pos_filled
  )

}
