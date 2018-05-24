# --------------------------------------------------------------------------
#
# R S4 core classes go here.
#
# These classes represent the four core classes of the semantic folding API:
# terms, expressions, texts and category filters. They all inherit from the
# "Fingerprint" class.
#
# --------------------------------------------------------------------------

#' Fingerprint class
#'
#' This class forms the basis for the core cortical API classes (Document, Term, Expression, Filter). It should not be called by the user directly.
#'
#' @slot uuid unique id for the fingerprint. Can be passed, else generated.
#' @slot type type of fingerprint (e.g. document if text, filter if filter)
#'
#' @seealso See the \href{http://documentation.cortical.io/intro.html}{Cortical documentation} for more information about semantic fingerprinting
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
