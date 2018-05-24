# --------------------------------------------------------------------------
#
# Generic utilities (helper) functions used by the package. These are sub-
# divided into the following categories:
#
# DISTANCE & SIMILARITY METHODS
#
# This section contains several methods that can be used to compare fingerprints
# or, for that matter, any two vectors that need to be compared based on
# bitwise operations.
#
# The methods in this file are based on the following paper:
#   http://www.iiisci.org/journal/CV$/sci/pdfs/GS315JG.pdf
#
# These methods are not available as functions to the end-user (they are not
# exported). Instead, they are called using the do_compare() function.
#
# --------------------------------------------------------------------------

# DISTANCE & SIMILARITY METHODS ----

#' Cosine similarity metric
#'
#' @param fp1 fingerprint in binary format. That is, if a retina contains X positions, the fingerprint contains X 1s and 0s depending on whether that position is in the fingerprint
#' @param fp2 fingerprint in binary format. That is, if a retina contains X positions, the fingerprint contains X 1s and 0s depending on whether that position is in the fingerprint
#'
#'
#' @noRd

cosine_similarity_util <- function(fp1, fp2) {

  # Where both t1 and t2 are 1
  a <- crossprod(fp1, fp2)

  # Where t1 is 0 and t2 is 1
  b <- crossprod(!fp1, fp2)

  # Where t1 is 1 and t2 is 0
  c <- crossprod(fp1, !fp2)

  # Where both are 0
  d <- crossprod(!fp1, !fp2)

  # Return cosine similarity
  (a / sqrt((a + b) * (a + c)))

}

#' Jaccard similarity matric
#'
#' @param fp1 fingerprint in binary format. That is, if a retina contains X positions, the fingerprint contains X 1s and 0s depending on whether that position is in the fingerprint
#' @param fp2 fingerprint in binary format. That is, if a retina contains X positions, the fingerprint contains X 1s and 0s depending on whether that position is in the fingerprint
#'
#' @noRd

jaccard_similarity_util <- function(fp1, fp2) {

  # Where both t1 and t2 are 1
  a <- crossprod(fp1, fp2)

  # Where t1 is 0 and t2 is 1
  b <- crossprod(!fp1, fp2)

  # Where t1 is 1 and t2 is 0
  c <- crossprod(fp1, !fp2)

  # Where both are 0
  d <- crossprod(!fp1, !fp2)

  # Return jaccard similarity
  (a / (a + b + c))

}

#' Dice similarity
#'
#' @param fp1 fingerprint in binary format. That is, if a retina contains X positions, the fingerprint contains X 1s and 0s depending on whether that position is in the fingerprint
#' @param fp2 fingerprint in binary format. That is, if a retina contains X positions, the fingerprint contains X 1s and 0s depending on whether that position is in the fingerprint
#'
#' @noRd

dice_similarity_util <- function(fp1, fp2) {

  # Where both t1 and t2 are 1
  a <- crossprod(fp1, fp2)

  # Where t1 is 0 and t2 is 1
  b <- crossprod(!fp1, fp2)

  # Where t1 is 1 and t2 is 0
  c <- crossprod(fp1, !fp2)

  # Where both are 0
  d <- crossprod(!fp1, !fp2)

  # Return similarity
  ((2*a) / ((2*a) + b + c))

}

#' Euclid distance metric
#'
#' @param fp1 fingerprint in binary format. That is, if a retina contains X positions, the fingerprint contains X 1s and 0s depending on whether that position is in the fingerprint
#' @param fp2 fingerprint in binary format. That is, if a retina contains X positions, the fingerprint contains X 1s and 0s depending on whether that position is in the fingerprint
#'
#' @noRd

euclid_distance_util <- function(fp1, fp2) {

  # Where both t1 and t2 are 1
  a <- crossprod(fp1, fp2)

  # Where t1 is 0 and t2 is 1
  b <- crossprod(!fp1, fp2)

  # Where t1 is 1 and t2 is 0
  c <- crossprod(fp1, !fp2)

  # Where both are 0
  d <- crossprod(!fp1, !fp2)

  # Return distance
  sqrt(b + c)

}

#' Hamming distance metric
#'
#' @param fp1 fingerprint in binary format. That is, if a retina contains X positions, the fingerprint contains X 1s and 0s depending on whether that position is in the fingerprint
#' @param fp2 fingerprint in binary format. That is, if a retina contains X positions, the fingerprint contains X 1s and 0s depending on whether that position is in the fingerprint
#'
#' @noRd

hamming_distance_util <- function(fp1, fp2) {

  # Where both t1 and t2 are 1
  a <- crossprod(fp1, fp2)

  # Where t1 is 0 and t2 is 1
  b <- crossprod(!fp1, fp2)

  # Where t1 is 1 and t2 is 0
  c <- crossprod(fp1, !fp2)

  # Where both are 0
  d <- crossprod(!fp1, !fp2)

  # Return distance
  (b + c)

}

#' Lance & Williams distance
#'
#' @param fp1 fingerprint in binary format. That is, if a retina contains X positions, the fingerprint contains X 1s and 0s depending on whether that position is in the fingerprint
#' @param fp2 fingerprint in binary format. That is, if a retina contains X positions, the fingerprint contains X 1s and 0s depending on whether that position is in the fingerprint
#'
#' @noRd

lancewilliams_distance_util <- function(fp1, fp2) {

  # Where both t1 and t2 are 1
  a <- crossprod(fp1, fp2)

  # Where t1 is 0 and t2 is 1
  b <- crossprod(!fp1, fp2)

  # Where t1 is 1 and t2 is 0
  c <- crossprod(fp1, !fp2)

  # Where both are 0
  d <- crossprod(!fp1, !fp2)

  # Return distance
  (b + c) / ((2*a) + b + c)

}

#' Gilbert & Wells similarity
#'
#' @param fp1 fingerprint in binary format. That is, if a retina contains X positions, the fingerprint contains X 1s and 0s depending on whether that position is in the fingerprint
#' @param fp2 fingerprint in binary format. That is, if a retina contains X positions, the fingerprint contains X 1s and 0s depending on whether that position is in the fingerprint
#'
#' @noRd

gilbertwells_similarity_util <- function(fp1, fp2) {

  # Where both t1 and t2 are 1
  a <- crossprod(fp1, fp2)

  # Where t1 is 0 and t2 is 1
  b <- crossprod(!fp1, fp2)

  # Where t1 is 1 and t2 is 0
  c <- crossprod(fp1, !fp2)

  # Where both are 0
  d <- crossprod(!fp1, !fp2)

  # Return similarity
  (log(a) - log(a + b + c + d) - log((a+b)/ (a + b + c + d)) - log((a + c) / (a + b + c + d)))

}

#' Sorgenfrei similarity
#'
#' @param fp1 fingerprint in binary format. That is, if a retina contains X positions, the fingerprint contains X 1s and 0s depending on whether that position is in the fingerprint
#' @param fp2 fingerprint in binary format. That is, if a retina contains X positions, the fingerprint contains X 1s and 0s depending on whether that position is in the fingerprint
#'
#' @noRd

sorgenfrei_similarity_util <- function(fp1, fp2) {

  # Where both t1 and t2 are 1
  a <- crossprod(fp1, fp2)

  # Where t1 is 0 and t2 is 1
  b <- crossprod(!fp1, fp2)

  # Where t1 is 1 and t2 is 0
  c <- crossprod(fp1, !fp2)

  # Where both are 0
  d <- crossprod(!fp1, !fp2)

  # Return similarity
  a^2 / ((a+b) * (a + c))

}

#' Dennis similarity
#'
#' @param fp1 fingerprint in binary format. That is, if a retina contains X positions, the fingerprint contains X 1s and 0s depending on whether that position is in the fingerprint
#' @param fp2 fingerprint in binary format. That is, if a retina contains X positions, the fingerprint contains X 1s and 0s depending on whether that position is in the fingerprint
#'
#' @noRd

dennis_similarity_util <- function(fp1, fp2) {

  # Where both t1 and t2 are 1
  a <- crossprod(fp1, fp2)

  # Where t1 is 0 and t2 is 1
  b <- crossprod(!fp1, fp2)

  # Where t1 is 1 and t2 is 0
  c <- crossprod(fp1, !fp2)

  # Where both are 0
  d <- crossprod(!fp1, !fp2)

  # Return similarity
  ((a*d) - (b*c)) / sqrt((a + b + c + d) * (a+b) * (a+c)) # a + b + c + d == n == total

}
