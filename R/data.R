# --------------------------------------------------------------------------
#
# roxygen2 documentation of datasets included in this package goes here.
#
# --------------------------------------------------------------------------


#' Fingerprints and labels for 690 documents (train set) and 189 documents (test set)
#'
#' 690 Fingerprinted documents (fps_train) and 189 fingerprinted documents (fps_test) belonging to nine categories taken from the 'reuters21578' dataset in the 'tm.corpus.Reuters21578' package. The data has been processed such that a) only articles are considered that belong to only one class, b) only articles belonging to one of the following topics are considered: grain, corn, crude, livestock, wheat, coffee, sugar, gold, copper, cocoa. The labels_binomial variable is created by recoding the topics as 'crude' if the topic belongs to 'crude' and 'other' if the topic belongs to one of the other classes.
#'
#' @format A list with three entries:
#' 1. A vector called 'label_binomial' with two class labels: 'crude' and 'other'
#' 2. A vector called 'label_multinomial' with nine, original class labels
#' 3. A list of 690 (train) or 189 (test) fingerprinted documents of S4 class 'Document'
#' \describe{
#'   \item{text}{original document}
#'   \item{fingerprint}{fingerprint of document}
#'   \item{uuid}{unique id}
#'   \item{type}{type of the document that was fingerprinted}
#' }
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Reuters-21578+Text+Categorization+Collection}
#' @rdname sfutils-data
"fps_train"

#' @rdname sfutils-data
"fps_test"

#' Small sample of business descriptions of six companies
#'
#' Name, ticker, url and business description of 6 companies. Taken from Thompson Reuters.
#'
#' @format A list, each entry of which is a list (one per company)
#' \describe{
#'   \item{desc}{business description}
#'   \item{name}{name of the company}
#'   \item{ticker}{ticker of the company}
#'   \item{url}{url to the company page on Thompson Reuters website}
#' }
#' @source \url{https://www.reuters.com/markets/stocks}
#' @rdname sfutils-data
"company_descriptions"
