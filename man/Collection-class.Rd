% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classes_ancillary.R
\docType{class}
\name{Collection-class}
\alias{Collection-class}
\alias{.Collection}
\alias{Collection}
\title{Collection class}
\usage{
Collection(entry_list)
}
\arguments{
\item{entry_list}{list of objects containing one of the following classes: Term, Document or Expression}
}
\description{
A Collection is simply a grouping of one or multiple Documents, Expressions or Terms. You can create a Collection on any combination of these.
}
\section{Slots}{

\describe{
\item{\code{entries}}{list of documents, terms or expressions}
}}

\examples{
\dontrun{
# Get reuters subset data
data("fps_train")

# Retrieve documents
docs <- fps_train$fingerprints

# This is a list of fingerprinted documents
# They can be turned into a collection as follows
col <- Collection(docs)

# You can turn them back into a list
col_list <- as.list(col)

# Or you can turn them into a sparse binary matrix
sbm <- as.matrix(col)
}
}
\author{
Jasper Ginn
}
