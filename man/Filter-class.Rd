% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classes_core.R
\docType{class}
\name{Filter-class}
\alias{Filter-class}
\alias{.Filter}
\alias{Filter}
\title{Filter class}
\usage{
Filter(name, positive, ...)
}
\arguments{
\item{name}{Name of the filter}

\item{positive}{positive example.}

\item{...}{other options to be passed: uuid (character) or negative (vector with negative examples)}
}
\description{
The Filter class is one of the four core classes in the sfutils package. A Filter works as a classification device because the user passes positive & negative examples to construct it.
}
\details{
(From \href{Cortical documentation}{http://documentation.cortical.io/classification.html}) This endpoint allows the user to create category filters, simply and easily, using only a small number of example texts. The newly created category filters can then be used as part of a classification process, where items to be classified can be compared against the category filters using our /compare and/or /compare/bulk endpoints (which you can see in our interactive API documentation). See our similarity metrics` guide for information on how to interpret the output from the compare endpoints.
}
\section{Slots}{

\describe{
\item{\code{name}}{Name of the filter}

\item{\code{positive}}{positive examples (should be contained in text)}

\item{\code{negative}}{negative examples (should be contained in text)}

\item{\code{fingerprint}}{numeric vector of the fingerprint}
}}

\examples{
\dontrun{
# Create a filter (without negative examples)
filt_without_negative <- do_create_filter(
  name = "filt1",
  positive = c(
    "Shoe with a lining to help keep your feet dry and comfortable on wet terrain.",
    "running shoes providing protective cushioning."
  )
)

# Create a filter (with negative examples)
filt_with_negative <- do_create_filter(
  name = "filt2",
  positive = c(
    "Shoe with a lining to help keep your feet dry and comfortable on wet terrain.",
    "running shoes providing protective cushioning."
  ),
  negative = c(
    "The most comfortable socks for your feet.",
    "6 feet USB cable basic white"
  )
)
}
}
\seealso{
See the \href{http://documentation.cortical.io/classification.html}{classification documentation} for more information about filters.
}
