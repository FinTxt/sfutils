## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=TRUE, echo=FALSE----------------------------------------------
suppressPackageStartupMessages(library(FinTxtUtils))
suppressPackageStartupMessages(library(dplyr))

## ---- message=FALSE, eval=TRUE-------------------------------------------
# Clean wd
rm(list=ls())
# Load data
data("fps_train")
data("fps_test")
# To list
fps_train <- as.list(fps_train)
fps_test <- as.list(fps_test)

# Unpack
train_lbls_b <- fps_train$label_binomial
train_lbls_m <- fps_train$label_multinomial
fps_train <- fps_train$fingerprints

test_lbls_b <- fps_test$label_binomial
test_lbls_m <- fps_test$label_multinomial
fps_test <- fps_test$fingerprints

## ------------------------------------------------------------------------
# Turn fingerprints into Collection
col_train <- Collection(fps_train)
col_test <- Collection(fps_test)

## ---- eval=FALSE, fig.height=4, fig.width=4, fig.show='hold'-------------
#  # Let's see if we can pick out the oil & gas companies
#  # Create a filter. Currently I've not implemented that either can be empty.
#  filt <- Filter(name = "crude",
#                 positive = c("oil", "gas", "saudi-arabia", "middle east",
#                              "oil platform", "oil reserves", "fuel", "gasoline", "crude oil",
#                              "oil pipeline", "petroleum",
#                              "opec", "oil well", "fracking", "oil field", "energy"))
#  # We can compare the fingerprint plot of the filter to the intensity plot of articles about crude oil
#  crude_index <- which(train_lbls_b == "crude")
#  crude_intense <- Collection(fps_train[crude_index])
#  plot(crude_intense)
#  plot(filt)

## ------------------------------------------------------------------------
# Compare two fingerprints
methods <- c("cosine", "jaccard", "dice", "gilbertwells",
  "dennis", "sorgenfrei", "lancewilliams", "euclid", "hamming")
type <- c(rep("similarity", 6), rep("distance", 3))
# Compute for all methods
mm <- purrr::map2_df(methods, type, function(x, y) {
  data_frame(
    "metric" = x,
    "type" = y,
    "value" = round(do_compare(fps_train[[1]], fps_train[[2]], method = x)[1,1],
                    digits = 3)
  )
})

knitr::kable(mm)


## ------------------------------------------------------------------------
# Turn test and train into a sparse binary matrix
mtrain <- as.matrix(col_train)
mtest <- as.matrix(col_test)

mtrain[1:5, 1:8]

## ------------------------------------------------------------------------
# Compare documents to another fingerprinted document
comp <- do_compare(mtrain, fps_train[[1]], method = "cosine")

knitr::kable(as.data.frame(comp[1:10,]))

## ------------------------------------------------------------------------
# Quick convenience function
get_most_similar_articles <- function(doc) {
  # Compare documents to another fingerprinted document
  comp <- do_compare(mtrain, doc, method = "cosine")
  # To data frame
  comp_df <- data_frame("sim" = comp[,1],
                        "uuid" = row.names(comp)) %>%
                arrange(desc(sim)) %>%
                slice(2:3) %>%
                select(uuid) %>%
                pull()
  # Get text
  texts <- purrr::map(fps_train, function(x) if(uuid(x) %in% comp_df) text(x))
  # Remove null
  texts <- texts[sapply(texts, function(x) !is.null(x))]
  # Cat
  prnt <- paste0("INPUT DOC: \n\n", 
                 text(doc), "\n\n", 
                 paste0(paste0("RECOMMENDATION: \n\n",
                               unlist(texts)), collapse = "\n\n"))
  cat(prnt)
}
# Test function
get_most_similar_articles(fps_train[[45]])

