## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(dplyr)
library(FinTxtUtils)

data("fps_train")
fps <- fps_train$fingerprints
fps_col <- Collection(fps)
fps_col_mat <- as.matrix(fps_col)

# Create search function
search <- function(query) {

  t1 <- Sys.time()

  # Create expression
  exp <- do_fingerprint_expression(query[[1]])

  # Compare
  res <- do_compare(fps_col_mat, exp)
  # Change column name
  colnames(res) <- "res"
  # Save row names (these are document IDs)
  rn <- row.names(res)

  # Get the closest matches
  res_ordered <- res %>%
    as_data_frame() %>%
    mutate("doc_id" = rn) %>%
    arrange(desc(res)) %>%
    slice(1:10)

  # Retrieve documents
  docs <- lapply(fps, function(x) if(uuid(x) %in% res_ordered$doc_id) text(x))
  docs <- docs[sapply(docs, function(x) !is.null(x))]

  # Cat
  for(doc in docs) {
    cat(doc)
    cat("\n\n")
  }
  Sys.time() - t1

}

## ------------------------------------------------------------------------
# Example of search
query <- create_expression() %>%
  AND() %>%
  add_term("petroleum") %>%
  add_term("oil") %>%
  add_term("OPEC") %>%
  END() %>%
  build_expression()

# Search
search(query)

