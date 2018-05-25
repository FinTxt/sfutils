# What are we testing?
context("test-Collection_class.R")

# Test
test_that("We can create and compute with Collections", {

  # Get data
  data("fps_train")

  # To collection
  fps <- fps_train$fingerprints
  fps_col <- as.collection(fps)

  # To list
  tst_list <- as.list(fps_col)

  # To sparse binary matrix
  mat <- as.matrix(fps_col)

})
