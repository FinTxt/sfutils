# What are we testing?
context("test-Collection_class.R")

# Test
test_that("We can create and compute with Collections", {

  # Get data
  data("fps_train")

  # To collection
  fps <- fps_train$fingerprints
  fps_col <- as.collection(fps)

  # Is collection?
  testthat::expect_s4_class(fps_col, "Collection")
  testthat::expect_true(all(is(fps_col) == c("Collection", "Intensity")))
  testthat::expect_length(fps_col, 690)
  testthat::expect_true(all(slotNames(fps_col) == c("entries", "intensity")))

  # To list
  tst_list <- as.list(fps_col)

  # To sparse binary matrix
  mat <- as.matrix(fps_col)
  # Tests
  testthat::expect_s4_class(mat, "dgCMatrix")
  testthat::expect_equal(nrow(mat), 690)
  testthat::expect_equal(ncol(mat), 16384)

})
