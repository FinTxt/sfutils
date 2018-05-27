# What are we testing?
context("test-Datasets.R")

# Test
test_that("Datasets have the correct dimensions, lengths and are of the right types", {

  ### Company descriptions data

  # Load company descriptions
  data("company_descriptions")

  # Length
  testthat::expect_length(company_descriptions, 6)

  # Entries of each element

  r <- lapply(company_descriptions, function(x) {
    testthat::expect_equal(names(x),
                          c("desc", "name", "ticker", "url"))
  })

  # All entries should have text that is longer than 50 characters
  r <- lapply(company_descriptions, function(x) {
    testthat::expect_gte(nchar(x$desc), 50)
  })

  ### FPS train data
  data("fps_train")

  # Names
  testthat::expect_equal(
    names(fps_train),
    c("fingerprints",
      "label_binomial",
      "label_multinomial")
  )

  # Dimensions
  fp_length <- length(fps_train$fingerprints)
  bin_length <- length(fps_train$label_binomial)
  mult_length <- length(fps_train$label_multinomial)

  # Test
  testthat::expect_equal(fp_length,
                        bin_length)
  testthat::expect_equal(fp_length,
                        mult_length)
  testthat::expect_equal(bin_length,
                        bin_length)

  # Fingerprints are all documents
  type <- unlist(lapply(fps_train$fingerprints, function(x) type(x)))
  testthat::expect_equal(all(type == "document"), TRUE)

})
