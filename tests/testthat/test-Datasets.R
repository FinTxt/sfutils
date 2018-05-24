# What are we testing?
context("test-Datasets.R")

# Test
test_that("Datasets have the correct dimensions, lengths and are of the right types", {

  ### Company descriptions data

  # Load company descriptions
  data("company_descriptions")

  # Length
  assertthat::assert_that(
    length(company_descriptions) == 6,
    msg = "'company_descriptions' data should have 6 entries"
  )

  # Entries of each element
  r <- lapply(company_descriptions, function(x) {
    assertthat::are_equal(names(x),
                          c("desc", "name", "ticker", "url"))
  })

  # All entries should have text that is longer than 50 characters
  r <- lapply(company_descriptions, function(x) {
    assertthat::assert_that(nchar(x$desc) >= 50,
                            msg = "Not all entries have a description of at least 50 characters")
  })

  ### FPS train data
  data("fps_train")

  # Names
  assertthat::are_equal(
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
  assertthat::are_equal(fp_length,
                        bin_length)
  assertthat::are_equal(fp_length,
                        mult_length)
  assertthat::are_equal(bin_length,
                        bin_length)

  # Fingerprints are all documents
  type <- unlist(lapply(fps_train$fingerprints, function(x) type(x)))
  assertthat::are_equal(all(type == "document"), TRUE)

})
