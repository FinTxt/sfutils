# What are we testing?
context("test-Document_class.R")

# Test
test_that("We can fingerprint a text and it contains the right number of slots, none of which are empty", {
  # Put tests in function
  test <- function(fp) {
    # Expect these S4 classes
    expect_s4_class(fp,
                    "Document")
    expect_s4_class(fp,
                    "Fingerprint")
    # Expect these slot names
    expect_setequal(slotNames(fp), c("text", "fingerprint", "uuid", "type"))
    # Expect that none of the slot names are empty (and test that the getters work)
    slot_lengths <- c(length(fingerprint(fp)),
                      length(uuid(fp)),
                      length(text(fp)),
                      length(type(fp)))
    expect_true(all(slot_lengths > 0))
    # Expect a document type
    expect_equal(type(fp), "document")
  }

  # Load test data
  data("company_descriptions")

  # Get fingerprint from API
  fp_from_API <- do_fingerprint_document(company_descriptions[[1]]$desc)
  test(fp_from_API)

  # Get fingerprint from existing document
  data("fps_train")
  fp_from_existing <- Document(text = text(fps_train$fingerprints[[1]]),
                               fingerprint = fingerprint(fps_train$fingerprints[[1]]))
  # Test
  test(fp_from_existing)

})
