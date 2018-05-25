# What are we testing?
context("test-Term_class.R")

# Test
test_that("We can fingerprint a term and it contains the right number of slots, none of which are empty", {

  # Fingerprint a term
  trm <- do_fingerprint_term("jaguar")

  # Expect these S4 classes
  expect_s4_class(trm,
                  "Term")
  expect_s4_class(trm,
                  "Fingerprint")

  # Expect these slot names
  expect_setequal(slotNames(trm), c("term", "df", "score", "pos_types",
                                   "fingerprint", "uuid", "type"))

  # Expect that none of the slot names are empty (and test that the getters work)
  slot_lengths <- c(length(fingerprint(trm)),
                    length(uuid(trm)),
                    length(term(trm)),
                    length(type(trm)),
                    length(df(trm)),
                    length(score(trm)),
                    length(pos_types(trm)))
  expect_true(all(slot_lengths > 0))

  # Expect a document type
  expect_equal(type(trm), "term")

  # Test that we can get contexts
  cntxts <- get_context(trm)

  # There should be 7 objects
  expect_length(cntxts, 7)

  # Test that we can get terms
  trms <- get_similar_terms(trm)

  # There should be 10 elements
  expect_length(trms, 10)

})
