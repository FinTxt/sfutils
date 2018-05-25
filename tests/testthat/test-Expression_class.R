# What are we testing?
context("test-Expression_class.R")

# Test
test_that("We can fingerprint an Expression and it contains the right number of slots, none of which are empty", {

  # Create a single expression
  exp <- list(
    "term" = "jaguar"
  )

  # Fingerprint a term
  expr <- do_fingerprint_expression(exp)

  # Expect these S4 classes
  expect_s4_class(expr,
                  "Expression")
  expect_s4_class(expr,
                  "Fingerprint")

  # Expect these slot names
  expect_setequal(slotNames(expr), c("expression", "fingerprint", "uuid",
                                     "type"))

  # Expect that none of the slot names are empty (and test that the getters work)
  slot_lengths <- c(length(fingerprint(expr)),
                    length(uuid(expr)),
                    length(sfexpression(expr)),
                    length(type(expr)))
  expect_true(all(slot_lengths > 0))

  # Expect a document type
  expect_equal(type(expr), "expression")

  # Test that we can get contexts
  cntxts <- get_context(expr)

  # There should be 7 objects
  expect_length(cntxts, 7)

  # Test that we can get terms
  trms <- get_similar_terms(expr)

  # There should be 10 elements
  expect_length(trms, 10)

})
