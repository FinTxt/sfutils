## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(magrittr)
library(FinTxtUtils)

# Start building the expression
exp <- create_expression() %>%
  # Subtract the following
  SUB() %>%
  add_term("apple") %>%
  AND() %>%
  # The following entries are added to the "SUB" level until you call "END()"
  add_text("Mac OS is a series of graphical user interface-based operating systems developed by Apple Inc. for their Macintosh line of computer systems.") %>%
  add_term("banana") %>%
  END() %>%
  END()

knitr::kable(exp@expression)

## ------------------------------------------------------------------------
exp %>%
  build_expression()

## ---- echo=FALSE---------------------------------------------------------
# THIS IS TEMPORARY
io <- exp %>% build_expression()
io <- io[[1]]

## ------------------------------------------------------------------------
expr <- do_fingerprint_expression(io)

