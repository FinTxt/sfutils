## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
exp <- list(
  "term" = "apple"
)

## ------------------------------------------------------------------------
exp <- list(
  # Named operator
  "sub" = list( # ... whose value is a list
    list("term" = "apple"), # ... each element has its own list.
    list("text" = "Mac OS is a series of graphical user interface-based operating systems developed by Apple Inc. for their Macintosh line of computer systems.")
  )
)

## ------------------------------------------------------------------------
exp <- list(
  # Each element is an unname list
  list(
    "term" = "apple"
  ),
  # Second element
  list(
    "sub" = list( 
      list("term" = "apple"), 
      list("text" = "Mac OS is a series of graphical user interface-based operating systems developed by Apple Inc. for their Macintosh line of computer systems.")
    )
  )
)

