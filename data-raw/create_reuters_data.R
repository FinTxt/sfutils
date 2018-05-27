# Clean wd
rm(list=ls())
# Packages
library(purrr)
library(dplyr)

# If necessary, install:
#   install.packages("tm")
#   install.packages("tm.corpus.Reuters21578", repos = "http://datacube.wu.ac.at")

library(tm.corpus.Reuters21578)
# Load data
data(Reuters21578)
reut <- Reuters21578 %>%
  as.list()
rm(Reuters21578)

# Get only topic and content
io <- map(reut, function(x) if(x$meta$topics == "YES") list(content = x$content,
                                                            tops = x$meta$topics_cat))
# Remove reut
rm(reut)
# Remove null
io <- io[unlist(map(io, function(x) !is.null(x)))]
# Get an idea of number of topics
topics <- unlist(map(io, function(x) x$tops)) %>%
  data_frame("topics" = .) %>%
  group_by(topics) %>%
  tally() %>%
  arrange(desc(n))

# I'm going to take articles about resources
in_list <- c("grain", "corn", "crude", "livestock", "wheat", "coffee", "sugar", "gold",
             "copper", "cocoa")

# Filter for these (only want articles that are clearly separated)
filter <- map(io, function(x) any(x$tops %in% in_list) & (length(x$tops) == 1))
reuters_arts <- io[unlist(filter)]

# How many of each?
topics <- unlist(map(reuters_arts, function(x) x$tops)) %>%
  data_frame("topics" = .) %>%
  group_by(topics) %>%
  tally() %>%
  arrange(desc(n))
# That's pretty good if we say crude and "other"

# Put list in data frame
reuters_final <- map_df(reuters_arts, function(x) {
  data_frame(
    "content" = x$content,
    "label" = x$tops
  )
}) %>%
  mutate("topic_crude" = ifelse(label == "crude",
                                "crude", "other"))

# Split into training and testing
set.seed(20)
split <- runif(nrow(reuters_final))
train <- reuters_final[split <= 0.8,]
test <- reuters_final[split > 0.8,]

# Fingerprint the documents
fps_train_raw <- do_fingerprint_document(lapply(train$content, function(x) x))
fps_train_raw <- entries(fps_train_raw)
fps_test_raw <- do_fingerprint_document(lapply(test$content, function(x) x))
fps_test_raw <- entries(fps_test_raw)

# Put the data into lists and add to the package
fps_train <- list(
  "fingerprints" = fps_train_raw,
  "label_binomial" = factor(train$topic_crude, levels=c("other", "crude")),
  "label_multinomial" = factor(train$label)
)
fps_test <- list(
  "fingerprints" = fps_test_raw,
  "label_binomial" = factor(test$topic_crude, levels=c("other", "crude")),
  "label_multinomial" = factor(test$label)
)

# Save data
devtools::use_data(fps_train, fps_test, overwrite = TRUE)
