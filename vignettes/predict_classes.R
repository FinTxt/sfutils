## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=TRUE, echo=FALSE----------------------------------------------
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(FinTxtUtils))
suppressPackageStartupMessages(library(ggplot2))

## ---- message=FALSE, eval=TRUE-------------------------------------------
# Clean wd
rm(list=ls())
# Load data
data("fps_train")
data("fps_test")

# Unpack
train_lbls_b <- fps_train$label_binomial
train_lbls_m <- fps_train$label_multinomial
fps_train <- fps_train$fingerprints

test_lbls_b <- fps_test$label_binomial
test_lbls_m <- fps_test$label_multinomial
fps_test <- fps_test$fingerprints

## ------------------------------------------------------------------------
# Select one fingerprinted document
one <- fps_train[[1]]
print(one)

## ----fig.height=6, fig.width=6-------------------------------------------
# Plot
plot(one)

## ------------------------------------------------------------------------
# Turn fingerprints into Collection
col_train <- Collection(fps_train)
col_test <- Collection(fps_test)

## ----fig.height=6, fig.width=6-------------------------------------------
# Plot intensity
plot(col_train) # Article fingerprints are very similar. This is not surprising given that we picked out articles about natural resources

## ---- fig.show="hold", fig.width=3, fig.height=3-------------------------
cats <- levels(train_lbls_m)
# For each, make a collection and plot
catplots <- purrr::map(cats, function(x) {
  index <- which(train_lbls_m == x)
  col <- Collection(fps_train[index])
  plot(col) + 
    ggtitle(x)
})

for(catplot in catplots) {
  plot(catplot)
}


## ------------------------------------------------------------------------
# Turn test and train into a sparse binary matrix
mtrain <- as.matrix(col_train)
mtest <- as.matrix(col_test)

mtrain[1:5, 1:8]

## ------------------------------------------------------------------------
# Remove columns whose sums are 0
all_zero <- which(colSums(mtrain) == 0)

# Replace
mtrain_p <- mtrain[, -all_zero]
mtest_p <- mtest[, -all_zero]

# Perform multiple correspondence analysis on sparse binary train data
library(FactoMineR)

# Multiple Correspondence Analysis on train
# We have to coerce the sparse binary matrix to a
# regular matrix
train_pca <- PCA(as.matrix(mtrain_p), ncp = ncol(mtrain_p), graph = FALSE)

# Predict on test 
test_pca <- predict(train_pca, as.matrix(mtest_p))$coord

# Plot PCA (variance explained)
plotPCA <- function(eigenValue.df) {
  ggplot(data.frame(x=1:nrow(eigenValue.df),
                    y=eigenValue.df[,3]), aes(x=x, y=y)) +
    geom_line(size = 1.2) +
    theme_bw() +
    scale_x_continuous(name = "Number of principal components") +
    scale_y_continuous(name = "Cumulative percentage of variance")
}
# Plot
plotPCA(train_pca$eig)


## ------------------------------------------------------------------------
# Number of princomp
k = 10

# Train glmnet model (binomial)
suppressPackageStartupMessages(library(glmnet))
# Train model
mod_train_binom <- cv.glmnet(x=data.matrix(train_pca$ind$coord[,1:k]),
                              y=train_lbls_b, family = "binomial")

# Predict
pred_class_b <- factor(
  predict(mod_train_binom, train_pca$ind$coord[,1:k], type="class")
, levels = c("other", "crude")
)

# Confusion matrix
caret::confusionMatrix(pred_class_b, train_lbls_b, positive = "crude")

## ------------------------------------------------------------------------
k = 10
library(ggplot2)
plot_df <- data_frame(
  "prob" = predict(mod_train_binom, train_pca$ind$coord[,1:k], type="response")[,1],
  "class" = train_lbls_b,
  "class_pred" = pred_class_b
)

# Plot double-density plot for binomial case
ggplot(plot_df, aes(x= prob, group = class, linetype=class)) +
  geom_density() +
  theme_bw()

## ------------------------------------------------------------------------
# Number of components
k <- 100

# Model
mod_train_multi <- cv.glmnet(x=data.matrix(train_pca$ind$coord[,1:k]),
                              y=train_lbls_m, family = "multinomial")

# Predict
pred_class_m <- factor(
  predict(mod_train_multi, train_pca$ind$coord[,1:k], type="class"),
  levels = levels(train_lbls_m)
)

# Confusion matrix
caret::confusionMatrix(pred_class_m, train_lbls_m)

## ------------------------------------------------------------------------
# Number of components
k <- 100

# Predict
pred_class_m_t <- factor(
  predict(mod_train_multi, test_pca[,1:k], type="class"),
  levels = levels(test_lbls_m)
)

# Confusion matrix
caret::confusionMatrix(pred_class_m_t, test_lbls_m)

