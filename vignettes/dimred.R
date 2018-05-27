## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
suppressPackageStartupMessages(library(sfutils))
# Load data
data("SAP500")

## ------------------------------------------------------------------------
# Choose these companies
chosen <- c(1:3, 12:13,
            16, 21, 28:29, 
            33, 35, 60:62, 
            69, 80, 119, 121,
            149, 160, 168, 174,
            183, 186, 193, 205,
            219, 229, 235, 262,
            266, 278, 279, 338,
            339, 342, 343, 353, 
            355, 359, 362, 373,
            378, 387, 409, 435, 
            436, 453, 468, 487) # 366
# Subset
subs <- SAP500[chosen]
names(subs)

## ---- eval=FALSE---------------------------------------------------------
#  # Retrieve only descriptions
#  desc_only <- lapply(subs, function(x) x$desc)
#  
#  # Fingerprint
#  fps <- do_fingerprint_document(desc_only)
#  
#  # To matrix
#  binmat <- as.matrix(fps)
#  rownames(binmat) <- names(desc_only)
#  
#  # Distances (need to convert matrix from Matrix package to normal matrix)
#  fpsdst <- dist(as.matrix(binmat))
#  
#  # Do hierarchical clustering and plot
#  clust <- hclust(fpsdst, method = "ward.D2")

## ---- eval = FALSE-------------------------------------------------------
#  # Reduce dimensions
#  library(FactoMineR)
#  red <- PCA(as.matrix(binmat),
#             ncp = ncol(binmat), graph = FALSE)
#  
#  # Predict
#  pred <- predict(red, binmat)$coord
#  
#  # Take 64 dims
#  pred_filt <- pred[,1:30]
#  
#  # As distance
#  dd <- dist(pred_filt)
#  
#  # Cluster
#  clusts <- hclust(dd, method = "ward.D2")

