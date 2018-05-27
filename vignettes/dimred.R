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
chosen <- c(2, 3, 12, 
            13, 16, 20, 
            21, 28, 29, 
            31, 32, 33, 35, 
            61, 60, 62, 
            69, 119, 135, 
            141, 149, 151, 160, 
            174, 184, 192, 222, 
            232, 269, 282, 309, 
            335, 344, 348, 
            349, 359, 365, 384, 
            391, 400, 416, 
            432, 443, 445, 
            451, 464, 474, 
            493, 500)
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
#  red <- MCA(as.matrix(bmf),
#             ncp = ncol(bmf), graph = FALSE,
#             quanti.sup = 1:16384)
#  
#  hccp <- HCPC(red$eig)
#  
#  # Predict
#  pred <- predict(red, binmat)$coord
#  
#  # Take 64 dims
#  pred_filt <- pred[,1:64]
#  
#  # As distance
#  dd <- dist(pred_filt)
#  
#  # Cluster
#  clusts <- hclust(dd, method = "ward.D2")

