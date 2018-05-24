# --------------------------------------------------------------------------
#
# Methods for the classes that are imported from other packages
#
# --------------------------------------------------------------------------

# dgCMatrix (PACKAGE: MATRIX) ----

#' @rdname do_compare-methods

setMethod("do_compare", "dgCMatrix",
          function(x, y, method=c("cosine", "jaccard", "dice",
                                  "gilbertwells", "dennis", "sorgenfrei",
                                  "lancewilliams", "euclid", "hamming")) {

            method <- match.arg(method)

            # Get fingerprint from reference
            fp_ref <- rep(0, 16384)
            fp_ref[fingerprint(y)] <- 1

            # Return metric
            sims <- switch(
              method,
              cosine = apply(x, 1, cosine_similarity_util, fp_ref),
              jaccard = apply(x, 1, jaccard_similarity_util, fp_ref),
              dice = apply(x, 1, dice_similarity_util, fp_ref),
              gilbertwells = apply(x, 1, gilbertwells_similarity_util, fp_ref),
              dennis = apply(x, 1, dennis_similarity_util, fp_ref),
              sorgenfrei = apply(x, 1, sorgenfrei_similarity_util, fp_ref),
              lancewilliams = apply(x, 1, lancewilliams_distance_util, fp_ref),
              euclid = apply(x, 1, euclid_distance_util, fp_ref),
              hamming = apply(x, 1, hamming_distance_util, fp_ref)
            )

            # To matrix
            msims <- as.matrix(sims)
            colnames(msims) <- uuid(y)

            # Return
            return(msims)

          })
