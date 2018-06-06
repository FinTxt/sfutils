# --------------------------------------------------------------------------
#
# Methods for the Fingerprint class
#
# Standard methods are an extension of generics that already exist in R
# (e.g. print, plot etc).
#
# Specific methods are specific to sfutils (we create the generic in
# generics.R)
#
# --------------------------------------------------------------------------

# STANDARD METHODS ----

#' Print method for Fingerprint class
#'
#' @param object object to print
#' @docType methods
#' @rdname Fingerprint-print

setMethod("show",
          signature = "Fingerprint",
          function(object) {

            mes <- paste0(
              is(object)[[1]], " object.\n\n",
              " unique id: \t", slot(object, "uuid"), "\n",
              " type: \t\t", slot(object, "type"), "\n",
              " fingerprint: \t", length(slot(object, "fingerprint")), " positions","\n"
            )

            cat(mes)

          })

#' Plot method for Fingerprint class
#'
#' @param x object of class Fingerprint to plot
#' @param y optional. object of class Fingerprint to plot
#' @param ... other options. You can pass the results of 'get_context()' or 'get_similar_terms()' to highlight them in the plot.
#'
#' @rdname Fingerprint-plot
#' @docType methods
#'
#' @import ggplot2
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stats na.omit
#'
#' @export

setMethod("plot",
          signature = "Fingerprint",
          function(x, y, ...) {

            # Opts
            opts <- list(...)
            # ILLEGAL IF MULTIPLE OPTIONS PASSED!

            # Number of positions in the retina
            positions <- 1:16384
            # Length
            position_length <- length(positions)
            # Length of x and y axis
            xlen <- sqrt(16384)
            # Space between each position
            xspace <- 4

            # Building the grid values
            ratio <- trunc(positions / xlen)
            # X positions
            xax <- positions*xspace - xspace*xlen*ratio
            # Y positions
            yax <- xspace*ratio
            # Create fill values
            fill_values <- rep(0, 16384)

            # Fingerprint
            fp <- x@fingerprint

            # Detect if y is passed
            y <- tryCatch({
              yp <- y@fingerprint
            }, error = function(e) {
              NULL
            })

            if(!is.null(y)) {
              #browser()
              overlap <- length(intersect(fp, y))
              # Where value present, add to data
              fill_values[fp] <- fill_values[fp] + 1
              fill_values[yp] <- fill_values[yp] + 1
              # Print overlap message
              msg <- paste0(
                "Fingerprints share ", round((sum(fill_values == 2))/(sum(fill_values != 0)),
                                            digits = 4) * 100,
                "% overlap\n\n",
                "Overlapping left: ", round(overlap / length(fp), digits = 4) * 100, "%\n",
                "Overlapping right: ", round(overlap / length(y), digits = 4) * 100, "%"
              )
              cat(msg)
            } else {
              # Where value present, add to data
              fill_values[fp] <- 1
            }

            # If contexts present
            if("contexts" %in% names(opts)) {

              contexts <- opts$contexts
              cntext <- rep(NA, 16384)
              # For each context, do
              for(context in contexts) {
                cntext[context$fingerprint] <- context$context_label
              }

              # To data frame
              df <- data.frame(
                x = xax,
                y = yax,
                fill = fill_values,
                context = cntext,
                stringsAsFactors = FALSE
              )
              # Add factor with context
              df$context <- factor(
                    ifelse(is.na(df$context) & df$fill == 1,
                           "other",
                           ifelse(!is.na(df$context),
                                  df$context,
                                  NA)
                    ),
                    levels = c(as.character(na.omit(unique(names(contexts)))), "other")
                  )

              # Color palette
              col.pal <- RColorBrewer::brewer.pal(length(levels(df$context)),
                                                  "Set3")

              # Plot
              ggplot(data= df, aes(x=x, y=y)) +
                geom_tile(aes(fill = context), alpha = 0.9) +
                theme_bw() +
                scale_fill_manual(name = "Context",
                                  na.value = "white",
                                  values = col.pal) +
                theme(line = element_blank(),
                      axis.ticks = element_blank(),
                      axis.text.y = element_blank(),
                      axis.text.x = element_blank(),
                      axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      legend.position = "right") +
                ggtitle("Fingerprint plot")

            } else if("terms" %in% names(opts)) {

              trms <- opts$terms
              terms <- rep(NA, 16384)
              # For each context, do
              for(trm in trms) {
                terms[trm$fingerprint] <- trm$term
              }

              # To data frame
              df <- data.frame(
                x = xax,
                y = yax,
                fill = fill_values,
                terms = terms,
                stringsAsFactors = FALSE
              )
              # Add factor with terms
              df$terms <- factor(
                ifelse(is.na(df$terms) & df$fill == 1,
                       "other",
                       ifelse(!is.na(df$terms),
                              df$terms,
                              NA)
                ),
                levels = c(as.character(na.omit(unique(names(trms)))), "other")
              )

              # Color palette
              col.pal <- RColorBrewer::brewer.pal(length(levels(df$terms)),
                                                  "Set3")

              # Plot
              ggplot(data= df, aes(x=x, y=y)) +
                geom_tile(aes(fill = terms), alpha = 0.9) +
                theme_bw() +
                scale_fill_manual(name = "Terms",
                                  na.value = "white",
                                  values = col.pal) +
                theme(line = element_blank(),
                      axis.ticks = element_blank(),
                      axis.text.y = element_blank(),
                      axis.text.x = element_blank(),
                      axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      legend.position = "right") +
                      ggtitle("Fingerprint plot")

            } else {

              # To data frame
              df <- data.frame(
                x = xax,
                y = yax,
                fill = fill_values
              )

              # Palette
              if(length(unique(df$fill)) == 2) {
                col.pal <- c("#ebfafa","red")
              } else {
                col.pal <- c("#ebfafa","red", "black")
              }

              # Plot
              ggplot(data= df, aes(x=x, y=y)) +
                geom_tile(aes(fill = factor(fill_values), alpha = 0.9)) +
                theme_bw() +
                scale_fill_manual(values=col.pal) +
                theme(line = element_blank(),
                      axis.ticks = element_blank(),
                      axis.text.y = element_blank(),
                      axis.text.x = element_blank(),
                      axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      legend.position = "none") +
                ggtitle("Fingerprint plot")

            }
          })


# SPECIFIC METHODS -----

#' @rdname do_compare-methods

setMethod("do_compare",
          "Fingerprint",
          function(x, y, method=c("cosine", "jaccard", "dice",
                                  "gilbertwells", "dennis", "sorgenfrei",
                                  "lancewilliams", "euclid", "hamming", "other"),
                   ...) {

            method <- match.arg(method)

            # If method == 'other', check optional arguments
            if(method == "other") {

              opts <- list(...)

              if("func" != names(opts)) {

                stop("You specified 'other' as method but did not supply a function. Pass your own similarity/distance
                     function using the parameter 'func = <YOURFUNCTION>'")

              }

              otherFunc <- opts$func

            }

            # Create positions
            pos1 <- rep(0, 16384)
            pos2 <- rep(0, 16384)

            # Add 1s to existing positions
            pos1[fingerprint(x)] <- 1
            pos2[fingerprint(y)] <- 1

            # Return metric
            switch(
              method,
              cosine = cosine_similarity_util(pos1, pos2),
              jaccard = jaccard_similarity_util(pos1, pos2),
              dice = dice_similarity_util(pos1, pos2),
              gilbertwells = gilbertwells_similarity_util(pos1,pos2),
              dennis = dennis_similarity_util(pos1,pos2),
              sorgenfrei = sorgenfrei_similarity_util(pos1, pos2),
              lancewilliams = lancewilliams_distance_util(pos1, pos2),
              euclid = euclid_distance_util(pos1, pos2),
              hamming = hamming_distance_util(pos1, pos2),
              other = otherFunc(pos1, pos2)
            )

          })

