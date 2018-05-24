# --------------------------------------------------------------------------
#
# Methods for the Collection class
#
# Standard methods are an extension of generics that already exist in R
# (e.g. print, plot etc).
#
# Specific methods are specific to sfutils (we create the generic in
# generics.R)
#
# API methods are methods that call the Cortical API / interact with Python
#
# --------------------------------------------------------------------------

# STANDARD METHODS -----

#' Turn a Collection into a list
#'
#' @param x object of class Collection
#'
#' @return List of Collection entries
#'
#' @rdname Collection-as.list
#' @docType methods
#' @export

setMethod("as.list", "Collection",
          function(x, ...) {

            x@entries

          })

#' Turn a collection of fingerprints into a sparse binary matrix
#'
#' @param x object of class 'Collection' that needs to be turned into a sparse binary matrix
#'
#' @return object of class dgCMatrix (sparse binary matrix)
#'
#' @import Matrix
#'
#' @rdname Collection-as.matrix
#' @docType methods
#' @export
#' @examples
#' \dontrun{
#' # Get data
#' data("company_descriptions")
#'
#' # Retrieve texts
#' txts <- lapply(company_descriptions, function(x) x$desc)
#'
#' # Fingerprint
#' txts_fp <- do_fingerprint_document(txts)
#'
#' # Or you can turn them into a sparse binary matrix
#' sbm <- as.matrix(txts_fp)
#'
#' # This matrix works in a similar way as normal matrices
#' sbm[1:3, 1:8]
#'
#' # You can do operations such as apply, rowSums, colSums etc.
#' dim(sbm)
#' colsums <- apply(sbm, 2, sum)
#' }

setMethod("as.matrix", "Collection",
          function(x, ...) {

            # Take entries in collection
            ent <- entries(x)

            # Get uuids
            uuids <- vapply(ent, function(x) uuid(x), "character")
            # Get fingerprints
            fps <- lapply(ent, function(x) fingerprint(x))
            # In model maatrix
            fp_positioned <- Matrix::Matrix(t(as.data.frame(fps)))

            # Positions
            positions <- 1:16384
            # TRUE/FALSE matrix
            notin.m <- Matrix::Matrix(t(apply(fp_positioned,1,function(x) positions %in% x)))
            # To binary
            notin.m <- notin.m*1
            # Row names & column names
            rownames(notin.m) <- uuids
            colnames(notin.m) <- paste0("p", positions)

            # Return
            return(notin.m)

          })

#' Length of a Collection object
#'
#' @param object Collection object
#'
#' @rdname Collection-length
#' @docType methods
#' @export

setMethod("length",
          "Collection",
          function(x) {

            base::length(entries(x))

          })

#' Indexing of a Collection object
#'
#' @param object Collection object
#'
#' @rdname Collection-doubleindex
#' @docType methods
#' @export

setMethod(
  "[[",
  "Collection",
  function(x, i, j, drop) {

    entries(x)[[i]]

  }
)

#' Indexing of a Collection object
#'
#' @param object Collection object
#'
#' @rdname Collection-singleindex
#' @docType methods
#' @export

setMethod(
  "[",
  "Collection",
  function(x, i, j, drop) {

    entries(x)[i]

  }
)

#' Show method for Collection class
#'
#' @param object object to print
#'
#' @rdname Collection-show
#' @docType methods
#' @export

setMethod("show",
          signature = "Collection",
          function(object) {

            mes <- paste0(
              is(object)[[1]], " object.\n\n",
              " no. documents: \t", length(entries(object)), "\n"
            )

            cat(mes)

          })

#' Plot method for Collection class
#'
#' @param object object to plot
#'
#' @rdname Collection-plot
#' @docType methods
#' @import ggplot2
#' @export

setMethod("plot",
          signature = "Collection",
          function(x, y) {

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

            # Fingerprint
            fill_values <- x@intensity

            # To data frame
            df <- data.frame(
              x = xax,
              y = yax,
              fill = fill_values
            )

            # Plot
            ggplot(data= df, aes(x=x, y=y)) +
              geom_tile(aes(fill = fill_values)) +
              theme_bw() +
              scale_fill_continuous(high = "#034e7b", low = "#ece7f2") +
              theme(line = element_blank(),
                    axis.ticks = element_blank(),
                    axis.text.y = element_blank(),
                    axis.text.x = element_blank(),
                    axis.title.x = element_blank(),
                    axis.title.y = element_blank(),
                    legend.position = "none") +
              ggtitle("Fingerprint plot of news intensities (darker is higher intensity)")
          })

# SPECIFIC METHODS -----
# API METHODS ----

#' @param type either one of 'Expression' or 'Term'
#'
#' @rdname get_context-methods
#'
#' @importFrom jsonlite toJSON

setMethod("get_context",
          "Collection",
          function(object, type = c("Expression", "Term")) {

            type <- match.arg(type)

            # Get any expressions from Collection
            is_expression <- vapply(entries(object),
                                    function(x) is(x)[1] == type,
                                    TRUE)

            # Get elements that are Expressions
            which_expression <- which(is_expression)

            # Subset and back to Collection
            subs <- Collection(object[which_expression])

            # Get uuids for these elements
            uuids_expression <- vapply(entries(subs),
                                       function(x) uuid(x),
                                       "character")

            # Get key
            key <- Sys.getenv("CORTICAL_API_KEY")
            server <- Sys.getenv("CORTICAL_SERVER")
            retina <- Sys.getenv("CORTICAL_RETINA")

            # Register
            conn <- retinasdk$FullClient(key,
                                         apiServer = server,
                                         retinaName = retina)

            # Dump expressions to json
            body <- lapply(entries(subs), function(x) sfexpression(x))
            body_json <- jsonlite::toJSON(body, auto_unbox = TRUE)

            # Get fingerprint
            res <- conn$getContextsForExpressions(
              body_json,
              getFingerprint = TRUE,
              maxResults = 10L
            )

            # Unlist
            res_unlist_mult <- mapply(function(x, y) {
                                             list(
                                               "uuid" = x,
                                               "contexts" = lapply(y, function(z) {
                                                 list(
                                                   "context_id" = z$context_id,
                                                   "context_label" = z$context_label,
                                                   "fingerprint" = z$fingerprint$positions
                                                 )
                                               })
                                             )
                                           },
                                      uuids_expression,
                                      res)

            # Names
            names(res_unlist_mult) <- uuids_expression

            # Return
            res_unlist_mult

          })
