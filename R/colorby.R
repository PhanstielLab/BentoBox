#' Handle BentoBox color scaling parameters
#'
#' \code{colorby} should be used to create a set of parameters
#' that specify color scaling for the functions \code{bb_plotPairs},
#' \code{bb_plotPairsArches}, and \code{bb_plotRanges}.
#'
#' @param column String specifying name of data column to scale colors by.
#' @param range A numeric vector specifying the range of values to
#' apply a color scale to.
#'
#' @return Returns a "\code{bb_colorby}" object.
#'
#' @examples
#' ## Load paired ranges data in BEDPE format
#' data("bb_bedpeData")
#'
#' ## Add a length column
#' bb_bedpeData$length <- (bb_bedpeData$start2 - bb_bedpeData$start1) / 1000
#'
#' ## Plot pairs with colorby object set for `length` column
#' bedpePlot <- bb_plotPairs(
#'     data = bb_bedpeData,
#'     chrom = "chr21",
#'     chromstart = 27900000, chromend = 30700000,
#'     fill = colorRampPalette(c("dodgerblue2", "firebrick2")),
#'     colorby = colorby("length"),
#'     lwd = 2, spaceHeight = .7,
#' )
#' @export
colorby <- function(column, range = NULL) {
    colorbyObject <- structure(list(column = column, range = range),
        class = "bb_colorby"
    )
    return(colorbyObject)
}
