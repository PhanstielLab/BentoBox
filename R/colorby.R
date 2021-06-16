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
#' library(BentoBoxData)
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
#'     assembly = "hg19",
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

## The following are colorby() related functions
# colorbyCol: get the colorbycol and convert as necessary
# mapColorbyCol: map colors to a colorby column (with no colorby,
# colors will go in order of visualized data rows)

colorbyCol <- function(data, object, objectInternal){
    
    if (!is.null(objectInternal$colorby) & nrow(data) > 0){
        colorbyCol <- which(colnames(data) == objectInternal$colorby$column)
        colorbyCol <- data[, colorbyCol]
        
        if (!is(colorbyCol, "numeric") & !is(colorbyCol, "integer")) {
            colorbyCol <- as.numeric(factor(colorbyCol))
        }
        data$colorby <- colorbyCol
        
        if (is.null(objectInternal$colorby$range)) {
            colorbyrange <- c(min(data$colorby), max(data$colorby))
            object$zrange <- colorbyrange
        }
        
    } else {
        data$colorby <- rep(NA, nrow(data))
    }
    
    return(list(data, object))
}
mapColorbyCol <- function(data, object, objectInternal, nrow, rows = TRUE){
    
    if (!is.null(objectInternal$colorby)){
        if (is(objectInternal$fill, "function")) {
            colors <- bb_maptocolors(data$colorby,
                                                objectInternal$fill,
                                                range = object$zrange
            )
            object$color_palette <- objectInternal$fill
        } else {
            colorbyCol <- factor(data$colorby)
            mappedColors <- rep(
                objectInternal$fill,
                ceiling(length(levels(colorbyCol)) /
                            length(objectInternal$fill))
            )
            names(mappedColors) <- levels(colorbyCol)
            colors <- mappedColors[colorbyCol]
        }
        
    } else {
        if (is(objectInternal$fill, "function")) {
            colors <- objectInternal$fill(nrow)
            if (rows == TRUE){
                indeces <- data$row
                colors <- colors[indeces]
            }
            
        } else {
            if (length(objectInternal$fill) == 1) {
                colors <- rep(objectInternal$fill, nrow(data))
            } else {
                colors <- rep(
                    objectInternal$fill,
                    ceiling(nrow / length(
                        objectInternal$fill
                    ))
                )[seq(1, nrow)]
                
                if (rows == TRUE){
                    indeces <- data$row
                    colors <- colors[indeces]
                }
                
            }
        }
        
    }
    data$color <- colors
    return(list(data, object))
    
}


