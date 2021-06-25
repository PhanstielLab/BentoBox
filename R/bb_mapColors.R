## Maps a numeric or character vector to a color palette and returns
## the vector of colors
# @param vector Vector to map to color
# @param palette Color palette function
# @param range Range of values to map for a numerical value.
bb_mapColors <- function(vector, palette, range = NULL){
    
    if (is(vector, "numeric") | is(vector, "integer")){
        
        ## Update range, if necessary
        if (is.null(range)){
            breaks <- seq(min(vector), max(vector), length.out = 100)
        } else {
            vector[which(vector < range[1])] <- range[1]
            vector[which(vector > range[2])] <- range[2]
            breaks <- seq(range[1], range[2], length.out = 100)
        }
        
        ## Map numbers to colors    
        colors <- palette(length(breaks) + 1)    
        colorVector <- as.character(cut(vector, c(-Inf, breaks, Inf), 
                                        labels = colors))
        
    } else {
        
        ## Convert if not a factor
        if (!is(vector, "factor")){
            vector <- as.factor(vector)
        }
        
        ## Map color palette to factor levels
        colors <- palette(length(levels(vector)))
        names(colors) <- levels(vector)
        colorVector <- colors[vector]
        
    } 
    
    return(colorVector)
    
}

## Color palette and range default assignments for bb_mapColors
## Returns an updated object with the color palette and range
bb_colorDefaults <- function(vector, palette = NULL, range = NULL, object){
    if (is(vector, "numeric") | is(vector, "integer")){
        
        if (is.null(palette)){
            palette <- colorRampPalette(brewer.pal(
                n = 9, "YlGnBu"
            ))
        }
        
        if (is.null(range)){
            range <- c(min(vector), max(vector))
        }
        
        object$zrange <- range
        
    } else {
        
        ## Convert if not a factor
        if (!is(vector, "factor")){
            vector <- as.factor(vector)
        }
        
        if (is.null(palette)){
            palette <- colorRampPalette(suppressWarnings(
                brewer.pal(n = length(levels(vector)),
                        "Paired")))
        }
        
    }
    
    object$color_palette <- palette
    
    return(object)
}

## Define a function that will parse a vector of colors vs. a colorby object
## Returns the final vector of colors and an updated plot object
# @param data Associated data, for finding `colorby` column
# @param fill Input fill - will either be a single value, a vector, or 
# a colorby object
# @param object The plot object, to be updated with any color_palette
# and zrange information
# @param subset A string describing the type of data, which will determine
# how to subset it. Options are ranges, pairs, or manhattan.
bb_parseColors <- function(data, fill, object, subset = NULL){
    
    ## `colorby` class
    if (is(fill, "bb_colorby")){
        
        colorbyColNo <- which(colnames(data) == fill$column)
        colorbyCol <- data[, colorbyColNo]
        
        ## Scale numeric colorby data by the subsetted plotted region
        if ((is(colorbyCol, "numeric") | is(colorbyCol, "integer")) 
                & is.null(object$zrange)){
            
            if (fill$scalePerRegion == TRUE){
                if (subset == "ranges"){
                    subData <- data[which(data[,1] == object$chrom &
                                            data[,2] <= object$chromend &
                                            data[,3] >= object$chromstart),] 
                    
                } else if (subset == "pairs"){
                    subData <- data[which(data[,1] == object$chrom &
                                            data[,4] == object$chrom &
                                            data[,2] <= object$chromend &
                                            data[,6] >= object$chromstart),]
                } else if (subset == "manhattan"){
                    subData <- data[which(data[,1] == object$chrom &
                                            data[,2] >= object$chromstart &
                                            data[,2] <= object$chromend),]
                } else {
                    subData <- data
                }
                
                fill$range <- range(subData[,colorbyColNo])
            }
            
        }
        
        ## Default palette and range
        object <- bb_colorDefaults(vector = colorbyCol,
                                palette = fill$palette,
                                range = fill$range,
                                object = object)
        
        ## Pass into bb_mapColors
        colors <- bb_mapColors(vector = colorbyCol,
                            palette = object$color_palette,
                            range = object$zrange)
    } else {
        
        if (length(fill) == 1){
            colors <- as.character(rep(fill, nrow(data)))
        } else {
            colors <- as.character(rep(fill,
                        ceiling(nrow(data) / length(fill))
            )[seq(1, nrow(data))])
        }
        
    }
    
    return(list(colors, object))
    
}

## Define a function that makes a color transparent
# @param color color string
# @param alpha Alpha value of color
makeTransparent <- function(color, alpha) {
    if (is.null(alpha)) {
        alpha <- 1
    }

    rgb <- grDevices::col2rgb(color)
    transp <- rgb(rgb[1], rgb[2], rgb[3],
        alpha = alpha * 255,
        maxColorValue = 255
    )
    return(transp)
}