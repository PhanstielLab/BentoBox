## Define a function to turn gTree child into a gPath
# @param grob grob to convert to gpath
convert_gpath <- function(grob) {

    ## Get the name of the grob
    name <- grob$name

    ## Turn it into a gPath
    gpath <- gPath(name)

    return(gpath)
}

## Define a function to make sure a bb_page viewport exists
# @param error Error message if bb_page doesn't exist
check_bbpage <- function(error) {
    if (!"bb_page" %in% current.vpPath()) {
        stop(error, call. = FALSE)
    }
}

## Define a function to check dimensions/placing coordinates
# @param object Plot object with dimensions/coordinates/plotted attribute
check_placement <- function(object) {
    if (attributes(object)$plotted == TRUE) {

        ## If giving placement coordinates
        if (!is.null(object$x) | !is.null(object[["y"]])) {

            ## 1. Need both an x and y coordinate
            if (!is.null(object$x) & is.null(object[["y"]])) {
                stop("Placement detected with y value missing.", call. = FALSE)
            }

            if (!is.null(object[["y"]]) & is.null(object$x)) {
                stop("Placement detected with x value missing.", call. = FALSE)
            }

            ## 2. Need plot dimensions
            if (is.null(object$width)) {
                stop("Placement detected with plot width missing.",
                    call. = FALSE
                )
            }

            if (as.numeric(object$width) == 0) {
                stop("Plot width cannot be 0.", call. = FALSE)
            }

            if (is.null(object$height)) {
                stop("Placement detected with plot height missing.",
                    call. = FALSE
                )
            }

            if (as.numeric(object$height) == 0) {
                stop("Plot height cannot be 0.", call. = FALSE)
            }


            ## 3. Need a bb_page
            check_bbpage(error = "Must make a BentoBox page with
                        `bb_pageCreate()` before placing a plot.")
        }
    }
}

## Define a character vector of valid coordinate systems to work in
validUnits <- c(
    "npc", "native", "inches", "cm", "mm", "points",
    "bigpts", "picas", "dida",
    "cicero", "scaledpts", "char", "lines", "snpc"
)

## Define a function to assign rows for pileup-style data
assignRows <- function(data, maxRows, wiggle, rowCol, side = "top",
                       gTree, extraData = NULL){
    
    if (nrow(data) > 0){
        ## Initialize a row column
        data$row <- 0
        
        ## Convert to numeric matrix for Rcpp function parsing
        dataMatrix <- as.matrix(data)
        
        ## Assign a row for each element
        rowData <- as.data.frame(checkRow(dataMatrix, maxRows, rowCol, wiggle))
        colnames(rowData) <- colnames(data)
        
        ## Combine with extra data columns after row assignment
        rowData <- cbind(rowData, extraData)
        
        ## Remove and warn if any data does not get assigned a row
        if (any(rowData$row == 0)){
            rowData <- rowData[which(rowData$row != 0), ]
            warning("Not enough plotting space for all provided elements.",
                    call. = FALSE)
            
            if (side == "top"){
                y <- unit(1, "npc")
                just <- c("right", "top")
            } else{
                y <- unit(0, "npc")
                just <- c("right", "bottom")
            }
            
            limitGrob <- textGrob(
                label = "+", x = unit(1, "npc"),
                y = y,
                just = just,
                gp = gpar(col = "grey", fontsize = 6)
            )
            assign(gTree,
                   addGrob(
                       gTree = get(gTree, envir = bbEnv),
                       child = limitGrob
                   ),
                   envir = bbEnv
            )
        }
        
        ## Change row index to 0 to calculate y
        rowData$row <- rowData$row - 1
    } else {
        rowData <- data.frame()
    }
    
    return(rowData)
}
