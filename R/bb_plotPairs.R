#' Plot paired-end genomic range elements
#'
#' @param data A string specifying the BEDPE file path, a dataframe
#' in BEDPE format specifying data to be plotted, or a
#' \link[InteractionSet]{GInteractions} object.
#' @param chrom Chromosome of region to be plotted, as a string.
#' @param chromstart Integer start position on chromosome to be plotted.
#' @param chromend Integer end position on chromosome to be plotted.
#' @param assembly Default genome assembly as a string or a
#' \link[BentoBox]{bb_assembly} object.
#' Default value is \code{assembly = "hg38"}.
#' @param fill Character value(s) as a single value, vector, or palette
#' specifying fill colors of BEDPE elements.
#' Default value is \code{fill = "#1f4297"}.
#' @param colorby A "\link[BentoBox]{colorby}" object specifying
#' information for scaling colors in data.
#' @param linecolor A character value specifying the color of the
#' lines outlining paired range elements.
#' Default value is \code{linecolor = NA}.
#' @param bg Character value indicating background color.
#' Default value is \code{bg = NA}.
#' @param boxHeight A numeric or unit object specifying height of boxes
#' at either end of paired range elements.
#' Default value is \code{boxHeight = unit(2, "mm")}.
#' @param spaceWidth A numeric specifying the width of spacing between
#' paired range elements, as a fraction of the plot's genomic range.
#' Default value is \code{spaceWidth = 0.02}.
#' @param spaceHeight A numeric specifying the height of space between
#' boxes of paired range elements on different rows.
#' Default value is \code{spaceHeight = 0.3}.
#' @param baseline Logical value indicating whether to include a baseline
#' along the x-axis. Default value is \code{baseline = FALSE}.
#' @param baseline.color Baseline color.
#' Default value is \code{baseline.color = "grey"}.
#' @param baseline.lwd Baseline line width.
#' Default value is \code{baseline.lwd = 1}.
#' @param x A numeric or unit object specifying paired range plot x-location.
#' @param y A numeric, unit object, or character containing a "b"
#' combined with a numeric value specifying paired range plot y-location.
#' The character value will
#' place the paired range plot y relative to the bottom of the most recently
#' plotted BentoBox plot according to the units of the BentoBox page.
#' @param width A numeric or unit object specifying paired range plot width.
#' @param height A numeric or unit object specifying paired range plot height.
#' @param just Justification of paired range plot relative
#' to its (x, y) location.
#' If there are two values, the first value specifies horizontal
#' justification and the second value specifies vertical justification.
#' Possible string values are: \code{"left"}, \code{"right"},
#' \code{"centre"}, \code{"center"}, \code{"bottom"}, and \code{"top"}.
#' Default value is \code{just = c("left", "top")}.
#' @param default.units A string indicating the default units to use
#' if \code{x}, \code{y}, \code{width}, or \code{height} are only given
#' as numerics. Default value is \code{default.units = "inches"}.
#' @param draw A logical value indicating whether graphics
#' output should be produced.
#' @param params An optional \link[BentoBox]{bb_params} object
#' containing relevant function parameters.
#' @param ... Additional grid graphical parameters. See \link[grid]{gpar}.
#'
#' @return Returns a \code{bb_pairs} object containing relevant
#' genomic region, placement, and \link[grid]{grob} information.
#'
#' @examples
#' ## Load paired ranges data in BEDPE format
#' library(BentoBoxData)
#' data("bb_bedpeData")
#'
#' ## Set the coordinates
#' params <- bb_params(
#'     chrom = "chr21",
#'     chromstart = 27900000, chromend = 30700000,
#'     assembly = "hg19",
#'     width = 7
#' )
#'
#' ## Create a page
#' bb_pageCreate(width = 7.5, height = 2.1, default.units = "inches")
#'
#' ## Add a length column
#' bb_bedpeData$length <- (bb_bedpeData$start2 - bb_bedpeData$start1) / 1000
#'
#' ## Plot the data
#' bedpePlot <- bb_plotPairs(
#'     data = bb_bedpeData, params = params,
#'     fill = colorRampPalette(c("dodgerblue2", "firebrick2")),
#'     colorby = colorby("length"),
#'     lwd = 2, spaceHeight = .7,
#'     x = 0.25, y = 0.25, height = 1.5,
#'     just = c("left", "top"), default.units = "inches"
#' )
#'
#' ## Annotate genome label
#' bb_annoGenomeLabel(plot = bedpePlot, x = 0.25, y = 1.78, scale = "Mb")
#'
#' ## Add heatmap legend
#' bb_annoHeatmapLegend(
#'     plot = bedpePlot, fontcolor = "black",
#'     x = 7.0, y = 0.25,
#'     width = 0.10, height = 1, fontsize = 10
#' )
#'
#' ## Add heatmap legend label
#' bb_plotText(
#'     label = "Kb", rot = 90, x = 6.9, y = 0.75,
#'     just = c("center", "center"), fontsize = 10
#' )
#'
#' ## Hide page guides
#' bb_pageGuideHide()
#' @details
#' #' A paired ranges plot can be placed on a BentoBox coordinate page
#' by providing plot placement parameters:
#' \preformatted{
#' bb_plotPairs(data, chrom,
#'              chromstart = NULL, chromend = NULL,
#'              x, y, width, height, just = c("left", "top"),
#'              default.units = "inches")
#' }
#' This function can also be used to quickly plot an unannotated paired
#' ranges plot by ignoring plot placement parameters:
#' \preformatted{
#' bb_plotPairs(data, chrom,
#'              chromstart = NULL, chromend = NULL)
#' }
#'
#' @export
bb_plotPairs <- function(data, chrom, chromstart = NULL, chromend = NULL,
                        assembly = "hg38", fill = "#1f4297", colorby = NULL,
                        linecolor = NA, bg = NA, boxHeight = unit(2, "mm"),
                        spaceWidth = 0.02, spaceHeight = 0.3,
                        baseline = FALSE, baseline.color = "grey",
                        baseline.lwd = 1,
                        x = NULL, y = NULL, width = NULL, height = NULL,
                        just = c("left", "top"), default.units = "inches",
                        draw = TRUE, params = NULL, ...) {

    # =========================================================================
    # FUNCTIONS
    # =========================================================================

    ## Define a function that catches errors
    errorcheck_bb_plotPairs <- function(bedpe, bedpe_plot, colorby) {

        ## Can't have only one NULL chromstart or chromend
        if ((is.null(bedpe_plot$chromstart) & !is.null(bedpe_plot$chromend)) |
            (is.null(bedpe_plot$chromend) & !is.null(bedpe_plot$chromstart))) {
            stop("Cannot have one \'NULL\' \'chromstart\' or \'chromend\'.",
                call. = FALSE
            )
        }

        if (!is.null(bedpe_plot$chromstart) & !is.null(bedpe_plot$chromend)) {
            if (bedpe_plot$chromstart == bedpe_plot$chromend) {
                stop("Genomic region is 0 bp long.", call. = FALSE)
            }

            ## chromend > chromstart
            if (bedpe_plot$chromend < bedpe_plot$chromstart) {
                stop("\'chromstart\' should not be larger than \'chromend\'.",
                    call. = FALSE
                )
            }
        }

        if (!is.null(colorby)) {
            if (!any(colnames(bedpe) == colorby$column)) {
                stop("Colorby column not found in data. ",
                    "Check colorby column name.",
                    call. = FALSE
                )
            }

            if (length(which(colnames(bedpe) == colorby$column)) > 1) {
                stop("Multiple matching colorby columns found in data. ",
                    "Please provide colorby column name with only ",
                    "one occurrence.", call. = FALSE)
            }
        }
    }

    # =========================================================================
    # PARSE PARAMETERS
    # =========================================================================

    bb_bedpeInternal <- parseParams(
        params = params,
        defaultArgs = formals(eval(match.call()[[1]])),
        declaredArgs = lapply(match.call()[-1], eval),
        class = "bb_bedpeInternal"
    )

    ## Parse gp
    bb_bedpeInternal$gp <- setGP(
        gpList = gpar(),
        params = bb_bedpeInternal, ...
    )

    # =========================================================================
    # CHECK ARGUMENT ERRORS
    # =========================================================================
    if (is.null(bb_bedpeInternal$data)) stop("argument \"data\" is missing, ",
                                            "with no default.", call. = FALSE)
    if (is.null(bb_bedpeInternal$chrom)) stop("argument \"chrom\" is missing, ",
                                            "with no default.", call. = FALSE)

    if (!is.null(bb_bedpeInternal$colorby)) {
        if (class(bb_bedpeInternal$colorby) != "bb_colorby") {
            stop("\"colorby\" not of class \"bb_colorby\". Input colorby ",
                "information with \"colorby()\".", call. = FALSE)
        }
    }
    # =========================================================================
    # INITIALIZE OBJECT
    # =========================================================================

    bb_bedpe <- structure(list(
        bedpe = NULL, chrom = bb_bedpeInternal$chrom,
        chromstart = bb_bedpeInternal$chromstart,
        chromend = bb_bedpeInternal$chromend,
        assembly = bb_bedpeInternal$assembly,
        color_palette = NULL,
        zrange = bb_bedpeInternal$colorby$range,
        x = bb_bedpeInternal$x, y = bb_bedpeInternal$y,
        width = bb_bedpeInternal$width,
        height = bb_bedpeInternal$height,
        just = bb_bedpeInternal$just, grobs = NULL
    ),
    class = "bb_pairs"
    )
    attr(x = bb_bedpe, which = "plotted") <- bb_bedpeInternal$draw

    # =========================================================================
    # CHECK PLACEMENT
    # =========================================================================

    check_placement(object = bb_bedpe)

    # =========================================================================
    # PARSE ASSEMBLY
    # =========================================================================

    bb_bedpe$assembly <- parse_bbAssembly(assembly = bb_bedpe$assembly)

    # =========================================================================
    # PARSE UNITS
    # =========================================================================

    bb_bedpe <- defaultUnits(
        object = bb_bedpe,
        default.units = bb_bedpeInternal$default.units
    )
    if (!"unit" %in% class(bb_bedpeInternal$boxHeight)) {
        if (!is.numeric(bb_bedpeInternal$boxHeight)) {
            stop("\'boxHeight\' is neither a unit object or a ",
                "numeric value. Cannot make paired ranges plot.",
                call. = FALSE
            )
        }

        if (is.null(bb_bedpeInternal$default.units)) {
            stop("\'boxHeight\' detected as numeric.\'default.units\' ",
                "must be specified.", call. = FALSE)
        }

        bb_bedpeInternal$boxHeight <- unit(
            bb_bedpeInternal$boxHeight,
            bb_bedpeInternal$default.units
        )
    }

    # =========================================================================
    # READ IN FILE OR DATAFRAME
    # =========================================================================

    bedpe <- read_pairedData(data = bb_bedpeInternal$data,
                            assembly = bb_bedpe$assembly)
    
    # =========================================================================
    # CATCH ERRORS
    # =========================================================================

    errorcheck_bb_plotPairs(
        bedpe = bedpe, bedpe_plot = bb_bedpe,
        colorby = bb_bedpeInternal$colorby
    )

    # =========================================================================
    # ORGANIZE DATA
    # =========================================================================

    ## Get appropriate starts/stops
    start1 <- apply(bedpe[, c(2, 3)], 1, min)
    stop1 <- apply(bedpe[, c(2, 3)], 1, max)
    start2 <- apply(bedpe[, c(5, 6)], 1, min)
    stop2 <- apply(bedpe[, c(5, 6)], 1, max)
    bedpe[, 2] <- start1
    bedpe[, 3] <- stop1
    bedpe[, 5] <- start2
    bedpe[, 6] <- stop2

    # =========================================================================
    # WHOLE CHROMOSOME DATA AND XSCALE
    # =========================================================================

    if (is.null(bb_bedpe$chromstart) & is.null(bb_bedpe$chromend)) {
        if (class(bb_bedpe$assembly$TxDb) == "TxDb") {
            txdbChecks <- TRUE
        } else {
            txdbChecks <- check_loadedPackage(
                package = bb_bedpe$assembly$TxDb,
                message = paste(
                    paste0("`", bb_bedpe$assembly$TxDb, "`"),
                    "not loaded. Please install and load to plot
                full chromosome paired data."
                )
            )
        }

        xscale <- c(0, 1)
        if (txdbChecks == TRUE) {
            if (class(bb_bedpe$assembly$TxDb) == "TxDb") {
                tx_db <- bb_bedpe$assembly$TxDb
            } else {
                tx_db <- eval(parse(text = bb_bedpe$assembly$TxDb))
            }

            assembly_data <- GenomeInfoDb::seqlengths(tx_db)

            if (!bb_bedpe$chrom %in% names(assembly_data)) {
                txdbChecks <- FALSE
                warning("Chromosome",
                    "'", bb_bedpe$chrom, "'",
                    "not found in",
                    "`", bb_bedpe$assembly$TxDb$packageName, "`",
                    "and data for entire chromosome cannot be plotted.",
                    call. = FALSE
                )
            } else {
                bb_bedpe$chromstart <- 1
                bb_bedpe$chromend <- assembly_data[[bb_bedpe$chrom]]
                xscale <- c(bb_bedpe$chromstart, bb_bedpe$chromend)
            }
        }
    } else {
        txdbChecks <- TRUE
        xscale <- c(bb_bedpe$chromstart, bb_bedpe$chromend)
    }

    # =========================================================================
    # SUBSET DATA FOR CHROMOSOME AND ANY OVERLAPPING REGIONS
    # =========================================================================

    if (!is.null(bb_bedpe$chromstart) & !is.null(bb_bedpe$chromend)) {
        bedpe <- bedpe[which(bedpe[, 1] == bb_bedpe$chrom &
            bedpe[, 4] == bb_bedpe$chrom &
            bedpe[, 2] <= bb_bedpe$chromend &
            bedpe[, 6] >= bb_bedpe$chromstart), ]
    } else {
        bedpe <- data.frame(matrix(nrow = 0, ncol = 6))
    }

    bb_bedpe$bedpe <- bedpe
    # =========================================================================
    # GET BOX WIDTHS AND TOTAL DISTANCES
    # =========================================================================

    bedpe$width1 <- bedpe[, 3] - bedpe[, 2]
    bedpe$width2 <- bedpe[, 6] - bedpe[, 5]
    bedpe$pos1 <- rowMeans(bedpe[, c(2, 3)])
    bedpe$pos2 <- rowMeans(bedpe[, c(5, 6)])
    bedpe$distance <- abs(bedpe$pos2 - bedpe$pos1)

    # =========================================================================
    # SORT BY DISTANCE FOR PRETTIER PLOTTING
    # =========================================================================

    bedpe <- bedpe[order(bedpe$distance, decreasing = TRUE), ]

    # =========================================================================
    # SET COLORBY DATA
    # =========================================================================
    if (!is.null(bb_bedpeInternal$colorby) & nrow(bedpe) > 0) {
        colorbyCol <- which(colnames(bedpe) == bb_bedpeInternal$colorby$column)
        colorbyCol <- bedpe[, colorbyCol]

        if (class(colorbyCol) != "numeric" & class(colorbyCol) != "integer") {
            colorbyCol <- factor(colorbyCol)
            bedpe$colorby <- as.numeric(colorbyCol)
        } else {
            bedpe$colorby <- colorbyCol
        }

        if (is.null(bb_bedpeInternal$colorby$range)) {
            colorbyrange <- c(min(bedpe$colorby), max(bedpe$colorby))
            bb_bedpe$zrange <- colorbyrange
        }
    } else {
        bedpe$colorby <- rep(NA, nrow(bedpe))
    }

    # =========================================================================
    # VIEWPORTS
    # =========================================================================

    ## Get viewport name
    currentViewports <- current_viewports()
    vp_name <- paste0(
        "bb_pairs",
        length(grep(
            pattern = "bb_pairs",
            x = currentViewports
        )) + 1
    )

    ## If placing information is provided but plot == TRUE,
    ## set up it's own viewport separate from bb_makepage
    ## Not translating into page_coordinates
    if (is.null(bb_bedpe$x) | is.null(bb_bedpe$y)) {
        vp <- viewport(
            height = unit(0.5, "snpc"), width = unit(1, "snpc"),
            x = unit(0.5, "npc"), y = unit(0.5, "npc"),
            clip = "on",
            xscale = xscale,
            yscale = c(0, 1),
            just = "center",
            name = vp_name
        )

        if (bb_bedpeInternal$draw == TRUE) {
            vp$name <- "bb_pairs1"
            grid.newpage()
        }
    } else {
        add_bbViewport(vp_name)

        ## Convert coordinates into same units as page
        page_coords <- convert_page(object = bb_bedpe)

        ## Make viewport
        vp <- viewport(
            height = page_coords$height, width = page_coords$width,
            x = page_coords$x, y = page_coords$y,
            clip = "on",
            xscale = xscale,
            yscale = c(0, convertHeight(page_coords$height,
                unitTo = get("page_units",
                    envir = bbEnv
                ),
                valueOnly = TRUE
            )),
            just = bb_bedpeInternal$just,
            name = vp_name
        )
    }

    # =========================================================================
    # INITIALIZE GTREE FOR GROBS WITH BACKGROUND
    # =========================================================================

    backgroundGrob <- rectGrob(gp = gpar(
        fill = bb_bedpeInternal$bg,
        col = NA
    ), name = "background")
    assign("bedpe_grobs", gTree(
        vp = vp,
        children = gList(backgroundGrob)
    ),
    envir = bbEnv
    )

    # =========================================================================
    # DETERMINE ROWS FOR EACH ELEMENT
    # =========================================================================

    ## Determine how many bepe elements are going to fit based on
    ## boxHeight and space
    if (is.null(bb_bedpe$x) & is.null(bb_bedpe$y)) {
        pushViewport(vp)
        boxHeight <- convertHeight(bb_bedpeInternal$boxHeight,
            unitTo = "npc", valueOnly = TRUE
        )
        spaceHeight <- boxHeight * (bb_bedpeInternal$spaceHeight)
        upViewport()
    } else {
        boxHeight <- convertHeight(bb_bedpeInternal$boxHeight,
            unitTo = get("page_units", envir = bbEnv),
            valueOnly = TRUE
        )
        spaceHeight <- boxHeight * (bb_bedpeInternal$spaceHeight)
    }


    limit <- floor((as.numeric(vp$height) + spaceHeight) /
        (boxHeight + spaceHeight))
    wiggle <- abs(bb_bedpe$chromend - bb_bedpe$chromstart) *
        bb_bedpeInternal$spaceWidth


    if (nrow(bedpe) > 0) {
        bedpe$row <- 0

        ## Convert to numeric matrix for Rcpp function parsing
        bedpeMatrix <- as.matrix(bedpe[, c(
            2, 6, 5,
            seq((ncol(bedpe) - 6), ncol(bedpe))
        )])

        ## Assign a row for each element
        rowBedpe <- checkRow(bedpeMatrix, limit, 9, wiggle)

        rowBedpe <- as.data.frame(rowBedpe)
        colnames(rowBedpe) <- c(
            "start1", "stop2", "start2", "width1", "width2",
            "pos1", "pos2", "distance", "colorby", "row"
        )

        if (any(rowBedpe$row == 0)) {
            rowBedpe <- rowBedpe[which(rowBedpe$row != 0), ]
            warning("Not enough plotting space for all provided pair elements.",
                call. = FALSE
            )

            limitGrob <- textGrob(
                label = "+", x = unit(1, "npc"),
                y = unit(1, "npc"), just = c("right", "top"),
                gp = gpar(col = "grey", fontsize = 6)
            )
            assign("bedpe_grobs",
                addGrob(
                    gTree = get("bedpe_grobs", envir = bbEnv),
                    child = limitGrob
                ),
                envir = bbEnv
            )
        }

        ## Change row index to 0
        rowBedpe$row <- rowBedpe$row - 1
        rowBedpe$y <- rowBedpe$row * (boxHeight + spaceHeight)

        # =====================================================================
        # COLORS
        # =====================================================================

        if (is.null(bb_bedpeInternal$colorby)) {
            if (class(bb_bedpeInternal$fill) == "function") {
                colors <- bb_bedpeInternal$fill(limit)
                indeces <- rowBedpe$row + 1
                rowBedpe$color <- colors[indeces]
            } else {
                if (length(bb_bedpeInternal$fill) == 1) {
                    rowBedpe$color <- rep(bb_bedpeInternal$fill, nrow(rowBedpe))
                } else {
                    colors <- rep(
                        bb_bedpeInternal$fill,
                        ceiling(limit /
                            length(bb_bedpeInternal$fill))
                    )[seq(
                        1, limit
                    )]
                    indeces <- rowBedpe$row + 1
                    rowBedpe$color <- colors[indeces]
                }
            }
        } else {
            if (class(bb_bedpeInternal$fill) == "function") {
                rowBedpe$color <- bb_maptocolors(rowBedpe$colorby,
                    bb_bedpeInternal$fill,
                    range = bb_bedpe$zrange
                )
                bb_bedpe$color_palette <- bb_bedpeInternal$fill
            } else {
                colorbyCol <- factor(rowBedpe$colorby)
                mappedColors <- rep(
                    bb_bedpeInternal$fill,
                    ceiling(length(levels(colorbyCol)) /
                        length(bb_bedpeInternal$fill))
                )
                names(mappedColors) <- levels(colorbyCol)
                rowBedpe$color <- mappedColors[colorbyCol]
            }
        }


        # =====================================================================
        # MAKE GROBS
        # =====================================================================

        if (bb_bedpeInternal$baseline == TRUE) {
            baselineGrob <- segmentsGrob(
                x0 = unit(0, "npc"), y0 = 0,
                x1 = unit(1, "npc"), y1 = 0,
                default.units = "native",
                gp = gpar(
                    col = bb_bedpeInternal$baseline.color,
                    lwd = bb_bedpeInternal$baseline.lwd
                )
            )
            assign("bedpe_grobs",
                addGrob(
                    gTree = get("bedpe_grobs", envir = bbEnv),
                    child = baselineGrob
                ),
                envir = bbEnv
            )
        }

        bb_bedpeInternal$gp$fill <- rowBedpe$color
        bb_bedpeInternal$gp$col <- bb_bedpeInternal$linecolor

        bedpeRect1 <- rectGrob(
            x = rowBedpe$start1,
            y = rowBedpe$y,
            width = rowBedpe$width1,
            height = boxHeight,
            just = c("left", "bottom"),
            default.units = "native",
            gp = bb_bedpeInternal$gp
        )

        bedpeRect2 <- rectGrob(
            x = rowBedpe$start2,
            y = rowBedpe$y,
            width = rowBedpe$width2,
            height = boxHeight,
            just = c("left", "bottom"),
            default.units = "native",
            gp = bb_bedpeInternal$gp
        )

        bb_bedpeInternal$gp$col <- rowBedpe$color
        bb_bedpeInternal$gp$lineend <- "butt"

        bedpeLine <- segmentsGrob(
            x0 = rowBedpe$pos1,
            y0 = rowBedpe$y + 0.5 * boxHeight,
            x1 = rowBedpe$pos2,
            y1 = rowBedpe$y + 0.5 * boxHeight,
            default.units = "native",
            gp = bb_bedpeInternal$gp
        )

        assign("bedpe_grobs",
            addGrob(
                gTree = get("bedpe_grobs", envir = bbEnv),
                child = bedpeLine
            ),
            envir = bbEnv
        )
        assign("bedpe_grobs",
            addGrob(
                gTree = get("bedpe_grobs", envir = bbEnv),
                child = bedpeRect1
            ),
            envir = bbEnv
        )
        assign("bedpe_grobs",
            addGrob(
                gTree = get("bedpe_grobs", envir = bbEnv),
                child = bedpeRect2
            ),
            envir = bbEnv
        )
    } else {
        if (txdbChecks == TRUE) {
            warning("Data contains no values.", call. = FALSE)
        }
    }

    # =========================================================================
    # IF PLOT == TRUE, DRAW GROBS
    # =========================================================================

    if (bb_bedpeInternal$draw == TRUE) {
        grid.draw(get("bedpe_grobs", envir = bbEnv))
    }

    # =========================================================================
    # ADD GROBS TO OBJECT
    # =========================================================================

    bb_bedpe$grobs <- get("bedpe_grobs", envir = bbEnv)

    # =========================================================================
    # RETURN OBJECT
    # =========================================================================
    message("bb_pairs[", vp$name, "]")
    invisible(bb_bedpe)
}