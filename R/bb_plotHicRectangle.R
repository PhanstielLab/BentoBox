#' Plot a triangular Hi-C interaction matrix in a rectangular format
#' 
#' @usage bb_plotHicRectangle(
#'     data,
#'     resolution = "auto",
#'     zrange = NULL,
#'     norm = "KR",
#'     matrix = "observed",
#'     chrom,
#'     chromstart = NULL,
#'     chromend = NULL,
#'     assembly = "hg38",
#'     palette = colorRampPalette(brewer.pal(n = 9, "YlGnBu")),
#'     colorTrans = "linear",
#'     x = NULL,
#'     y = NULL,
#'     width = NULL,
#'     height = NULL,
#'     just = c("left", "top"),
#'     default.units = "inches",
#'     draw = TRUE,
#'     params = NULL,
#'     quiet = FALSE
#' )
#'
#' @param data Path to .hic file as a string or a 3-column dataframe of
#' interaction counts in sparse upper triangular format.
#' @param resolution A numeric specifying the width in basepairs
#' of each pixel. For hic files, "auto" will attempt to choose a
#' resolution based on the size of the region. For
#' dataframes, "auto" will attempt to detect the resolution the
#' dataframe contains.
#' @param zrange A numeric vector of length 2 specifying the range of
#' interaction scores to plot, where extreme values will be set to the
#' max or min.
#' @param norm Character value specifying hic data normalization method,
#' if giving .hic file. This value must be found in the .hic file.
#' Default value is \code{norm = "KR"}.
#' @param matrix Character value indicating the type of matrix to output.
#' Default value is \code{matrix = "observed"}. Options are:
#' \itemize{
#' \item{\code{"observed"}: }{Observed counts.}
#' \item{\code{"oe"}: }{Observed/expected counts.}
#' \item{\code{"log2oe"}: }{Log2 transformed observed/expected counts.}
#' }
#' @param chrom Chromosome of region to be plotted, as a string.
#' @param chromstart Integer start position on chromosome to be plotted.
#' @param chromend Integer end position on chromosome to be plotted.
#' @param assembly Default genome assembly as a string or a
#' \link[BentoBox]{bb_assembly} object.
#' Default value is \code{assembly = "hg38"}.
#' @param palette A function describing the color palette to use for
#' representing scale of interaction scores. Default value is
#' \code{palette =  colorRampPalette(brewer.pal(n = 9, "YlGnBu"))}.
#' @param colorTrans A string specifying how to scale Hi-C colors.
#' Options are "linear", "log", "log2", or "log10".
#' Default value is \code{colorTrans = "linear"}.
#' @param x A numeric or unit object specifying rectangle
#' Hi-C plot x-location.
#' @param y A numeric, unit object, or character containing a "b" combined
#' with a numeric value specifying rectangle Hi-C plot y-location.
#' The character value will
#' place the rectangle Hi-C plot y relative to the bottom of the most
#' recently plotted BentoBox plot according to the units of the BentoBox page.
#' @param width A numeric or unit object specifying the width of the
#' Hi-C plot rectangle.
#' @param height A numeric or unit object specifying the height of the
#' Hi-C plot rectangle.
#' @param just Justification of rectangle Hi-C plot relative to
#' its (x, y) location. If there are two values, the first value
#' specifies horizontal justification and the second value specifies
#' vertical justification.
#' Possible string values are: \code{"left"}, \code{"right"},
#' \code{"centre"}, \code{"center"}, \code{"bottom"}, and \code{"top"}.
#' Default value is \code{just = c("left", "top")}.
#' @param default.units A string indicating the default units to use
#' if \code{x}, \code{y}, \code{width}, or \code{height} are only given
#' as numerics. Default value is \code{default.units = "inches"}.
#' @param draw A logical value indicating whether graphics output should
#' be produced. Default value is \code{draw = TRUE}.
#' @param params An optional \link[BentoBox]{bb_params} object containing
#' relevant function parameters.
#' @param quiet A logical indicating whether or not to print messages.
#'
#' @return Returns a \code{bb_hicRectangle} object containing
#' relevant genomic region, Hi-C data, placement,
#' and \link[grid]{grob} information.
#'
#' @examples
#' ## Load Hi-C data
#' library(BentoBoxData)
#' data("bb_imrHicData")
#'
#' ## Create a page
#' bb_pageCreate(width = 6, height = 3.5, default.units = "inches")
#'
#' ## Plot and place rectangle Hi-C plot
#' hicPlot <- bb_plotHicRectangle(
#'     data = bb_imrHicData, resolution = 10000,
#'     zrange = c(0, 70),
#'     chrom = "chr21",
#'     chromstart = 28950000, chromend = 29800000,
#'     assembly = "hg19",
#'     x = 0.5, y = 0.5, width = 5, height = 2.5,
#'     just = c("left", "top"),
#'     default.units = "inches"
#' )
#'
#' ## Annotate x-axis genome label
#' bb_annoGenomeLabel(
#'     plot = hicPlot, scale = "Kb", x = 0.5, y = 3.03,
#'     just = c("left", "top")
#' )
#'
#' ## Annotate heatmap legend
#' bb_annoHeatmapLegend(
#'     plot = hicPlot, x = 5.6, y = 0.5,
#'     width = 0.13, height = 1.5,
#'     just = c("left", "top")
#' )
#'
#' ## Hide page guides
#' bb_pageGuideHide()
#' @details
#'
#' This function is similar is \link[BentoBox]{bb_plotHicTriangle} but
#' will fill in additional pixels around the
#' the triangular portion of the plot to make a rectangle.
#'
#' A rectangle Hi-C plot can be placed on a BentoBox coordinate
#' page by providing plot placement parameters:
#' \preformatted{
#' bb_plotHicRectangle(data, chrom,
#'                     chromstart = NULL, chromend = NULL,
#'                     x, y, width, height, just = c("left", "top"),
#'                     default.units = "inches")
#' }
#' This function can also be used to quickly plot an unannotated
#' rectangle Hi-C plot by ignoring plot placement parameters:
#' \preformatted{
#' bb_plotHicRectangle(data, chrom,
#'                     chromstart = NULL, chromend = NULL)
#' }
#'
#' @seealso \link[BentoBox]{bb_readHic}, \link[BentoBox]{bb_plotHicTriangle}
#'
#' @export
bb_plotHicRectangle <- function(data, resolution = "auto", zrange = NULL,
                                norm = "KR", matrix = "observed", chrom,
                                chromstart = NULL, chromend = NULL,
                                assembly = "hg38",
                                palette = colorRampPalette(brewer.pal(
                                    n = 9, "YlGnBu"
                                )),
                                colorTrans = "linear", x = NULL, y = NULL,
                                width = NULL, height = NULL,
                                just = c("left", "top"),
                                default.units = "inches", draw = TRUE,
                                params = NULL, quiet = FALSE) {

    # =========================================================================
    # FUNCTIONS
    # =========================================================================

    ## Define a function that catches errors for bb_plotTriangleHic
    errorcheck_bb_plotHicRectangle <- function(hic, hic_plot, norm, assembly) {

        ###### hic/norm 
        bb_hicErrors(hic = hic, 
                    norm = norm)
        
        ## Even though straw technically works without "chr" for hg19,
        ## will not accept for consistency purposes
        if (assembly == "hg19") {
            if (grepl("chr", hic_plot$chrom) == FALSE) {
                stop("'", hic_plot$chrom, "'",
                    "is an invalid input for an hg19 chromsome. ",
                    "Please specify chromosome as",
                    "'chr", hic_plot$chrom, "'.",
                    call. = FALSE
                )
            }
        }

        ## Genomic region errors
        bb_regionErrors(chromstart = hic_plot$chromstart,
                    chromend = hic_plot$chromend)

        ##### zrange errors
        bb_rangeErrors(range = hic_plot$zrange)
    }

    ## Define a function that adjusts chromstart/chromend to include
    ## additional data for rectangle
    rect_region <- function(inputHeight, inputWidth, chromstart, chromend) {
        if (!is.null(chromstart) & !is.null(chromend)) {
            dimRatio <- inputHeight / inputWidth
            rectChromstart <- chromstart - ((chromend - chromstart) * dimRatio)
            rectChromend <- chromend + ((chromend - chromstart) * dimRatio)
        } else {
            rectChromstart <- NULL
            rectChromend <- NULL
        }
        
        return(list(rectChromstart, rectChromend))
    }

    ## Define a function that subsets data for rectangle Hi-C plot
    subset_data <- function(hic, hic_plot) {
        if (nrow(hic) > 0) {
            hic <- hic[which(hic[, 1] >= floor(hic_plot$chromstart /
                hic_plot$resolution) *
                hic_plot$resolution &
                hic[, 1] < hic_plot$chromend &
                hic[, 2] >= floor(hic_plot$chromstart / hic_plot$resolution) *
                    hic_plot$resolution &
                hic[, 2] < hic_plot$chromend), ]
        }

        return(hic)
    }

    rect_vpScale <- function(hic_plot, hic_plotInternal) {
        if (!is.null(hic_plot$chromstart) & !is.null(hic_plot$chromend)) {

            # updated chromstart
            in_xscale <- c(hic_plot$chromstart, hic_plot$chromend)
            # input chromstart
            out_xscale <- c(
                hic_plotInternal$chromstart,
                hic_plotInternal$chromend
            )
        } else {
            in_xscale <- c(0, 1)
            out_xscale <- c(0, 1)
        }

        return(list(in_xscale, out_xscale))
    }

    # =========================================================================
    # PARSE PARAMETERS
    # =========================================================================
    
    bb_rhicInternal <- parseParams(params = params, 
                                    defaultArgs = formals(eval(
                                        match.call()[[1]])),
                                    declaredArgs = lapply(
                                        match.call()[-1], eval.parent, n = 2),
                                    class = "bb_rhicInternal")

    # =========================================================================
    # INITIALIZE OBJECT
    # =========================================================================

    hic_plot <- structure(list(
        chrom = bb_rhicInternal$chrom,
        chromstart = bb_rhicInternal$chromstart,
        chromend = bb_rhicInternal$chromend,
        altchrom = bb_rhicInternal$chrom,
        altchromstart = bb_rhicInternal$chromstart,
        altchromend = bb_rhicInternal$chromend,
        assembly = bb_rhicInternal$assembly,
        resolution = bb_rhicInternal$resolution,
        x = bb_rhicInternal$x, y = bb_rhicInternal$y,
        width = bb_rhicInternal$width,
        height = bb_rhicInternal$height,
        just = bb_rhicInternal$just,
        color_palette = NULL,
        colorTrans = bb_rhicInternal$colorTrans,
        zrange = bb_rhicInternal$zrange,
        outsideVP = NULL, grobs = NULL
    ),
    class = "bb_hicRectangle"
    )
    attr(x = hic_plot, which = "plotted") <- bb_rhicInternal$draw

    # =========================================================================
    # CHECK PLACEMENT/ARGUMENT ERRORS
    # =========================================================================

    if (is.null(bb_rhicInternal$data)) stop("argument \"data\" is missing, ",
                                            "with no default.", call. = FALSE)
    if (is.null(bb_rhicInternal$chrom)) stop("argument \"chrom\" is missing, ",
                                            "with no default.", call. = FALSE)
    check_placement(object = hic_plot)

    # =========================================================================
    # PARSE ASSEMBLY
    # =========================================================================

    hic_plot$assembly <- parse_bbAssembly(assembly = hic_plot$assembly)

    # =========================================================================
    # PARSE UNITS
    # =========================================================================

    hic_plot <- defaultUnits(
        object = hic_plot,
        default.units = bb_rhicInternal$default.units
    )

    # =========================================================================
    # CATCH ERRORS
    # =========================================================================

    errorcheck_bb_plotHicRectangle(
        hic = bb_rhicInternal$data,
        hic_plot = hic_plot,
        norm = bb_rhicInternal$norm,
        assembly = hic_plot$assembly$Genome
    )

    # =========================================================================
    # GENOMIC SCALE
    # =========================================================================
    
    scaleChecks <- genomicScale(object = hic_plot,
                                objectInternal = bb_rhicInternal,
                                plotType = "rectangle Hi-C plot")
    hic_plot <- scaleChecks[[1]]
    bb_rhicInternal <- scaleChecks[[2]]

    # =========================================================================
    # ADJUST RESOLUTION
    # =========================================================================

    if (bb_rhicInternal$resolution == "auto") {
        hic_plot <- adjust_resolution(
            hic = bb_rhicInternal$data,
            hic_plot = hic_plot
        )
    } else {
        hic_plot <- hic_limit(
            hic = bb_rhicInternal$data,
            hic_plot = hic_plot
        )
    }

    # =========================================================================
    # ADJUST CHROMSTART TO GRAB MORE DATA
    # =========================================================================

    if (is.null(hic_plot$x) & is.null(hic_plot$y)) {
        inputHeight <- 0.5
        inputWidth <- 1
    } else {
        inputHeight <- convertHeight(hic_plot$height,
            unitTo = get("page_units", envir = bbEnv),
            valueOnly = TRUE
        )
        inputWidth <- convertWidth(hic_plot$width,
            unitTo = get("page_units", envir = bbEnv),
            valueOnly = TRUE
        )
    }


    adjRegion <- rect_region(
        inputHeight = inputHeight, inputWidth = inputWidth,
        chromstart = hic_plot$chromstart,
        chromend = hic_plot$chromend
    )

    hic_plot$chromstart <- adjRegion[[1]]
    hic_plot$chromend <- adjRegion[[2]]

    # =========================================================================
    # READ IN DATA
    # =========================================================================

    hic <- read_data(
        hic = bb_rhicInternal$data, hic_plot = hic_plot,
        norm = bb_rhicInternal$norm, assembly = hic_plot$assembly,
        type = bb_rhicInternal$matrix,
        quiet = bb_rhicInternal$quiet
    )

    # =========================================================================
    # SUBSET DATA
    # =========================================================================

    hic <- subset_data(hic = hic, hic_plot = hic_plot)

    # =========================================================================
    # SET ZRANGE AND SCALE DATA
    # =========================================================================

    hic_plot <- set_zrange(hic = hic, hic_plot = hic_plot)
    hic$counts[hic$counts <= hic_plot$zrange[1]] <- hic_plot$zrange[1]
    hic$counts[hic$counts >= hic_plot$zrange[2]] <- hic_plot$zrange[2]

    # =========================================================================
    # CONVERT NUMBERS TO COLORS
    # =========================================================================

    ## if we don't have an appropriate zrange
    ## (even after setting it based on a null zrange), can't scale to colors
    if (!is.null(hic_plot$zrange) & length(unique(hic_plot$zrange)) == 2) {
        if (grepl("log", bb_rhicInternal$colorTrans) == TRUE) {
            logBase <- as.numeric(gsub("log", "", bb_rhicInternal$colorTrans))
            if (is.na(logBase)) {
                logBase <- exp(1)
            }

            ## Won't scale to log if negative values
            if (any(hic$counts < 0)) {
                stop("Negative values in Hi-C data. Cannot scale ",
                "colors on a log scale. Please set `colorTrans = 'linear'`.",
                    call. = FALSE
                )
            }


            hic$counts <- log(hic$counts, base = logBase)
            hic$color <- bb_mapColors(vector = hic$counts,
                palette = bb_rhicInternal$palette,
                range = log(hic_plot$zrange, logBase)
            )
        } else {
            hic$color <- bb_mapColors(vector = hic$counts,
                palette = bb_rhicInternal$palette,
                range = hic_plot$zrange
            )
        }

        hic_plot$color_palette <- bb_rhicInternal$palette
    }

    # =========================================================================
    # VIEWPORTS
    # =========================================================================

    ## Viewport scale
    scale <- rect_vpScale(
        hic_plot = hic_plot,
        hic_plotInternal = bb_rhicInternal
    )

    ## Get viewport name
    currentViewports <- current_viewports()
    vp_name <- paste0(
        "bb_hicRectangle",
        length(grep(
            pattern = "bb_hicRectangle",
            x = currentViewports
        )) + 1
    )

    if (is.null(hic_plot$x) | is.null(hic_plot$y)) {
        inside_vp <- viewport(
            height = unit(4 / sqrt(2), "npc"),
            width = unit(2 / sqrt(2), "npc"),
            x = unit(scale[[1]][1], "native"),
            y = unit(0, "npc"),
            xscale = scale[[1]],
            yscale = scale[[1]],
            just = c("left", "bottom"),
            name = paste0(vp_name, "_inside"),
            angle = -45
        )

        outside_vp <- viewport(
            height = unit(0.5, "snpc"),
            width = unit(1, "snpc"),
            x = unit(0.5, "npc"),
            y = unit(0.5, "npc"),
            xscale = scale[[2]],
            clip = "on",
            just = "center",
            name = paste0(vp_name, "_outside")
        )


        if (bb_rhicInternal$draw == TRUE) {
            vp_name <- "bb_hicRectangle1"
            inside_vp$name <- "bb_hicRectangle1_inside"
            outside_vp$name <- "bb_hicRectangle1_outside"
            grid.newpage()
        }
    } else {

        ## Convert coordinates into same units as page
        page_coords <- convert_page(object = hic_plot)

        ## Get length of side of inside viewport
        vp_side <- (as.numeric(page_coords$width) + 2 *
            as.numeric(page_coords$height)) / sqrt(2)

        inside_vp <- viewport(
            height = unit(
                vp_side,
                get("page_units", envir = bbEnv)
            ),
            width = unit(
                vp_side,
                get("page_units", envir = bbEnv)
            ),
            x = unit(scale[[1]][1], "native"),
            y = unit(0, "npc"),
            xscale = scale[[1]],
            yscale = scale[[1]],
            just = c("left", "bottom"),
            name = paste0(vp_name, "_inside"),
            angle = -45
        )

        outside_vp <- viewport(
            height = page_coords$height,
            width = page_coords$width,
            x = page_coords$x, y = page_coords$y,
            just = hic_plot$just,
            xscale = scale[[2]],
            clip = "on",
            name = paste0(vp_name, "_outside")
        )


        add_bbViewport(paste0(vp_name, "_outside"))
    }

    # =========================================================================
    # INITIALIZE GTREE FOR GROBS
    # =========================================================================

    hic_plot$outsideVP <- outside_vp
    assign("hic_grobs3", gTree(vp = inside_vp), envir = bbEnv)

    # =========================================================================
    # MAKE GROBS
    # =========================================================================

    if (!is.null(hic_plot$chromstart) & !is.null(hic_plot$chromend)) {
        if (nrow(hic) > 0) {
            hic_squares <- rectGrob(
                x = hic$x,
                y = hic$y,
                just = c("left", "bottom"),
                width = hic_plot$resolution,
                height = hic_plot$resolution,
                gp = gpar(col = NA, fill = hic$color),
                default.units = "native"
            )

            assign("hic_grobs3",
                addGrob(
                    gTree = get("hic_grobs3", envir = bbEnv),
                    child = hic_squares
                ),
                envir = bbEnv
            )
        } else {
            if (!is.na(hic_plot$resolution)){
                warning("No data found in region. Suggestions: check that ",
                        "chromosome names match genome assembly; ",
                        "check region.", call. = FALSE)
            }
        }
    }


    # ========================================================================
    # IF DRAW == TRUE, DRAW GROBS
    # ========================================================================

    if (bb_rhicInternal$draw == TRUE) {
        pushViewport(outside_vp)
        grid.draw(get("hic_grobs3", envir = bbEnv))
        upViewport()
    }

    # =========================================================================
    # ADD GROBS TO OBJECT AND RESET CHROMSTART AND CHROMEND
    # =========================================================================

    hic_plot$grobs <- get("hic_grobs3", envir = bbEnv)
    hic_plot$chromstart <- bb_rhicInternal$chromstart
    hic_plot$chromend <- bb_rhicInternal$chromend

    # =========================================================================
    # RETURN OBJECT
    # =========================================================================

    message("bb_hicRectangle[", vp_name, "]")
    invisible(hic_plot)
}
