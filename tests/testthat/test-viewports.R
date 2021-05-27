test_that("Viewport conversions", {
    bb_pageCreate(width = 2, height = 2, default.units = "inches")
    testVP <- viewport(
        x = 1, y = 1,
        width = 1, height = 1,
        just = "center",
        default.units = "inches"
    )
    expect_equal(
        BentoBox:::vp_bottomLeft(testVP),
        list(unit(0.5, "inches"), unit(0.5, "inches"))
    )
    expect_equal(
        BentoBox:::vp_topRight(testVP),
        list(unit(1.5, "inches"), unit(1.5, "inches"))
    )
    expect_equal(
        BentoBox:::vp_bottomRight(testVP),
        list(unit(1.5, "inches"), unit(0.5, "inches"))
    )

    testVP <- viewport(
        x = 0.25, y = 0.25,
        width = 1.5, height = 1,
        just = c("left", "bottom"),
        default.units = "inches"
    )
    expect_equal(
        BentoBox:::adjust_vpCoords(testVP),
        list(unit(1, "inches"), unit(0.75, "inches"))
    )
})

test_that("Viewport order, naming, and numbering", {
    data("bb_bedpeData")
    bb_pageCreate(width = 3, height = 5, default.units = "inches")
    arches <- suppressMessages(bb_plotPairsArches(
        data = bb_bedpeData,
        chrom = "chr21", chromstart = 28000000, chromend = 30300000,
        x = 0.5, y = 2.5, width = 2, height = 0.25,
        just = c("left", "top"), default.units = "inches",
        fill = "black", linecolor = "black", flip = TRUE
    ))
    expect_setequal(
        unlist(BentoBox:::current_viewports()),
        "bb_arches1"
    )
    expect_equal(current.viewport()$name, "bb_page")

    data("bb_imrH3K27acData")
    suppressMessages(bb_plotSignal(
        data = bb_imrH3K27acData,
        chrom = "chr21", chromstart = 28000000, chromend = 30300000,
        x = 0.5, y = 2.75, width = 2, height = 0.5,
        just = c("left", "top"), default.units = "inches"
    ))
    expect_setequal(
        unlist(BentoBox:::current_viewports()),
        c("bb_arches1", "bb_signal1")
    )
    suppressMessages(bb_plotPairsArches(
        data = bb_bedpeData,
        chrom = "chr21", chromstart = 28000000, chromend = 30300000,
        x = 0.5, y = 2.5, width = 2, height = 0.25,
        just = c("left", "top"), default.units = "inches",
        fill = "black", linecolor = "black", flip = TRUE
    ))
    expect_setequal(
        unlist(BentoBox:::current_viewports()),
        c("bb_arches1", "bb_signal1", "bb_arches2")
    )

    bb_pagePlotRemove(plot = arches)
    expect_setequal(
        unlist(BentoBox:::current_viewports()),
        c("bb_signal1", "bb_arches2")
    )
})

test_that("Below-y coordinate calculation", {
    data("bb_imrHicData")
    bb_pageCreate(width = 3, height = 5, default.units = "inches")
    suppressMessages(bb_plotHicSquare(
        data = bb_imrHicData,
        chrom = "chr21",
        chromstart = 28000000, chromend = 30300000,
        x = 0.5, y = 0.5, width = 2, height = 2,
        just = c("left", "top"), default.units = "inches"
    ))
    expect_equal(BentoBox::plot_belowY("0b"), unit(2.5, "inches"))
})

test_that("draw parameter and bb_pagePlotPlace", {
    data("bb_imrH3K27acData")
    bb_pageCreate(width = 3, height = 3, default.units = "inches")

    expect_error(signalPlot <- bb_plotSignal(
        data = bb_imrH3K27acData,
        chrom = "chr21", chromstart = 28000000, chromend = 30300000,
        x = 0.25,
        draw = TRUE
    ))

    signalPlot <- suppressMessages(bb_plotSignal(
        data = bb_imrH3K27acData,
        chrom = "chr21", chromstart = 28000000, chromend = 30300000,
        x = 0.25,
        draw = FALSE
    ))
    expect_equal(
        unlist(BentoBox:::current_viewports()),
        NULL
    )

    signalPlot <- suppressMessages(bb_pagePlotPlace(
        plot = signalPlot,
        x = 0.5, y = 0.5, width = 2, height = 1,
        just = c("left", "top"), default.units = "inches"
    ))
    expect_setequal(
        unlist(BentoBox:::current_viewports()),
        c("bb_signal1")
    )
    expect_equal(signalPlot$x, unit(0.5, "inches"))
})
