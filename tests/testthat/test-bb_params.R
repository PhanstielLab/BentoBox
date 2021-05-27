test_that("bb_params object created", {
    object <- bb_params(chrom = "chr1")
    expect_equal(class(object), "bb_params")
})

test_that("bb_params object concatenates", {
    object1 <- bb_params(chrom = "chr1")
    object2 <- bb_params(chromstart = 1000000)
    object3 <- bb_params(chrom = "chr1", chromstart = 1000000)

    expect_equal(class(c(object1, object2)), "bb_params")
    expect_equal(c(object1, object2), object3)
    expect_warning(c(object1, "gene"))
})

test_that("bb_params parameter parsing", {
    params <- bb_params(chrom = "chr1", assembly = "hg19")
    objectParams <- list(
        chromstart = 1000000
    )
    expect_mapequal(
        BentoBox:::parseParams(
            bb_params = params,
            object_params = objectParams
        ),
        list(
            chrom = "chr1",
            chromstart = 1000000,
            assembly = "hg19"
        )
    )

    objectParams <- list(
        chrom = "chr2"
    )

    expect_mapequal(
        BentoBox:::parseParams(
            bb_params = params,
            object_params = objectParams
        ),
        list(
            chrom = "chr2",
            assembly = "hg19"
        )
    )
})
