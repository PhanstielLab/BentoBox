## Define a function to get the data packages of a genome assembly
# @param genome Genome name
# @param TxDb a TxDb to switch from default
getPackages <- function(genome, TxDb = NULL) {
    switch(genome,
        "bosTau8" = structure(list(
            "Genome" = "bosTau8", "TxDb" = ifelse(
                is.null(TxDb), "TxDb.Btaurus.UCSC.bosTau8.refGene", TxDb
            ),
            "OrgDb" = "org.Bt.eg.db",
            gene.id.column = "ENTREZID", display.column = "SYMBOL",
            "BSgenome" = "BSgenome.Btaurus.UCSC.bosTau8"
        ), class = "bb_assembly"),
        "bosTau9" = structure(list(
            "Genome" = "bosTau9", "TxDb" = ifelse(
                is.null(TxDb), "TxDb.Btaurus.UCSC.bosTau9.refGene", TxDb
            ),
            "OrgDb" = "org.Bt.eg.db", gene.id.column = "ENTREZID",
            display.column = "SYMBOL",
            "BSgenome" = "BSgenome.Btaurus.UCSC.bosTau9"
        ), class = "bb_assembly"),
        "canFam3" = structure(list(
            "Genome" = "canFam3", "TxDb" = ifelse(
                is.null(TxDb), "TxDb.Cfamiliaris.UCSC.canFam3.refGene", TxDb
            ),
            "OrgDb" = "org.Cf.eg.db", gene.id.column = "ENTREZID",
            display.column = "SYMBOL",
            "BSgenome" = "BSgenome.Cfamiliaris.UCSC.canFam3"
        ), class = "bb_assembly"),
        "ce6" = structure(list(
            "Genome" = "ce6", "TxDb" = ifelse(
                is.null(TxDb), "TxDb.Celegans.UCSC.ce6.ensGene", TxDb
            ),
            "OrgDb" = "org.Ce.eg.db", gene.id.column = "GENEID",
            display.column = "GENEID",
            "BSgenome" = "BSgenome.Celegans.UCSC.ce6"
        ), class = "bb_assembly"),
        "ce11" = structure(list(
            "Genome" = "ce11", "TxDb" = ifelse(
                is.null(TxDb), "TxDb.Celegans.UCSC.ce11.refGene", TxDb
            ),
            "OrgDb" = "org.Ce.eg.db", gene.id.column = "ENTREZID",
            display.column = "SYMBOL",
            "BSgenome" = "BSgenome.Celegans.UCSC.ce11"
        ), class = "bb_assembly"),
        "danRer10" = structure(list(
            "Genome" = "danRer10", "TxDb" = ifelse(
                is.null(TxDb), "TxDb.Drerio.UCSC.danRer10.refGene", TxDb
            ),
            "OrgDb" = "org.Dr.eg.db", gene.id.column = "ENTREZID",
            display.column = "SYMBOL",
            "BSgenome" = "BSgenome.Drerio.UCSC.danRer10"
        ), class = "bb_assembly"),
        "danRer11" = structure(list(
            "Genome" = "danRer11", "TxDb" = ifelse(
                is.null(TxDb), "TxDb.Drerio.UCSC.danRer11.refGene", TxDb
            ),
            "OrgDb" = "org.Dr.eg.db", gene.id.column = "ENTREZID",
            display.column = "SYMBOL",
            "BSgenome" = "BSgenome.Drerio.UCSC.danRer11"
        ), class = "bb_assembly"),
        "dm3" = structure(list(
            "Genome" = "dm3", "TxDb" = ifelse(
                is.null(TxDb), "TxDb.Dmelanogaster.UCSC.dm3.ensGene", TxDb
            ),
            "OrgDb" = "org.Dm.eg.db", gene.id.column = "ENSEMBL",
            display.column = "SYMBOL",
            "BSgenome" = "BSgenome.Dmelanogaster.UCSC.dm3"
        ), class = "bb_assembly"),
        "dm6" = structure(list(
            "Genome" = "dm6", "TxDb" = ifelse(
                is.null(TxDb), "TxDb.Dmelanogaster.UCSC.dm6.ensGene", TxDb
            ),
            "OrgDb" = "org.Dm.eg.db", gene.id.column = "ENSEMBL",
            display.column = "SYMBOL",
            "BSgenome" = "BSgenome.Dmelanogaster.UCSC.dm6"
        ), class = "bb_assembly"),
        "galGal4" = structure(list(
            "Genome" = "galGal4", "TxDb" = ifelse(
                is.null(TxDb), "TxDb.Ggallus.UCSC.galGal4.refGene", TxDb
            ),
            "OrgDb" = "org.Gg.eg.db", gene.id.column = "ENTREZID",
            display.column = "SYMBOL",
            "BSgenome" = "BSgenome.Ggallus.UCSC.galGal4"
        ), class = "bb_assembly"),
        "galGal5" = structure(list(
            "Genome" = "galGal5", "TxDb" = ifelse(
                is.null(TxDb), "TxDb.Ggallus.UCSC.galGal5.refGene", TxDb
            ),
            "OrgDb" = "org.Gg.eg.db", gene.id.column = "ENTREZID",
            display.column = "SYMBOL",
            "BSgenome" = "BSgenome.Ggallus.UCSC.galGal5"
        ), class = "bb_assembly"),
        "galGal6" = structure(list(
            "Genome" = "galGal6", "TxDb" = ifelse(
                is.null(TxDb), "TxDb.Ggallus.UCSC.galGal6.refGene", TxDb
            ),
            "OrgDb" = "org.Gg.eg.db", gene.id.column = "ENTREZID",
            display.column = "SYMBOL",
            "BSgenome" = "BSgenome.Ggallus.UCSC.galGal6"
        ), class = "bb_assembly"),
        "hg18" = structure(list(
            "Genome" = "hg18", "TxDb" = ifelse(
                is.null(TxDb), "TxDb.Hsapiens.UCSC.hg18.knownGene", TxDb
            ),
            "OrgDb" = "org.Hs.eg.db", gene.id.column = "ENTREZID",
            display.column = "SYMBOL",
            "BSgenome" = "BSgenome.Hsapiens.UCSC.hg18"
        ), class = "bb_assembly"),
        "hg19" = structure(list(
            "Genome" = "hg19", "TxDb" = ifelse(
                is.null(TxDb), "TxDb.Hsapiens.UCSC.hg19.knownGene", TxDb
            ),
            "OrgDb" = "org.Hs.eg.db", gene.id.column = "ENTREZID",
            display.column = "SYMBOL",
            "BSgenome" = "BSgenome.Hsapiens.UCSC.hg19"
        ), class = "bb_assembly"),
        "hg38" = structure(list(
            "Genome" = "hg38", "TxDb" = ifelse
            (is.null(TxDb), "TxDb.Hsapiens.UCSC.hg38.knownGene", TxDb),
            "OrgDb" = "org.Hs.eg.db", gene.id.column = "ENTREZID",
            display.column = "SYMBOL",
            "BSgenome" = "BSgenome.Hsapiens.UCSC.hg38"
        ), class = "bb_assembly"),
        "mm9" = structure(list(
            "Genome" = "mm9", "TxDb" = ifelse(
                is.null(TxDb), "TxDb.Mmusculus.UCSC.mm9.knownGene", TxDb
            ),
            "OrgDb" = "org.Mm.eg.db", gene.id.column = "ENTREZID",
            display.column = "SYMBOL",
            "BSgenome" = "BSgenome.Mmusculus.UCSC.mm9"
        ), class = "bb_assembly"),
        "mm10" = structure(list(
            "Genome" = "mm10", "TxDb" = ifelse(
                is.null(TxDb), "TxDb.Mmusculus.UCSC.mm10.knownGene", TxDb
            ),
            "OrgDb" = "org.Mm.eg.db", gene.id.column = "ENTREZID",
            display.column = "SYMBOL",
            "BSgenome" = "BSgenome.Mmusculus.UCSC.mm10"
        ), class = "bb_assembly"),
        "rheMac3" = structure(list(
            "Genome" = "rheMac3", "TxDb" = ifelse(
                is.null(TxDb), "TxDb.Mmulatta.UCSC.rheMac3.refGene", TxDb
            ),
            "OrgDb" = "org.Mmu.eg.db", gene.id.column = "ENTREZID",
            display.column = "SYMBOL",
            "BSgenome" = "BSgenome.Mmulatta.UCSC.rheMac3"
        ), class = "bb_assembly"),
        "rheMac8" = structure(list(
            "Genome" = "rheMac8", "TxDb" = ifelse(
                is.null(TxDb), "TxDb.Mmulatta.UCSC.rheMac8.refGene", TxDb
            ),
            "OrgDb" = "org.Mmu.eg.db", gene.id.column = "ENTREZID",
            display.column = "SYMBOL",
            "BSgenome" = "BSgenome.Mmulatta.UCSC.rheMac8"
        ), class = "bb_assembly"),
        "rheMac10" = structure(list(
            "Genome" = "rheMac10", "TxDb" = ifelse(
                is.null(TxDb), "TxDb.Mmulatta.UCSC.rheMac10.refGene", TxDb
            ),
            "OrgDb" = "org.Mmu.eg.db", gene.id.column = "ENTREZID",
            display.column = "SYMBOL",
            "BSgenome" = "BSgenome.Mmulatta.UCSC.rheMac10"
        ), class = "bb_assembly"),
        "panTro5" = structure(list(
            "Genome" = "panTro5", "TxDb" = ifelse(
                is.null(TxDb), "TxDb.Ptroglodytes.UCSC.panTro5.refGene", TxDb
            ),
            "OrgDb" = "org.Pt.eg.db", gene.id.column = "ENTREZID",
            display.column = "SYMBOL",
            "BSgenome" = "BSgenome.Ptroglodytes.UCSC.panTro5"
        ), class = "bb_assembly"),
        "panTro6" = structure(list(
            "Genome" = "panTro6", "TxDb" = ifelse(
                is.null(TxDb), "TxDb.Ptroglodytes.UCSC.panTro6.refGene", TxDb
            ),
            "OrgDb" = "org.Pt.eg.db", gene.id.column = "ENTREZID",
            display.column = "SYMBOL",
            "BSgenome" = "BSgenome.Ptroglodytes.UCSC.panTro6"
        ), class = "bb_assembly"),
        "rn4" = structure(list(
            "Genome" = "rn4", "TxDb" = ifelse(
                is.null(TxDb), "TxDb.Rnorvegicus.UCSC.rn4.ensGene", TxDb
            ),
            "OrgDb" = "org.Rn.eg.db", gene.id.column = "ENSEMBL",
            display.column = "SYMBOL",
            "BSgenome" = "BSgenome.Rnorvegicus.UCSC.rn4"
        ), class = "bb_assembly"),
        "rn5" = structure(list(
            "Genome" = "rn5", "TxDb" = ifelse(
                is.null(TxDb), "TxDb.Rnorvegicus.UCSC.rn5.refGene", TxDb
            ),
            "OrgDb" = "org.Rn.eg.db", gene.id.column = "ENTREZID",
            display.column = "SYMBOL",
            "BSgenome" = "BSgenome.Rnorvegicus.UCSC.rn5"
        ), class = "bb_assembly"),
        "rn6" = structure(list(
            "Genome" = "rn6", "TxDb" = ifelse(
                is.null(TxDb), "TxDb.Rnorvegicus.UCSC.rn6.refGene", TxDb
            ),
            "OrgDb" = "org.Rn.eg.db", gene.id.column = "ENTREZID",
            display.column = "SYMBOL",
            "BSgenome" = "BSgenome.Rnorvegicus.UCSC.rn6"
        ), class = "bb_assembly"),
        "sacCer2" = structure(list(
            "Genome" = "sacCer2", "TxDb" = ifelse(
                is.null(TxDb), "TxDb.Scerevisiae.UCSC.sacCer2.sgdGene", TxDb
            ),
            "OrgDb" = "org.Sc.sgd.db", gene.id.column = "ORF",
            display.column = "GENENAME",
            "BSgenome" = "BSgenome.Scerevisiae.UCSC.sacCer2"
        ), class = "bb_assembly"),
        "sacCer3" = structure(list(
            "Genome" = "sacCer3", "TxDb" = ifelse(
                is.null(TxDb), "TxDb.Scerevisiae.UCSC.sacCer3.sgdGene", TxDb
            ),
            "OrgDb" = "org.Sc.sgd.db", gene.id.column = "ORF",
            display.column = "GENENAME",
            "BSgenome" = "BSgenome.Scerevisiae.UCSC.sacCer3"
        ), class = "bb_assembly"),
        "susScr3" = structure(list(
            "Genome" = "susScr3", "TxDb" = ifelse
            (is.null(TxDb), "TxDb.Sscrofa.UCSC.susScr3.refGene", TxDb),
            "OrgDb" = "org.Ss.eg.db", gene.id.column = "ENTREZID",
            display.column = "SYMBOL",
            "BSgenome" = "BSgenome.Sscrofa.UCSC.susScr3"
        ), class = "bb_assembly"),
        "susScr11" = structure(list(
            "Genome" = "susScr11", "TxDb" = ifelse(
                is.null(TxDb), "TxDb.Sscrofa.UCSC.susScr11.refGene", TxDb
            ),
            "OrgDb" = "org.Ss.eg.db", gene.id.column = "ENTREZID",
            display.column = "SYMBOL",
            "BSgenome" = "BSgenome.Sscrofa.UCSC.susScr11"
        ), class = "bb_assembly"),
    )
}

## Define a function to check that a TxDb, Org, or BSgenome package is loaded
# @param package Associated package
# @param message Message for warning message
check_loadedPackage <- function(package, message) {
    if (!package %in% (.packages())) {
        warning(message, call. = FALSE)
        return(FALSE)
    } else {
        return(TRUE)
    }
}

## Define a function that checks for whole chromosome data and
## sets the plot xscale accordingly
# @param object plot object
# @param objectInternal internal plot object
# @param plotType string of plot type to show up in error message
genomicScale <- function(object, objectInternal, plotType) {
    if (is.null(object$chromstart) & is.null(object$chromend)) {
        if (is(object$assembly$TxDb, "TxDb")) {
            txdbChecks <- TRUE
        } else {
            txdbChecks <- check_loadedPackage(
                package = object$assembly$TxDb,
                message = paste(
                    paste0("`", object$assembly$TxDb, "`"),
                    "not loaded. Please install and load to generate",
                    plotType, "."
                )
            )
        }
        objectInternal$xscale <- c(0, 1)
        if (txdbChecks == TRUE) {
            if (is(object$assembly$TxDb, "TxDb")) {
                tx_db <- object$assembly$TxDb
            } else {
                tx_db <- eval(parse(text = object$assembly$TxDb))
            }
        }
        assembly_data <- GenomeInfoDb::seqlengths(tx_db)
        if (!object$chrom %in% names(assembly_data)) {
            warning("Chromosome ",
                "'", object$chrom, "' ",
                "not found in ",
                "`", object$assembly$TxDb$packageName, "`",
                " and data for entire chromosome cannot be plotted.",
                call. = FALSE
            )
        } else {
            object$chromstart <- 1
            object$chromend <- assembly_data[[object$chrom]]
            if (class(object) %in% c("bb_hicTriangle", "bb_hicRectangle")) {
                object$altchromstart <- 1
                object$altchromend <- assembly_data[[object$chrom]]
            }
            objectInternal$xscale <- c(object$chromstart, object$chromend)
        }
    } else {
        txdbChecks <- TRUE
        objectInternal$xscale <- c(object$chromstart, object$chromend)
    }

    objectInternal$txdbChecks <- txdbChecks

    return(list(object, objectInternal))
}

## Define a function that checks for and gets gene/transcript data
# @param object plot object
# @param objectInternal internal plot object
geneData <- function(object, objectInternal) {

    ## TxDb
    if (is(object$assembly$TxDb, "TxDb")) {
        txdbChecks <- TRUE
    } else {
        txdbChecks <- check_loadedPackage(
            package = object$assembly$TxDb,
            message = paste(
                paste0("`", object$assembly$TxDb, "`"),
                "not loaded. Please install and load to plot genes
                or transcripts."
            )
        )
    }

    ## orgDb

    orgdbChecks <- check_loadedPackage(
        package = object$assembly$OrgDb,
        message = paste(
            paste0("`", object$assembly$OrgDb, "`"),
            "not loaded. Please install and load to plot genes
            or transcripts"
        )
    )

    ## Data
    data <- data.frame(matrix(ncol = 22, nrow = 0))
    xscale <- c(0, 1)

    if (txdbChecks == TRUE & orgdbChecks == TRUE) {

        ## Load txdb
        if (is(object$assembly$TxDb, "TxDb")) {
            tx_db <- object$assembly$TxDb
        } else {
            tx_db <- eval(parse(text = object$assembly$TxDb))
        }

        genome <- GenomeInfoDb::seqlengths(tx_db)

        if (object$assembly$gene.id.column ==
            object$assembly$display.column) {
            objectInternal$displayCol <- "GENEID"
        } else {
            objectInternal$displayCol <- object$assembly$display.column
        }

        if (!object$chrom %in% names(genome)) {
            warning("Chromosome ", "'", object$chrom, "'",
                "not found in ", "`", object$assembly$TxDb$packageName, "`",
                " and data for entire chromosome cannot be plotted.",
                call. = FALSE
            )
        } else {
            if (is.null(object$chromstart) & is.null(object$chromend)) {
                object$chromstart <- 1
                object$chromend <- genome[[object$chrom]]
            }

            data <- bb_getExons(
                assembly = object$assembly,
                chromosome = object$chrom,
                start = object$chromstart,
                stop = object$chromend
            )
            xscale <- c(object$chromstart, object$chromend)
        }
    }

    objectInternal$xscale <- xscale
    objectInternal$data <- data
    return(list(object, objectInternal))
}

## Define a function that will by default prioritize genes by citations
# (if available) or gene lengths
# @param data data frame of gene or transcript data
# @param assembly bb_assembly associated with gene data
# @param transcsript a logical indicating whether or not we're
# plotting transcripts or not
defaultGenePriorities <- function(data, assembly, transcript = FALSE) {

    ## Define our list of available defaults that have citations
    availCitations <- list(
        bosTau8 = "Citations.Btaurus.NCBI.bosTau8",
        bosTau9 = "Citations.Btaurus.NCBI.bosTau9",
        canFam3 = "Citations.Cfamiliaris.NCBI.canFam3",
        ce11 = "Citations.Celegans.NCBI.ce11",
        danRer10 = "Citations.Drerio.NCBI.danRer10",
        danRer11 = "Citations.Drerio.NCBI.danRer11",
        dm3 = "Citations.Dmelanogaster.NCBI.dm3",
        dm6 = "Citations.Dmelanogaster.NCBI.dm6",
        galGal4 = "Citations.Ggallus.NCBI.galGal4",
        galGal5 = "Citations.Ggallus.NCBI.galGal5",
        galGal6 = "Citations.Ggallus.NCBI.galGal6",
        hg18 = "Citations.Hsapiens.NCBI.hg18",
        hg19 = "Citations.Hsapiens.NCBI.hg19",
        hg38 = "Citations.Hsapiens.NCBI.hg38",
        mm9 = "Citations.Mmusculus.NCBI.mm9",
        mm10 = "Citations.Mmusculus.NCBI.mm10",
        rheMac3 = "Citations.Mmulatta.NCBI.rheMac3",
        rheMac8 = "Citations.Mmulatta.NCBI.rheMac8",
        rehMac10 = "Citations.Mmulatta.NCBI.rheMac10",
        panTro5 = "Citations.Ptroglodytes.NCBI.panTro5",
        panTro6 = "Citations.Ptroglodytes.NCBI.panTro6",
        rn4 = "Citations.Rnorvegicus.NCBI.rn4",
        rn5 = "Citations.Rnorvegicus.NCBI.rn5",
        rn6 = "Citations.Rnorvegicus.NCBI.rn6",
        sacCer2 = "Citations.Scerevisiae.NCBI.sacCer2",
        sacCer3 = "Citations.Scerevisiae.NCBI.sacCer3",
        susScr3 = "Citations.Sscrofa.NCBI.susScr3",
        susScr11 = "Citations.Sscrofa.NCBI.susScr11"
    )

    ## Define assemblies whose TxDb IDs will need to be converted to
    ## ENTREZID from a different ID
    convertIDs <- list(
        dm3 = "ENSEMBL", dm6 = "ENSEMBL",
        rn4 = "ENSEMBL", sacCer2 = "ORF",
        sacCer3 = "ORF"
    )

    assemblyName <- assembly$Genome
    ## If assembly is included in package, access citations
    if (any(names(availCitations) %in% assemblyName)) {
        name <- names(availCitations)[which(
            names(availCitations) %in% assemblyName
        )]

        ## Convert necessary builds to ENTREZID
        if (name %in% names(convertIDs)) {

            ## Load associated OrgDb
            org_db <- eval(parse(text = assembly$OrgDb))

            ## Convert gene ids in data to ENTREZID based on previous keytype
            entrezIDs <- suppressMessages(
                AnnotationDbi::select(org_db,
                    keys = data$GENEID,
                    columns = "ENTREZID",
                    keytype = convertIDs[[name]]
                )
            )

            data$ENTREZID <- entrezIDs
        } else {
            data$ENTREZID <- data$GENEID
        }

        ## Get internal citation data and match based on ENTREZID
        citationData <- eval(parse(text = availCitations[[name]]))
        updatedData <- suppressMessages(dplyr::left_join(
            x = data,
            y = citationData,
            by = "ENTREZID"
        ))

        ## Set any missing citations to 0
        updatedData[is.na(updatedData$Citations), ]$Citations <-
            rep(0, nrow(updatedData[is.na(updatedData$Citations), ]))

        if (transcript == TRUE) {
            updatedData <-
                updatedData[duplicated(updatedData$TXNAME) == FALSE, ]
        }


        ## Order based on citation number
        updatedData <- updatedData[order(updatedData$Citations,
            decreasing = TRUE
        ), ]
    } else {

        ## With no internal citation data, set default priority
        ## based on gene/transcript length
        updatedData <- data[order(data$length, decreasing = TRUE), ]
    }

    ## Return data with priorities
    return(updatedData)
}

## Define a function that determines the chromosome offsets for a plot
## with multiple chromosomes (manhattan plots)
# @param assemblyData data.frame of an assembly's chromosomes and lengths
# @param space the space between each chrom as a fraction of plot width
spaceChroms <- function(assemblyData, space) {

    ## Determine the offset for each chromomse
    cumsums <- cumsum(as.numeric(assemblyData[, 2]))
    spacer <- cumsums[length(cumsums)] * space
    additionalSpace <- (seq(1, length(cumsums) - 0)) * spacer

    ## Start position
    startPos <- c(0, cumsums[seq(1, length(cumsums) - 1)])
    startPos <- startPos + additionalSpace
    assemblyData[, 3] <- startPos

    ## Stop Position
    stopPos <- cumsums + (seq(1, (length(cumsums)))) * spacer
    assemblyData[, 4] <- stopPos

    colnames(assemblyData) <- c("chrom", "length", "start", "stop")

    return(assemblyData)
}
