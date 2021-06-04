# Define a function to read in various kinds of genomic range data
#' @importFrom plyranges %>%
read_rangeData <- function(data, assembly, chrom = NULL, 
                        start = NULL, end = NULL){
    
    if (!"data.frame" %in% class(data)) {
        if (!"GRanges" %in% class(data)) {
            if (file_ext(data) == "bam") {
                indexFile <- paste0(data, ".bai")
                if (!file.exists(indexFile)) {
                    stop("Cannot read in bam file without a ",
                        "corresponding bam index file (.bai) in the ",
                        "same directory.", call. = FALSE)
                }
                data <- plyranges::read_bam(data) %>%
                    plyranges::filter_by_overlaps(GenomicRanges::GRanges(
                        seqnames = chrom,
                        ranges = IRanges::IRanges(
                            start = start,
                            end = end
                        )
                    )) %>%
                    dplyr::mutate()
            } else if (file_ext(data) %in% c("bw", "bigWig",
                                            "bigwig", "bedgraph")) {

                data <- bb_readBigwig(
                    file = data,
                    chrom = chrom,
                    chromstart = start,
                    chromend = end
                )
            } else {
                data <- data.table::fread(data)
            }
        } else {
            
            ## check GRanges genome with assembly input
            checkAssemblyMatch(data = data, assembly = assembly)
            
        }
    }
    
    data <- as.data.frame(data)
    return(data)
    
}

# Define a function to read in various kinds of genomic paired range data
read_pairedData <- function(data, assembly, warning = FALSE){
    
    if (!"data.frame" %in% class(data)) {
        if (!isClass("GInteractions", data)) {
            data <- as.data.frame(data.table::fread(data))
        } else {
            ## Reorder GInteractions columns
            data <- as.data.frame(data)
            dataSubset <- data[, c(
                "seqnames1", "start1", "end1",
                "seqnames2", "start2", "end2"
            )]
            
            data <- data[, which(!colnames(data) %in%
                                colnames(dataSubset))]
            data <- cbind(dataSubset, data)
            
            ## check GInteractions genome with assembly input
            checkAssemblyMatch(data = data, assembly = assembly)
                
        }
    } else {
        data <- as.data.frame(data)
    }
    
    if (warning == TRUE){
        if (nrow(data) < 1){
            warning("\'data\' input contains no values.", call. = FALSE)
        }
        
    }
    
    return(data)
    
    }
    
# Define a function that checks matching for GRanges/GInteractions
# objects and declared BentoBox assembly
checkAssemblyMatch <- function(data, assembly){
    
    genome <- unique(GenomeInfoDb::genome(data))
    if (!is.na(genome)){
        if (genome != assembly$Genome){
            warning("Input data assembly detected as ",
                    genome, " and BentoBox assembly ",
                    "detected as ", assembly$Genome, ".",
                    .call = FALSE)
        }
    }    
}