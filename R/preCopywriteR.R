<<<<<<< HEAD
preCopywriteR <- function(black.GC.mapa.folder, output.folder, bin.size,
                       reference) {

    ## Make folder path absolute
    black.GC.mapa.folder <- tools::file_path_as_absolute(black.GC.mapa.folder)
    output.folder <- tools::file_path_as_absolute(output.folder)

    ## Checks
    # Check whether folders exist.
if (!(reference == "hg19" || reference == "mm10" || reference == "mm9")) {
        stop("The reference is not recognised. Please provide a suitable",
             "reference.")
    }

    if (file.exists(black.GC.mapa.folder) == FALSE) {
        stop("The annotation folder could not be found. Please change your",
             "black.GC.mapa.folder path.")
    } else {
        cat("Reference folder", black.GC.mapa.folder, "detected", "\n")
    }

    if (file.exists(output.folder) == FALSE) {
        stop("The output.folder could not be found. Please change the output",
             "folder path.")
    } else {
        cat("Reference folder", output.folder, "detected", "\n")
=======
preCopywriteR <- function(output.folder, bin.size, ref.genome, prefix = "") {

    ## Make folder path absolute
    output.folder <- tools::file_path_as_absolute(output.folder)

    ## Checks

    # Check the existence of the output folder
    if (file.exists(output.folder) == FALSE) {
        stop(.wrap("The output folder could not be found. Please change the",
                   "path specified in", sQuote(output.folder)))
    } else {
        cat(.wrap("The output folder", sQuote(output.folder), "has been",
                  "detected"), "\n")
>>>>>>> master
    }

    # Check if bin.size is factor of 1kb
    if (!.is.wholenumber(bin.size/1000)) {
<<<<<<< HEAD
        stop("Please provide a bin.size which is a multiple of 1000.")
    }

    ## Generate files with desired bin.size (MAPA, GC, bed, blacklist)
    # Load blacklist, CG-content and mapability files
    load(file.path(black.GC.mapa.folder, "mapability.rda"))
    load(file.path(black.GC.mapa.folder, "GCcontent.rda"))
    bed_file <- read.table(file.path(black.GC.mapa.folder, "blacklist.bed"),
                           as.is = TRUE, sep = "\t")
=======
        stop(.wrap("Please provide a bin size which is a multiple of 1000."))
    }

    ## Generate files with desired bin.size (mappa, GC, bed, blacklist)
    # Pre-define GC.mappa.grange and blacklist.grange variables to avoid them
    # from raising NOTES during R CMD CHECK (variables are loaded from file
    # below)
    GC.mappa.grange <- NULL
    blacklist.grange <- NULL
    
    # Load GC.grange, GC.mappa.grange and blacklist.grange variables
    black.GC.mappa.folder <- getPathHelperFiles(ref.genome)
    load(file.path(black.GC.mappa.folder, "GC_mappability.rda"))
    load(file.path(black.GC.mappa.folder, "blacklist.rda"))
>>>>>>> master

    # Create bins with desired bin size
    MERGEBINNUMBER <- bin.size/1000

<<<<<<< HEAD
    newBin <- NULL
    options(warn = -1)
    options(scipen = 999)
    for (chr in unique(mapa$chromosome)) {
        col2 <- colMins(matrix(mapa$start[mapa$chromosome == chr],
                               nrow = MERGEBINNUMBER))
        col3 <- colMaxs(matrix(mapa$end[mapa$chromosome == chr],
                               nrow = MERGEBINNUMBER))
        tmp <- cbind(chr, col2, col3)
        tmp <- tmp[1:(nrow(tmp) - 1), ]
        newBin <- rbind(newBin, tmp)
    }
    options(scipen = 0)
    options(warn = 0)

    cat("Generated", bin.size, "bp bins for all chromosomes", "\n")

    # Create mapabillity file with desired bin size
    MERGEBINNUMBER <- bin.size/1000

    newMapa <- NULL
    options(warn = -1)
    options(scipen = 999)
    for (chr in unique(mapa$chromosome)) {
        col2 <- colMins(matrix(mapa$start[mapa$chromosome == chr],
                               nrow = MERGEBINNUMBER))
        col3 <- colMaxs(matrix(mapa$end[mapa$chromosome == chr],
                               nrow = MERGEBINNUMBER))
        col4 <- colMeans(matrix(mapa$mapability[mapa$chromosome == chr],
                                nrow = MERGEBINNUMBER))
        tmp <- cbind(chr, col2, col3, col4)
        tmp <- tmp[1:(nrow(tmp) - 1), ]
        newMapa <- rbind(newMapa, tmp)
    }
    options(scipen = 0)
    options(warn = 0)

    cat("Generated mapability file for bin size of", bin.size, "bp", "\n")

    # Create GC-content file with desired bin size
    newGC <- NULL
    options(warn = -1)
    options(scipen = 999)
    for (chr in unique(mapa$chromosome)) {
        col2 <- colMins(matrix(GC$start[GC$chromosome == chr],
                               nrow = MERGEBINNUMBER))
        col3 <- colMaxs(matrix(GC$end[GC$chromosome == chr],
                               nrow = MERGEBINNUMBER))
        col4 <- colMeans(matrix(GC$ATcontent[GC$chromosome == chr],
                                nrow = MERGEBINNUMBER))
        col5 <- colMeans(matrix(GC$GCcontent[GC$chromosome == chr],
                                nrow = MERGEBINNUMBER))
        tmp <- cbind(chr, col2, col3, col4, col5)
        tmp <- tmp[1:(nrow(tmp) - 1), ]
        newGC <- rbind(newGC, tmp)
    }
    options(scipen = 0)
    options(warn = 0)

    cat("Generated GC-content file for bin size of", bin.size, "bp", "\n")

    ## Create folder for output files
    file_name <- paste0(reference, "_", bin.size/1000, "kb/")
    dir.create(file.path(output.folder, file_name))
    if (file.exists(file.path(output.folder, file_name)) == FALSE) {
        stop("No output folder created, please check argument output.folder ",
             "and the corresponding folder permissions.")
=======
    # Create bins
    custom.bin <- data.frame()
    
    old.options <- options()
    on.exit(options(old.options))
    options(warn = -1, scipen = 999)
    
    for (chr in seqlevels(GC.mappa.grange)) {
        selection <- as(seqnames(GC.mappa.grange) == chr, "vector")
        start.bin <- colMins(matrix(start(GC.mappa.grange)[selection],
                                    nrow = MERGEBINNUMBER))
        end.bin <- colMaxs(matrix(end(GC.mappa.grange)[selection],
                                  nrow = MERGEBINNUMBER))
        ATcontent.bin <- colMeans(matrix(GC.mappa.grange$ATcontent[selection],
                                         nrow = MERGEBINNUMBER))
        GCcontent.bin <- colMeans(matrix(GC.mappa.grange$GCcontent[selection],
                                         nrow = MERGEBINNUMBER))
        mappability.bin <- colMeans(matrix(GC.mappa.grange$mappability[selection],
                                           nrow = MERGEBINNUMBER))
        chr.bin <- cbind(seqnames = paste0(prefix, chr), start = start.bin,
                         end = end.bin, ATcontent = ATcontent.bin,
                         GCcontent = GCcontent.bin,
                         mappability = mappability.bin)
        chr.bin <- chr.bin[seq_len(nrow(chr.bin) - 1), ]
        custom.bin <- rbind(custom.bin, chr.bin)
    }
    custom.bin[, 2:ncol(custom.bin)] <-
        apply(custom.bin[, 2:ncol(custom.bin)], c(1, 2), as.numeric)
    custom.bin[, 4:ncol(custom.bin)] <-
        apply(custom.bin[, 4:ncol(custom.bin)], c(1, 2), round, 3)
    GC.mappa.grange <- makeGRangesFromDataFrame(custom.bin,
                                                keep.extra.columns = TRUE)
    
    cat(.wrap("Generated GC-content and mappability data at", bin.size, "bp",
              "resolution..."), "\n")
    
    # Generate blacklist object
    blacklist.grange <- as(blacklist.grange, "data.frame")
    blacklist.grange$seqnames <- paste0(prefix, blacklist.grange$seqnames)
    blacklist.grange <- makeGRangesFromDataFrame(blacklist.grange)
    cat(.wrap("Generated blacklist file..."), "\n")

    ## Create folder for output files
    file.name <- paste0(ref.genome, "_", bin.size/1000, "kb",
                        if (prefix != "") {paste0("_", prefix)})
    dir.create(file.path(output.folder, file.name))
    if (file.exists(file.path(output.folder, file.name)) == FALSE) {
        stop(.wrap("The output folder could not be created; please check that",
                   "you have permission to write in", sQuote(output.folder)))
>>>>>>> master
    }

    ## Write files to folder
    # Blacklist
<<<<<<< HEAD
    write.table(bed_file, file = file.path(output.folder, file_name,
                                           "blacklist.bed"),
                quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
    # GC-content
    write.table(newGC, file = file.path(output.folder, file_name,
                                        "GC_content.bed"),
                quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
    # Mapability
    write.table(newMapa, file = file.path(output.folder, file_name,
                                          "mapability.bed"),
                quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
    # bed file with bins
    write.table(newBin, file = file.path(output.folder, file_name,
                                         "bins.bed"),
                quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
=======
    save(blacklist.grange, file = file.path(output.folder, file.name,
                                            "blacklist.rda"), compress = "xz")
    # GC-content and mappability
    save(GC.mappa.grange,
         file = file.path(output.folder, file.name, "GC_mappability.rda"),
         compress = "xz")
>>>>>>> master

}