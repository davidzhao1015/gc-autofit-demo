###########################################################################################
## Main program for the Automation GC-MS profiling 
## Author: Beomsoo Han and Jack Liang
## Usage: Rscript apgcms_main.R <input CDF files directory> <sample type: 1-Serum, 2-Organic Acid> 
##               <Alkane Start Cn (e.g., 11)> <number of Alkane (eg, 11 or 20)
## 
# input: sample.D files (usually same batch including alkane standard)
# final output: sample.profiled.tsv (tab separated file)
#   - compound name, concentration, Scores (MF, R.MF, Prob)
# temporary output:
#   - peak #, RT(min), RI, Intensity, RT range, Area, 
## 
## Analysis Steps:
## 1. upload CDF files with directory information
## 2. create temporary directory
## 3. analysis and generate report file
##    3.1 call libraries including RI/mz, functions, calibration ( use absolute directory. eg ".\lib" )
##    3.2 run main script; each procedure
##      A. Peak detection
##      B. Quantification
##      C. Report Generation
## 4. download link with the report file (.csv)
############################################################################################

#!/usr/bin/Rscript
options(echo=FALSE) # if you want see commands in output file
options("width"=120)
options(warn = -1) # ignore the warnings

library(parallel) # for using multiple cores

## argument check for standalone version
##===========================================================================================
## Ref: http://www.r-bloggers.com/parse-arguments-of-an-r-script/

args <- commandArgs(trailingOnly = FALSE)
# args <- commandArgs(TRUE)
# cat("length(args):", length(args),"\n")
# cat("argsL:\n"); print(argsL)

# get program directory
RProgram_dir <- normalizePath(dirname(sub("^--file=", "", args[grep("^--file=", args)])))
lib_dir <- file.path(RProgram_dir, "lib")

source( file.path(RProgram_dir, "envVars.cfg") )  ## loading packages, libraries, and definitions
source( file.path(lib_dir, libfunc.SetOpts.file) )  ## loading packages, libraries, and definitions

showProgramInfo(Version)

if (length(args) == 5) {  args <- c("--help") } 
if ("--help" %in% args) {  helpMessage()  }

## parsing input arguments
parsingArgument(args)
# cat("\n## AFTER PARSING ARGs\n")

## create and set working directory (save output/result files) 
## may be changed: upload_user_temp_dir \data, \Result
dir.create(user.outdir, showWarnings=FALSE, recursive=TRUE) # expected full path
dirProfileResult <- normalizePath(user.outdir)
setwd(dirProfileResult)  ## Set working directory    

if(ISDEBUG) cat("## dirProfileResult:"); print(dirProfileResult)

if( USE_INTERNAL_LIBRARY == 'NONE') {    
    lib.peak <- getLibInfo(userLibFile)
    lib.calicurv <- getLibInfo(userCalFile)
} else{  
    SampleType <- setSampleType(USE_INTERNAL_LIBRARY)
    
    ## load libraries (profiling and calibration curve)
    fname.lib.peak <- file.path(file.path(lib_dir, LibFile[SampleType]))
    lib.peak <- getLibInfo(fname.lib.peak)
    
    fname.lib.calicurve <- file.path(file.path(lib_dir, LibCaliCurveFile[SampleType]))
    lib.calicurv <- getLibInfo(fname.lib.calicurve)
    
    ## making Subset of Internal Library 
    ## if USER_SELECTED_CMPDS is not NULL (user selected some compounds of internal library)
    if (! is.null(USER_SELECTED_CMPDS)) {
        userSelectedCmpdList <- unlist(strsplit(USER_SELECTED_CMPDS, ","))
        # cat("\n\n# lib.peak:\n"); print(as.character(lib.peak$HMDB_ID)) 
        # cat("\n\n# lib.calicurv:\n"); print(as.character(lib.calicurv$HMDB_ID))
        lib.peak <- lib.peak[which(as.character(lib.peak$HMDB_ID) %in% userSelectedCmpdList), ]
        lib.calicurv <- lib.calicurv[which(as.character(lib.calicurv$HMDB_ID) %in% userSelectedCmpdList), ]
    }
}  

# print(lib.peak[,c(1:3)])
# print(lib.calicurv)
# stop()

## load library for the functions
source( file.path(lib_dir, libfunc.file) )  ## loading packages, libraries, and definitions
source( file.path(lib_dir, libfunc.Alkane.file) )  ## loading packages, libraries, and definitions
source( file.path(lib_dir, libfunc.JSON.file) )  ## loading packages, libraries, and definitions

# get Internal Standard Compound Name using input internal Std (compound name or HMDB ID)
if (internalStd.in == "NONE")  {
    cat("\n\n## Internal Standard was not assigned. Proceed to profilie the compound only! \n")
    internalStd <- "NONE"
} else {  
    internalStd <- getInternalStdCmpdName(lib.peak, internalStd.in)
    # cat("internalStd:"); print(internalStd.in)
    if ( is.null(internalStd) ) {
        cat("\n\n## Error: Cannot find a matched Internal Standard [", internalStd.in,"] in the library\n")
    } else {
        cat("\n\n## Internal Standard [", internalStd.in,"] is in the library\n")
    }
}

## set Debug Mode (TRUE / FALSE)
setDebugMode(ISDEBUG);  

## Get Sample mzXML/CDF file list from the input directory
## return: infile.alkane/blank/samples
fileList <- get_file_list(sampleFileDir, processOption)
if (DEBUG) { cat("Input Files:\n"); print(fileList) }

## read raw data from spectra file (.CDF) to get peak, EIC
## xset_list <- extract_xset_list(file_vec, mzStep) # xset_list$alkane & xset_list&samples 
cat("\n## [", processOption, "] Extracting Raw data ...\n")


####################################################################################
# in Preprocessing
# Alkane Std, Blank, and Spectrum plot of Samples
####################################################################################

if (processOption == 'PREPROCESSING') {
  
    ## =======================================================================================
    ## Alkane Peak Handling (Profiling & Spectrum Plot)
    ## =======================================================================================
  
  
    source( file.path(lib_dir, libfunc.Alkane.file) )  ## loading packages, libraries, and definitions

    # Alkane Peak Profiling
    fname.lib.alkane <- file.path(lib_dir, LibFile.Alkane)
    
    # peak_alkane_std <- do_AlkanePeakProfile(fname.lib.alkane, fileList$alkaneFile, setAdjustAlkanePeakCn=FALSE, userDefined.Cn=FALSE, userEstAlkaneRT==TRUE) 
    peak_alkane_std <- do_AlkanePeakProfile(fname.lib.alkane, fileList$alkaneFile, setAdjustAlkanePeakCn=IS_AlkanePeakCnAdjust, userDefined.Cn=FALSE) 

    if(DEBUG) { cat("final alkane profiled:\n"); print(peak_alkane_std) }
    alkaneInfo <- check_alkane_std(peak_alkane_std)

    ## =======================================================================================
    ## Blank Sample Handling
    ## =======================================================================================
    
    ## @@ blank has something serious noise; so if we substract it from sample the result may be wrong
    if( USE_BLANK_SPECTRUM & length(fileList$blankFile) > 0 ) {
        ###### make a single function as Alkane
        xset.blank <- extractBlankInfo(fileList$blankFile)  
        specFilename.blank <- sub(".mzXML|.CDF","", basename(fileList$blankFile), ignore.case = TRUE)
        
        cat("\n## Extracting peak list for Blank sample:", basename(fileList$blankFile), "\n")
        ## peak picking for samples using EIC(extracted ion chromatograms) for m/z values of interest. 
        # peaks.blank <- extract_peak_list_blank(xset.blank, ctype="TIC", offset=1.5, RunPlotOnly)
        peaks.blank <- extract_peak_list_blank(xset.blank, ctype="TIC", offset=1.5)

        ## RI calculation using Alkane Std
        peak_blank_ri <- get_RI_for_samples2(peaks.blank, peak_alkane_std)    
        profiled_peaks_blank <- compoundIdentify3(peak_blank_ri, xset.blank, lib.peak, alkaneInfo, RI.Variation, isBLANK=TRUE)
        # cat("\n\n## profiled_peaks_blank\n"); print(head(profiled_peaks_blank))
        colnames(profiled_peaks_blank)[1] <- "HMDB_ID"

        if( DEBUG ) {
            ofilename <- paste(specFilename.blank,"_profiledPeaksTMP.tsv", sep='')
            write.table(profiled_peaks_blank, file=ofilename, quote=TRUE, row.names=FALSE, sep="\t")
        }
        
        final_PeakProfile_blank <- arrangeProfiledPeaks2(profiled_peaks_blank)
        if (DEBUG) { cat("\n\n## final_PeakProfile_blank\n"); print(head(final_PeakProfile_blank)) }

        #!!!! should be included in arrangeProfiledPeaks2
        final_PeakProfile_blank <- final_PeakProfile_blank[which(final_PeakProfile_blank$MatchFactor > MF_THRESHOLD), ]
        
        final_PeakProfile_blank.json <- cbind(final_PeakProfile_blank[, c("HMDB_ID","CompoundWithTMS","RT_min","RI","Intensity",
                                                        "mzMaxInts","MatchFactor","TScore","Area","RT.start","RT.end","mz","mzInt")], 
                                                        Concentration="NA") # keep mass Spectrum Info (m/z, Intensity)
        final_PeakProfile_blank <- cbind(final_PeakProfile_blank[,c("HMDB_ID", "CompoundWithTMS", "RT_min","RT","RI","Intensity",
                                                                    "mzMaxInts","MatchFactor", "RI.Similarity","Area","RT.start","RT.end")], 
                                         Concentration="NA")
        ofilename <- paste(specFilename.blank,"_profiled.csv", sep='')
        outColnames <- c("HMDB_ID", "CompoundWithTMS", "RT_min","RT","RI","Intensity",
                         "Irons","MatchFactor", "RI.Similarity","Area","RT.start","RT.end","Concentration")
        write.table(final_PeakProfile_blank, file=ofilename, quote=TRUE, row.names=FALSE, col.names=outColnames, sep=",")

        if (CREATE_JSON_FILE) {
            ofilename <- paste(specFilename.blank,"_spectrum.json", sep='')
            create_json_file(ofilename, round(xset.blank@scantime/60,3), xset.blank@tic, final_PeakProfile_blank.json)
        }
                
        # rmCompoundStr <- ifelse(SampleType %in% c(SERUM,SALIVA), 'Ribitol', 'Cholesterol')     
        if( (internalStd != "NONE") && !is.null(internalStd) ) {
              rmCompoundStr <- internalStd
              final_PeakProfile_blank <- final_PeakProfile_blank[- which(final_PeakProfile_blank$Compound == rmCompoundStr), ]
              
              if( nrow(final_PeakProfile_blank) == 0  ) {
                  # stop("There is no Internal Standard in the Blank file")
                  cat("## Warning: There is no Internal Standard in Blank.\n## >> Running without Blank ! \n")
                  xset.blank <- NULL
                  USE_BLANK_SPECTRUM <- FALSE
              }
        }

    } else {
        cat("\n## Blank Sample is not found/used\n")
        xset.blank <- NULL
        USE_BLANK_SPECTRUM <- FALSE
        final_PeakProfile_blank <- NULL
    }
    
    save(alkaneInfo, file="alkaneInfo.Rdata")
    save(peak_alkane_std, file="alkanePeakInfo.Rdata")
    save(final_PeakProfile_blank, file="blankInfo.Rdata")
        
} else {
    ## for Quantification Process, loading preprocessed information from saved files
    cat("\n##### Loading Preprocessed Information Files \n")
    infoFileDir.alkane <- paste(infoFileDir, "/alkaneInfo.Rdata", sep='')
    # cat(infoFileDir.alkane,"\n")
    file.exists(infoFileDir.alkane)
    
    infoFileDir.alkanePeak <- paste(infoFileDir, "/alkanePeakInfo.Rdata", sep='')
    # cat(infoFileDir.alkanePeak,"\n")
    file.exists(infoFileDir.alkanePeak)
    
    infoFileDir.blank <- paste(infoFileDir, "/blankInfo.Rdata", sep='')
    # cat(infoFileDir.blank,"\n")
    file.exists(infoFileDir.blank)
    
    load(infoFileDir.alkane)
    load(infoFileDir.alkanePeak)
    load(infoFileDir.blank)
    
    if (DEBUG) {
        cat("\n\n##### Check alkane & blank #####\n\n")
        cat("## alkaneInfo:\n"); print(alkaneInfo);
        cat("\n## peak_alkane_std:\n"); print(peak_alkane_std);
        cat("\n## final_PeakProfile_blank:\n"); print(final_PeakProfile_blank);
    }
}

## =======================================================================================
## peak identifying for samples
## =======================================================================================
cat("\n## Processing Sample Spectra Files ...\n")
# print(head(lib.peak[,c(1:5)]))

HMDBID <- as.character(unique(lib.peak[, c('HMDB_ID')]))
cmpdlist <- as.data.frame( lib.peak[, c("SeqIndex","HMDB_ID","Compound",'CompoundWithTMS')] )
# cmpdlist

final.Concentration <- NULL
nCores <- detectCores() - 1  # get the number of cores, -1 for other use

## with Preprocessing Option
## Alkane & Blank & Plots
if ( processOption == "PREPROCESSING" ) {
    ## ALKANE
    ## BLANK
  
    ## Image Plot generation      
    cat("\n## Spectrum plot generation only ...\n")

    # Multi Core or Single Core => now 2015 10 05, only sing core
    generateSpectrumPlot(fileList$sampleFiles)
    if (FALSE) {
        if (nCores >= 2) {
            if (DEBUG) cat("\t# N Core:", nCores, "\n")
            # beg = Sys.time()
            # generateSpectrumPlot.multicore(nCluster=nCores, fileList$sampleFiles, RunPlotOnly)
            generateSpectrumPlot.multicore(nCluster=nCores, fileList$sampleFiles)
            # td = as.numeric(Sys.time() - beg, "secs")
            # cat("\n\n## Time - Multi Core:", td,"\n")
        } else { 
            # beg = Sys.time()
            # generateSpectrumPlot(fileList$sampleFiles, RunPlotOnly)
            generateSpectrumPlot(fileList$sampleFiles)
            # td = as.numeric(Sys.time() - beg, "secs")
            # cat("\n\n## Time - Single Core:", td,"\n")
        }
    }

} else {
    ## PROFILING & Quantification as optional
  
    ## Loading the Generated Alkane & Blank Profile Information in PREPROCESSING 
    cat("\n## Profiling and quantifying for each sample...\n")

    checkInternalStd <- FALSE    
    if (DEBUG) { beg = Sys.time() }
   
    # For supporting multiple cores. However, it was changed because process as queue structure
    if ( FALSE ) {
        # if(DEBUG) cat("\n\n## Profiling with multi cores\n")
        # flag.blank_spectrum <- ifelse( USE_BLANK_SPECTRUM, TRUE, FALSE)
        # conc.each <- quantification.multicore(nCores, fileList$sampleFiles, use.blank=flag.blank_spectrum, threshold.matchFactor=MF_THRESHOLD, 
        #                              internalStd=internalStd, lib.calicurv=lib.calicurv, cmpdlist=cmpdlist, final_PeakProfile_blank=final_PeakProfile_blank)  
        # if(DEBUG) cat("## Profiling for each sample -- Done\n")
    } else {
        if(DEBUG) cat("\n\n## Profiling with single core\n")
        conc.each <- lapply(fileList$sampleFiles, quantifictionFunc, print.on=TRUE, 
                            use.blank=USE_BLANK_SPECTRUM, threshold.matchFactor=MF_THRESHOLD, 
                            internalStd=internalStd, lib.calicurv=lib.calicurv, cmpdlist=cmpdlist, 
                            final_PeakProfile_blank=final_PeakProfile_blank)        
    }
   
    if (DEBUG) { 
        cat("\n\n# [Profiling] Each concentration table:\n"); print(conc.each) 
        td = as.numeric(Sys.time() - beg, "secs")
        if (DEBUG && nCores > 1) {
            cat("## Running Time with multiple Cores (", nCores,") :", td,"\n")
        }
    }
    
    ## merge concentration for all samples
    if (length(conc.each) >= 2) {
        final.Concentration <- mergeConcTable( conc.each )
    } else {
        # final.Concentration <- conc.each;
        final.Concentration <- as.data.frame(conc.each);
    }

    ## Collect concentrations only and combine all samples
    ## ==================================================================================================
    if (TRUE) {        
        ## Combining  all concentration and Generate Files for Concentration Table with Sample ID
        ## use the "final.Concentration" data frame
        if ( (internalStd != 'NONE') && !is.null(internalStd) ) {
            # ofile.merged <- paste(basename(sampleFileDir),"_profiledAll.csv", sep='')
            ofile.merged <- "profiled_All.csv"
            cat("\n## Generate Files for Concentration Table of All Samples:", ofile.merged, "\n")
            ## order by RI as in library; merge compound name with RI and sort by RI

            final.Concentration <- merge(lib.calicurv[,c('HMDB_ID','Compound','SeqIndex')], final.Concentration, by=c('HMDB_ID', 'Compound'), sort=FALSE, all=TRUE)   
            # cat("\n ### final.Concentration:\n"); print(final.Concentration)
            final.Concentration <- final.Concentration[order(as.integer(as.character(final.Concentration$SeqIndex)), decreasing=FALSE), ]
            # cat("\n ### final.Concentration (sort):\n"); print(final.Concentration)
                    
            rm.SeqIndex <- which(names(final.Concentration) == "SeqIndex")
            final.Concentration <- final.Concentration[ , - rm.SeqIndex]
            rownames(final.Concentration) <- c(1:nrow(final.Concentration))
            
            if( DEBUG ) { cat("final.concentration:\n"); print(final.Concentration)  }
            
            write.table( t(final.Concentration), file=ofile.merged, sep=",", col.names=FALSE, quote=TRUE)
            cat("\n==============================================================\n")
            cat("## Done:", length(fileList$sampleFiles), "spectrum files\n")
            cat("==============================================================\n\n")
            
        } else {
            ## OPTION or if no internal std, then just report without quantification 
            ostr <- paste("\n\n################################################################################\n",
                          "## NOTE: The profiled_ALL file was not generated due to missing internal Standard\n", 
                          "\t internalStd:", internalStd.in,"\n",
                          "################################################################################\n", sep='')
            ofile.merged <- "profiled_All.csv"
            cat(ostr, file=ofile.merged)            
            cat(ostr)
        }
    }
    
    ofile.note <- paste(basename(sampleFileDir),"_note.txt", sep='')
    saveProfilingInfo(ofile.note, Version, MF_THRESHOLD)   
}
