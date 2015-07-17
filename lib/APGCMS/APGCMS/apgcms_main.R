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

## update?
# threshold for each compound? or use different with respond to intensities
library(parallel)


#!/usr/bin/Rscript
options(echo=FALSE) # if you want see commands in output file
options("width"=120)

library(parallel) # for using multiple cores

ISDEBUG <- TRUE
IS_PRINT_MZINT4DB <- FALSE
IS_AlkanePeakCnAdjust <- TRUE 
## sample type and library --> will be gone
RI.VARIATION.DEFAULT <- 0.03 # 0.03 default

## R Script program directory
Version <- "1.0 released 20150521"
CREATE_JSON_FILE <- TRUE
# USE_BLANK_SPECTRUM <- FALSE
MF_THRESHOLD_DEFAULT <- 500 

## now in the library, it includes RI, mz, intensity, 
## as well as slope and intercept for quantification (calibration curve)
libfunc.file <- "apgcms_funclib.R"
libfunc.Alkane.file <- "apgcms_funclib_alkane.R"
libfunc.SetOpts.file <- "apgcms_funclib_setopts.R"

LibFile.Alkane <- "lib_alkane_20150324.csv"

# LibFile <- c("lib_serum_20140619.csv","lib_organicAcids_20140630.csv","lib_saliva_20140619.csv","lib_milk_20140722.csv") 
# Milk: from Serum because of same Internal Standard 
# LibFile <- c("lib_serum_20141211.csv","lib_urine_20141113.csv","lib_saliva_20140619.csv","lib_milk_20141211.csv") 
LibFile <- c("lib_serum_20150326.csv","lib_urine_20150326.csv","lib_saliva_20140619.csv","lib_milk_20150202.csv") 
LibCaliCurveFile <- c("lib_serum_CalibrationCurve_20150326.csv","lib_urine_CalibrationCurve_20150326.csv","lib_saliva_20140619.csv","lib_milk_20150202.csv") 


## argument check for standalone version
##===========================================================================================
## Ref: http://www.r-bloggers.com/parse-arguments-of-an-r-script/

args <- commandArgs(trailingOnly = FALSE)
# args <- commandArgs(TRUE)
# cat("length(args):", length(args),"\n")

# cat("argsL:\n"); print(argsL)
RProgram_dir <- normalizePath(dirname(sub("^--file=", "", args[grep("^--file=", args)])))
lib_dir <- file.path(RProgram_dir, "lib")

source( file.path(lib_dir, libfunc.SetOpts.file) )  ## loading packages, libraries, and definitions

showProgramInfo(Version)

if (length(args) == 5) {  args <- c("--help") } 
if ("--help" %in% args) {  helpMessage()  }

# NOT FUNCTION --> function 
# Define Global Variables
# Parsing arguments
{
  argsDF <- as.data.frame(do.call("rbind", parseArgs(args)))
  argsL <- as.list(as.character(argsDF$V2))
  names(argsL) <- argsDF$V1
  
  # cat("argsDF:\n"); print(argsDF);
  # cat("argsL:\n"); print(argsL);
  
  if(is.null(argsL$infiledir)) {
    showErrMessage("  Error in argument:\n\t There is no input spectrum file directory")
    helpMessage()
  } else {
    sampleFileDir <- normalizePath(argsL$infiledir) # should be absolute path
  }
  
  if(is.null(argsL$lib.internal)) {
    USE_INTERNAL_LIBRARY <- 'NONE' # argsL$lib.internal
    
    if(is.null(argsL$userlib)) {
      showErrMessage("  Error in argument:\n\t There is no user defined library file")
      helpMessage()
    } else {
      userLibFile <- argsL$userlib
    }
    
    if(is.null(argsL$usercal)) {
      showErrMessage("  Error in argument:\n\t There is no user defined calibration curve file")
      helpMessage()
    } else {
      userCalFile <- argsL$usercal
    }
    
  } else {
    # NONE, SERUM, URINE, SILIVA, ... default = none
    if (argsL$lib.internal %in% c('SERUM', 'URINE', 'SALIVA', 'MILK')) {
      USE_INTERNAL_LIBRARY <- argsL$lib.internal
    } else {
      showErrMessage("  Error in argument:\n\t see the help to correctly use the internal library option (lib.internal)")
      helpMessage()
    } 
  }
  
  if(is.null(argsL$internalstd)) {
    showErrMessage("  Error in argument:\n\t There is no internal standard compound. 
                          If you want profile only, then please set with 'NONE' ")
    helpMessage()
  } else {
    internalStd <- argsL$internalstd
  }
  
  if(is.null(argsL$plotonly)) {
    showErrMessage("  Warning in argument:\n\t plot only option is not assigned")
    helpMessage()
  } else {
    RunPlotOnly <- as.logical(argsL$plotonly)
  }
  
  # optional arguments
  if(is.null(argsL$useblank)) {
    # showErrMessage("  Warning in argument:\n\t Blank file option is not assigned. Program will use default (TRUE)")
    USE_BLANK_SPECTRUM <- TRUE
  } else {
    USE_BLANK_SPECTRUM <- as.logical(argsL$useblank)
  }
  
  if(is.null(argsL$MFscore)) {
    # showErrMessage("  Warning in argument:\n\t MFscore option is not assigned. Program will use default 400")
    MF_THRESHOLD <- MF_THRESHOLD_DEFAULT
  } else {
    MF_THRESHOLD <- as.numeric(argsL$MFscore)
  }
  
  if(is.null(argsL$RIoffset)) {
    # showErrMessage("  Warning in argument:\n\t RI offset option (RIoffset) is not assigned. Program will use default 0.03")
    RI.Variation <- RI.VARIATION.DEFAULT     
  } else {
    RI.Variation <- as.numeric(argsL$RIoffset)
  }
  
  if(is.null(argsL$AlkaneRT)) {
    user.AlkaneRT <-  NULL
  } else {
    user.AlkaneRT <- as.numeric(unlist(strsplit(argsL$AlkaneRT,",")))
    if(ISDEBUG) { cat("## user.AlkaneRT:"); print(user.AlkaneRT) }
  } 
}

if( USE_INTERNAL_LIBRARY == 'NONE') {    
    lib.peak <- getLibInfo(userLibFile)
    lib.calicurv <- getLibInfo(userCalFile)
} else{  
    SampleType <- setSampleType(USE_INTERNAL_LIBRARY)
    
    ## load libraries 
    {
      fname.lib.peak <- file.path(file.path(lib_dir, LibFile[SampleType]))
      lib.peak <- getLibInfo(fname.lib.peak)
      
      fname.lib.calicurve <- file.path(file.path(lib_dir, LibCaliCurveFile[SampleType]))
      lib.calicurv <- getLibInfo(fname.lib.calicurve)
    }
}  
# head(lib.peak)
# head(lib.calicurv)

## load library for the functions
source( file.path(lib_dir, libfunc.file) )  ## loading packages, libraries, and definitions
source( file.path(lib_dir, libfunc.Alkane.file) )  ## loading packages, libraries, and definitions
# cat("RProgram_dir:"); print(RProgram_dir)


## for testing/developing, Initiate variable and environment
##===========================================================================================
if(FALSE) {
  RProgram_dir <- "~/gcmsProfiling/APGCMS_developing"; 
  setwd(RProgram_dir); getwd()
  lib_dir <- file.path(RProgram_dir, "lib")
  source( file.path(lib_dir, libfunc.file) )  ## loading packages, libraries, and definitions    
  source( file.path(lib_dir, libfunc.Alkane.file) )  ## loading packages, libraries, and definitions
  
  sampleFileDir <- "~/gcmsProfiling/_data_spectrum/newSerumLib_Harding_20150225//Mix2_mzXML"
  
  SpectrumFile_list <- list.files(path = sampleFileDir);
  if (length(SpectrumFile_list) == 0) stop("No input CDF file exists")
  USE_INTERNAL_LIBRARY <- "SERUM"
  # set plottingStatus 
  plottingStatus <- 0 # 1 - plotting only; 0 - profiling
  RunPlotOnly <- ifelse (plottingStatus == 1, TRUE, FALSE) 
  MF_THRESHOLD <- MF_THRESHOLD_DEFAULT
  internalStd <- "Ribitol"
  user.AlkaneRT <-  NULL
  SampleType <- 1    
}

## set Debug Mode (TRUE / FALSE)
setDebugMode(ISDEBUG);  

## Get Sample mzXML/CDF file list from the input directory
## return: infile.alkane/blank/samples
fileList <- get_file_list(sampleFileDir)
if (DEBUG) { cat("Input Files:\n"); print(fileList) }
# stop()

## create and set working directory (save output/result files) 
## may be changed: upload_user_temp_dir \data, \Result
dirProfileResult <- paste(dirname(sampleFileDir), "/", sub("_input","",basename(sampleFileDir)), '_result', sep='')
dir.create(dirProfileResult, showWarnings=FALSE)
setwd(dirProfileResult)  ## Set working directory

## read raw data from spectra file (.CDF) to get peak, EIC
## xset_list <- extract_xset_list(file_vec, mzStep) # xset_list$alkane & xset_list&samples 
cat("\n## Extracting Raw data ...\n")

####################################################################################
# Alkane peak profiling 
####################################################################################
source( file.path(lib_dir, libfunc.Alkane.file) )  ## loading packages, libraries, and definitions

# Alkane Peak Profiling
fname.lib.alkane <- file.path(lib_dir, LibFile.Alkane)

# peak_alkane_std <- do_AlkanePeakProfile(fname.lib.alkane, fileList$alkaneFile, setAdjustAlkanePeakCn=FALSE, userDefined.Cn=FALSE, userEstAlkaneRT==TRUE) 
peak_alkane_std <- do_AlkanePeakProfile(fname.lib.alkane, fileList$alkaneFile, setAdjustAlkanePeakCn=IS_AlkanePeakCnAdjust, userDefined.Cn=FALSE) 

if(DEBUG) {cat("final alkane profiled:\n"); print(peak_alkane_std) }
alkaneInfo <- check_alkane_std(peak_alkane_std)


## =======================================================================================
## Blank Sample Handling
## =======================================================================================

## @@ blank has something serious noise; so if we substract it from sample the result may be wrong
if( USE_BLANK_SPECTRUM & length(fileList$blankFile) > 0 ) {
    xset.blank <- extractBlankInfo(fileList$blankFile)  
    
    cat("\n## Extracting peak list for Blank sample:", basename(fileList$blankFile), "\n")
    ## peak picking for samples using EIC(extracted ion chromatograms) for m/z values of interest. 
    peaks.blank <- extract_peak_list_blank(xset.blank, ctype="TIC", offset=1.5, RunPlotOnly)

    if ( ! RunPlotOnly ) {        
        ## RI calculation using Alkane Std
        peak_blank_ri <- get_RI_for_samples2(peaks.blank, peak_alkane_std)    
        profiled_peaks_blank <- compoundIdentify3(peak_blank_ri, xset.blank, lib.peak, alkaneInfo, RI.Variation, isBLANK=TRUE)
        # head(profiled_peaks_blank)
        # cat("profiled_peaks_blank\n"); print(profiled_peaks_blank)

        if( DEBUG ) {
          ofilename <- paste(sub(".mzXML|.CDF","", basename(fileList$blankFile), ignore.case = TRUE),"_profiledPeaksTMP.csv", sep='')
          write.csv(profiled_peaks_blank, file=ofilename, quote=TRUE, row.names=FALSE)
        }
        
        final_PeakProfile_blank <- arrangeProfiledPeaks2(profiled_peaks_blank, SampleType)
        cat("final_PeakProfile_blank\n"); print(final_PeakProfile_blank)

        final_PeakProfile_blank$CompoundWithTMS <- as.character(final_PeakProfile_blank$CompoundWithTMS)
        final_PeakProfile_blank$Area <- as.numeric(as.character(final_PeakProfile_blank$Area))
        final_PeakProfile_blank$MatchFactor <- as.numeric(as.character(final_PeakProfile_blank$MatchFactor))
        
        final_PeakProfile_blank <- final_PeakProfile_blank[which(final_PeakProfile_blank$MatchFactor > MF_THRESHOLD), ]
        final_PeakProfile_blank <- final_PeakProfile_blank[,-c(11,12,13,15,16,17)]
        ofilename <- paste(sub(".mzXML|.CDF","", basename(fileList$blankFile), ignore.case = TRUE),"_profiled.csv", sep='')
        write.csv(final_PeakProfile_blank, file=ofilename, quote=TRUE, row.names=FALSE)

        if (CREATE_JSON_FILE) {
            ofilename <- paste(sub(".mzXML|.CDF","", basename(fileList$blankFile), ignore.case = TRUE),"_spectrum.json", sep='')
            create_json_file(ofilename, xset.blank@scantime, xset.blank@tic,
                                        final_PeakProfile_blank$RT, final_PeakProfile_blank$Intensity, final_PeakProfile_blank$Compound)
        }        
        
        # rmCompoundStr <- ifelse(SampleType %in% c(SERUM,SALIVA), 'Ribitol', 'Cholesterol')     
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

if ( RunPlotOnly ) {
    ## Image Plot generation      
    cat("\n## Spectrum plot generation only ...\n")
    generateSpectrumPlot(fileList$sampleFiles, RunPlotOnly) 

    # Multi Core or Single Core
    if (TRUE) {
        # beg = Sys.time()
        generateSpectrumPlot.multicore(nCluster=nCores, fileList$sampleFiles, RunPlotOnly)
        # td = as.numeric(Sys.time() - beg, "secs")
        # cat("\n\n## Time - Multi Core:", td,"\n")
    } else { 
        # beg = Sys.time()
        generateSpectrumPlot(fileList$sampleFiles, RunPlotOnly)
        # td = as.numeric(Sys.time() - beg, "secs")
        # cat("\n\n## Time - Single Core:", td,"\n")
    }

} else {
    cat("\n## Profiling and quantifying for each sample...\n")

    checkInternalStd <- FALSE
    
    beg = Sys.time() 
   
    if (TRUE) {
        flag.blank_spectrum <- ifelse( USE_BLANK_SPECTRUM, TRUE, FALSE)
        conc.each <- quantification.multicore(nCores, fileList$sampleFiles, use.blank=flag.blank_spectrum, threshold.matchFactor=MF_THRESHOLD, 
                                      internalStd=internalStd, lib.calicurv=lib.calicurv, cmpdlist=cmpdlist, final_PeakProfile_blank=final_PeakProfile_blank)  
    } else {
        conc.each <- lapply(fileList$sampleFiles, quantifictionFunc, print.on=TRUE)
    }
   
    if (DEBUG) { cat("Each concentration table:\n"); print(conc.each) }
    td = as.numeric(Sys.time() - beg, "secs")
    cat("## Running Time with multiple Cores (", nCores,") :", td,"\n")
    
    ## merge concentration for all samples
    final.Concentration <- mergeConcTable( conc.each )
    if (DEBUG) { cat("\n\n final.Concentration:\n"); print(final.Concentration) }
        
    ## Combining  all concentration and Generate Files for Concentration Table with Sample ID
    ## use the "final.Concentration" data frame
    if ( internalStd != 'NONE') {
        # ofile.merged <- paste(basename(sampleFileDir),"_profiledAll.csv", sep='')
        ofile.merged <- "profiled_All.csv"
        cat("\n ## Generate Files for Concentration Table of All Samples:", ofile.merged, "\n")
        ## order by RI as in library; merge compound name with RI and sort by RI
        # cat("# final.concentration:\n"); print(final.Concentration)  
        colnames(final.Concentration)[1] <- 'HMDB_ID'
        # final.Concentration <- merge(lib.peakcal[,c('HMDB_ID','Compound','SeqIndex')], final.Concentration, by=c('HMDB_ID', 'Compound'), sort=FALSE, all=TRUE)   
        final.Concentration <- merge(lib.calicurv[,c('HMDB_ID','Compound','SeqIndex')], final.Concentration, by=c('HMDB_ID', 'Compound'), sort=FALSE, all=TRUE)   
        cat("\n ### final.Concentration:\n"); print(final.Concentration)
        final.Concentration <- final.Concentration[order(as.integer(as.character(final.Concentration$SeqIndex)), decreasing=FALSE), ]
        cat("\n ### final.Concentration (sort):\n"); print(final.Concentration)
                
        rm.SeqIndex <- which(names(final.Concentration) == "SeqIndex")
        final.Concentration <- final.Concentration[ , - rm.SeqIndex]
        rownames(final.Concentration) <- c(1:nrow(final.Concentration))
        
        if( DEBUG ) { cat("final.concentration:\n"); print(final.Concentration)  }
        
        write.table( t(final.Concentration), file=ofile.merged, sep=",", col.names=FALSE, quote=TRUE)
        cat("\n==============================================================\n")
        cat("\n## Done:", length(fileList$sampleFiles), "spectrum files\n\n")
    } else {
        cat("\n\n ## Fail to generate profiled_ALL due to missing internal Standard \n")
        cat("\t internalStd:", internalStd,"\n\n")
    }
    
    ofile.note <- paste(basename(sampleFileDir),"_note.txt", sep='')
    saveProfilingInfo(ofile.note, Version)   

}




  
