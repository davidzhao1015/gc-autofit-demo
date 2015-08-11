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
# Version <- "1.0 released 20150521"
Version <- "1.0 released 20150731"
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
  
  ### changed this option to support new platform 
  # 
  # if(is.null(argsL$plotonly)) {
  #  showErrMessage("  Warning in argument:\n\t plot only option is not assigned")
  #  helpMessage()
  #} else {
  #  RunPlotOnly <- as.logical(argsL$plotonly)
  #}
  
  if(argsL$process %in% c('PREPROCESSING','PROFILING')) {
      processOption <- argsL$process;
  } else {
      showErrMessage("  Error in argument:\n\t see the help to correctly use the process option")
      helpMessage()
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
  
  if(is.null(argsL$outdir)) {
    # default output directory
    user.outdir <- paste(dirname(sampleFileDir), "/", sub("_input","",basename(sampleFileDir)), '_result', sep='')
  } else {
    user.outdir <- argsL$outdir # expected full path
    if(ISDEBUG) { cat("## user.outdir:"); print(user.outdir) }
  }

  # In profiling process, it needs to have the Alkane and Blank information from the Preprocessing process.
  if( processOption == 'PROFILING' ) {
      cat("argsL$infoFileDir:"); print(argsL$infoFileDir)
      if(is.null(argsL$infoFileDir)) {
          showErrMessage("  Error in argument (--infoFileDir):\n\t Alkane Standard and Blank Profiled Information are required \n\t with '--infoFileDir' option")
          helpMessage()
      } else {
          infoFileDir <- argsL$infoFileDir
          if(ISDEBUG) { cat("## infoFileDir:"); print(argsL$infoFileDir) }
      }
  }
}

## create and set working directory (save output/result files) 
## may be changed: upload_user_temp_dir \data, \Result
dir.create(user.outdir, showWarnings=TRUE, recursive=TRUE) # expected full path
dirProfileResult <- normalizePath(user.outdir)
setwd(dirProfileResult)  ## Set working directory    

if(ISDEBUG) cat("## dirProfileResult:"); print(dirProfileResult)

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
# Preprocessing
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

    if(DEBUG) {cat("final alkane profiled:\n"); print(peak_alkane_std) }
    alkaneInfo <- check_alkane_std(peak_alkane_std)

    ## =======================================================================================
    ## Blank Sample Handling
    ## =======================================================================================
    
    ## @@ blank has something serious noise; so if we substract it from sample the result may be wrong
    if( USE_BLANK_SPECTRUM & length(fileList$blankFile) > 0 ) {
        ###### make a single function as Alkane
        ######
        
        xset.blank <- extractBlankInfo(fileList$blankFile)  
        
        cat("\n## Extracting peak list for Blank sample:", basename(fileList$blankFile), "\n")
        ## peak picking for samples using EIC(extracted ion chromatograms) for m/z values of interest. 
        # peaks.blank <- extract_peak_list_blank(xset.blank, ctype="TIC", offset=1.5, RunPlotOnly)
        peaks.blank <- extract_peak_list_blank(xset.blank, ctype="TIC", offset=1.5)
    
        # if ( ! RunPlotOnly ) {        
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
        # }
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
    cat("\n ##### Loading Preprocessed Information Files \n")
    infoFileDir.alkane <- paste(infoFileDir, "/alkaneInfo.Rdata", sep='')
    cat(infoFileDir.alkane,"\n")
    file.exists(infoFileDir.alkane)
    
    infoFileDir.alkanePeak <- paste(infoFileDir, "/alkanePeakInfo.Rdata", sep='')
    cat(infoFileDir.alkanePeak,"\n")
    file.exists(infoFileDir.alkanePeak)
    
    infoFileDir.blank <- paste(infoFileDir, "/blankInfo.Rdata", sep='')
    cat(infoFileDir.blank,"\n")
    file.exists(infoFileDir.blank)
    
    load(infoFileDir.alkane)
    load(infoFileDir.alkanePeak)
    load(infoFileDir.blank)
    
    cat("\n\ncheck alkane & blank\n\n")
    cat("## alkaneInfo:\n"); print(alkaneInfo);
    cat("## peak_alkane_std:\n"); print(peak_alkane_std);
    cat("## final_PeakProfile_blank:\n"); print(final_PeakProfile_blank);

    # stop()
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

    # Multi Core or Single Core
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

} else {
    ## PROFILING & Quantification as optional
  
    ## Loading the Generated Alkane & Blank Profile Information in PREPROCESSING 
    cat("\n## Profiling and quantifying for each sample...\n")

    checkInternalStd <- FALSE
    
    if (DEBUG) { beg = Sys.time() }
   
    if (nCores >= 1) {
        if(DEBUG) cat("## Profiling with multi cores\n")
        flag.blank_spectrum <- ifelse( USE_BLANK_SPECTRUM, TRUE, FALSE)
        conc.each <- quantification.multicore(nCores, fileList$sampleFiles, use.blank=flag.blank_spectrum, threshold.matchFactor=MF_THRESHOLD, 
                                      internalStd=internalStd, lib.calicurv=lib.calicurv, cmpdlist=cmpdlist, final_PeakProfile_blank=final_PeakProfile_blank)  
        if(DEBUG) cat("## Profiling for each sample -- Done\n")
    } else {
        if(DEBUG) cat("## Profiling with single core\n")
        conc.each <- lapply(fileList$sampleFiles, quantifictionFunc, print.on=TRUE)
    }
   
    if (DEBUG) { 
        cat("# [Profiling] Each concentration table:\n"); print(conc.each) 
        td = as.numeric(Sys.time() - beg, "secs")
        if (DEBUG) cat("## Running Time with multiple Cores (", nCores,") :", td,"\n")
    }
    
    ## merge concentration for all samples
    if (length(conc.each) >= 2) {       
        final.Concentration <- mergeConcTable( conc.each )
    } else {
        final.Concentration <- conc.each;
    }
    if (DEBUG) { cat("\n\n final.Concentration:\n"); print(final.Concentration) }
    
    if (FALSE) {        
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
            ## OPTION or if no internal std, then just report without quantification 
            cat("\n\n ## Fail to generate profiled_ALL due to missing internal Standard \n")
            cat("\t internalStd:", internalStd,"\n\n")
        }
    }
    
    ofile.note <- paste(basename(sampleFileDir),"_note.txt", sep='')
    saveProfilingInfo(ofile.note, Version)   
}
