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


## argument check for standalone version
##===========================================================================================
## Ref: http://www.r-bloggers.com/parse-arguments-of-an-r-script/

args <- commandArgs(trailingOnly = FALSE)
# args <- commandArgs(TRUE)
# cat("length(args):", length(args),"\n")

# cat("argsL:\n"); print(argsL)
RProgram_dir <- normalizePath(dirname(sub("^--file=", "", args[grep("^--file=", args)])))
lib_dir <- file.path(RProgram_dir, "lib")

source( file.path(RProgram_dir, "envVars.cfg") )  ## loading packages, libraries, and definitions
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
  
  # Internal Std in Library (HMDB ID or Compound Name; Final, Compound Name will be used for the analysis)
  if(is.null(argsL$internalstd)) {
      showErrMessage("  Error in argument:\n\t There is no internal standard compound. 
                            If you want profile only, then please set with 'NONE' ")
      helpMessage()
  } else {
      internalStd.in <- argsL$internalstd
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
          infoFileDir <- normalizePath(argsL$infoFileDir)
          if(ISDEBUG) { cat("## infoFileDir:"); print(argsL$infoFileDir) }
      }
  }
}

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
        # head(profiled_peaks_blank)
        # cat("profiled_peaks_blank\n"); print(profiled_peaks_blank)

        if( DEBUG ) {
            ofilename <- paste(specFilename.blank,"_profiledPeaksTMP.tsv", sep='')
            write.table(profiled_peaks_blank, file=ofilename, quote=TRUE, row.names=FALSE, sep="\t")
        }
        
        final_PeakProfile_blank <- arrangeProfiledPeaks2(profiled_peaks_blank)
        if (DEBUG) { cat("final_PeakProfile_blank\n"); print(final_PeakProfile_blank) }

        # moved to the arrangeProfiledPeak2() 
        # final_PeakProfile_blank$CompoundWithTMS <- as.character(final_PeakProfile_blank$CompoundWithTMS)
        # final_PeakProfile_blank$Area <- as.numeric(as.character(final_PeakProfile_blank$Area))
        # final_PeakProfile_blank$MatchFactor <- as.numeric(as.character(final_PeakProfile_blank$MatchFactor))
        
        #!!!! should be included in arrangeProfiledPeaks2
        final_PeakProfile_blank <- final_PeakProfile_blank[which(final_PeakProfile_blank$MatchFactor > MF_THRESHOLD), ]                 
        final_PeakProfile_blank.json <- cbind(final_PeakProfile_blank[,-c(9:12,17)], Concentration="NA") # keep mass Spectrum Info (m/z, Intensity)
        final_PeakProfile_blank <- cbind(final_PeakProfile_blank[,-c(9:13,17,18,19)], Concentration="NA")

        ofilename <- paste(specFilename.blank,"_profiled.csv", sep='')
        write.csv(final_PeakProfile_blank, file=ofilename, quote=TRUE, row.names=FALSE)

        if (CREATE_JSON_FILE) {
            ofilename <- paste(specFilename.blank,"_spectrum.json", sep='')
            create_json_file(ofilename, xset.blank@scantime, xset.blank@tic, final_PeakProfile_blank.json)
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
   
    # if (nCores > 1) {
    if ( FALSE ) {
        if(DEBUG) cat("\n\n## Profiling with multi cores\n")
        flag.blank_spectrum <- ifelse( USE_BLANK_SPECTRUM, TRUE, FALSE)
        conc.each <- quantification.multicore(nCores, fileList$sampleFiles, use.blank=flag.blank_spectrum, threshold.matchFactor=MF_THRESHOLD, 
                                      internalStd=internalStd, lib.calicurv=lib.calicurv, cmpdlist=cmpdlist, final_PeakProfile_blank=final_PeakProfile_blank)  
        if(DEBUG) cat("## Profiling for each sample -- Done\n")
    } else {
        if(DEBUG) cat("\n\n## Profiling with single core\n")
        # conc.each <- lapply(fileList$sampleFiles, quantifictionFunc, print.on=TRUE)
        conc.each <- lapply(fileList$sampleFiles, quantifictionFunc, print.on=TRUE, use.blank=USE_BLANK_SPECTRUM, threshold.matchFactor=MF_THRESHOLD, 
                            internalStd=internalStd, lib.calicurv=lib.calicurv, cmpdlist=cmpdlist, final_PeakProfile_blank=final_PeakProfile_blank)        
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
    if (DEBUG) { cat("\n\n final.Concentration:\n"); print(final.Concentration) }
    
    
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
            # cat("# final.concentration:\n"); print(final.Concentration)  

            # colnames(final.Concentration)[1] <- 'HMDB_ID'
            
            # final.Concentration <- merge(lib.peakcal[,c('HMDB_ID','Compound','SeqIndex')], final.Concentration, by=c('HMDB_ID', 'Compound'), sort=FALSE, all=TRUE)   
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
