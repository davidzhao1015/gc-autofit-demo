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

ISDEBUG <- FALSE
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
    cat("\n## Profiling for each sample...\n")

    checkInternalStd <- FALSE
    for (i in 1:length(fileList$sampleFiles)) {   ## running per each sample
        # i <- 1
        f.sample <- fileList$sampleFiles[i]
      
        quantifictionFunc(f.sample, print.on=FALSE) 
          
          
        f.sample.basename <- basename(f.sample)
        cat(paste("\n", i, ") Compound Profiling and Quantifying:", sep=''), f.sample.basename, "\n")
    
        ## should be updated (remove peakFind function; data structure because it doesnot use anymore)  
        cat("\t >> Extracting Spectrum Information \n")
        xset.asample <- extractSampleInfo2(f.sample)
            
        ## peak picking for samples using TIC or EIC (peak's RT & Intensity)
        if( DEBUG ) {
            cat("\t >> Extracting peak list \n")
        }
        peaks.sample <- extract_peak_list_samples2(xset.asample, ctype="TIC", offset=1.5, RunPlotOnly)
        # cat("peaks.sample:\n"); print(peaks.sample/60)
        
        if (FALSE & DEBUG) {
            ## to check missing peak
            ofilename <- paste(sub(".mzXML|.CDF","", f.sample.basename, ignore.case = TRUE),"_extractedPeaks_temp.csv", sep='')
            write.csv(peaks.sample/60, file=ofilename, quote=FALSE)
        }
            
        ## RI calculation using Alkane Std
        cat("\t >> Get RI for each peak \n")
        peak_samples_ri <- get_RI_for_samples2(peaks.sample, peak_alkane_std)
        if( FALSE & DEBUG ) { cat("peak_samples_ri\n"); print(peak_samples_ri) }
        
        ##########################################################################    
        ## NOT USED for ACTUAL PROCESS
        ## For internal Works including library and calibration curves    
        ##########################################################################    
        {
            if( FALSE & DEBUG ) {
                ## for Zerihun to easy to find a peak
                ofilename <- paste(sub(".mzXML|.CDF","", f.sample.basename, ignore.case = TRUE),"_selectedPeaks_RTRI.csv", sep='')
                write.csv(peak_samples_ri, file=ofilename, quote=FALSE)
            }
            
            ## Temporary procedure
            if ( FALSE & DEBUG ) {
                ## this is for making calibration curve of the Organic Acid
                profiled_peaks <- getPeakInfo_OrganicAcid(peak_samples_ri, xset.asample, RI.Variation)
                ofilename <- paste(sub(".mzXML|.CDF","", basename(f.sample), ignore.case = TRUE),"_PeaksAreaInfo.csv", sep='')
                write.csv(profiled_peaks, file=ofilename, quote=FALSE)
            }
        }
    
        ## identification using RI and m/z with intensity (Scores)
        cat("\t >> Identifying peaks \n")
        ## Including Additional Information: Area, RTstart & RTend
        profiled_peaks <- compoundIdentify3(peak_samples_ri, xset.asample, lib.peak, alkaneInfo, RI.Variation, isBLANK=FALSE, print_mzInt=IS_PRINT_MZINT4DB)
        # head(profiled_peaks)
        if (DEBUG) {
            ofilename <- paste(sub(".mzXML|.CDF","", basename(f.sample), ignore.case = TRUE),"_profiledPeaksTMP.csv", sep='')
            write.csv(profiled_peaks, file=ofilename, quote=TRUE)
        }
        
        ## -- select only the highest score peaks
        cat("\t >> arrange profiled peaks \n")
        final_PeakProfile <- arrangeProfiledPeaks2(profiled_peaks, SampleType)
        if (DEBUG) { cat("final_PeakProfile:"); print(final_PeakProfile) }
        
        # Subtract BLANK Peak Areas
        # if there is a blank sample, then substract the area from sample's area
        if( USE_BLANK_SPECTRUM &  ! is.null(xset.blank) ) {            
            cat("\t >> subtract blank peaks' area \n") 
            # final_PeakProfile <- cbind(final_PeakProfile, Area.Blank=0)
            # exclude Standard Peak
            final_PeakProfile <- as.data.frame(final_PeakProfile)
            final_PeakProfile_blank <- as.data.frame(final_PeakProfile_blank)
            final_PeakProfile$Area <- as.double(as.character(final_PeakProfile$Area))
            
            # for trace Blank substraction
            Area.origin <- NA
            Area.blank <- NA
            final_PeakProfile <- cbind(final_PeakProfile, Area.origin, Area.blank)
            
            for(p.idx in 1:nrow(final_PeakProfile_blank)) {
                cmpdname.tmp <- final_PeakProfile_blank$CompoundWithTMS[p.idx]
                sp.idx <- which(as.character(final_PeakProfile$CompoundWithTMS) == cmpdname.tmp)
                
                if ( length(sp.idx) > 0 ) {
                    if(DEBUG) {
                      cat("# Subtract Blank Sample's Peak Area - sp.idx:", sp.idx, "\n")
                      cat("\t cmpdname:", cmpdname.tmp, "sample:", final_PeakProfile[sp.idx, 'Area'], "-", "blank:", final_PeakProfile_blank$Area[p.idx], "\n")
                    } 

                    # final_PeakProfile[sp.idx, 'Area.Blank'] <- final_PeakProfile_blank[p.idx, 'Area']
                    final_PeakProfile[sp.idx, 'Area.origin'] <- final_PeakProfile[sp.idx, 'Area']
                    final_PeakProfile[sp.idx, 'Area.blank'] <- as.double(final_PeakProfile_blank[p.idx, 'Area'])
                    final_PeakProfile[sp.idx, 'Area'] <- as.double(as.character(final_PeakProfile[sp.idx, 'Area'])) - as.double(final_PeakProfile_blank[p.idx, 'Area'])
                    if ( final_PeakProfile[sp.idx, 'Area'] < 0 ) {
                         final_PeakProfile[sp.idx, 'Area']  <- 0
                    }
                    if(DEBUG) {
                      cat("\t subtracted sample area:", final_PeakProfile[sp.idx, 'Area'],"\n")
                    } 
                } 
            }
        }
        
        if (DEBUG) { cat("## final_peakProfile after blank subtraction:\n"); print(final_PeakProfile)  }
        
        # making JSON file for Profiled Peak View
        if (CREATE_JSON_FILE) {
          ofilename <- paste(sub(".mzXML|.CDF","", basename(f.sample), ignore.case = TRUE),"_spectrum.json", sep='')
          create_json_file(ofilename, xset.asample$xraw@scantime, xset.asample$xraw@tic,
                           final_PeakProfile$RT, final_PeakProfile$Intensity, final_PeakProfile$Compound )
        }
    
        ## Quantification        
        checkInternalStd <- existInternalStd(internalStd, final_PeakProfile, lib.peak)
        # if (i==1) checkInternalStd <- FALSE
        
        if ( internalStd != 'NONE' & checkInternalStd == TRUE ) {
              ## screening with Match Factor using threshold 
              cat("\t >> Screening with Match Factor\n")
              final_PeakProfile.screened <- screeningWithMatchFactor(final_PeakProfile, MF_THRESHOLD)
              # head(final_PeakProfile.screened)
              # nrow(final_PeakProfile); nrow(final_PeakProfile.screened)
              
              cat("\t >> Quantifying identified peaks\n")
              # unitConv 1 - uM, 1000 - mM ( Serum and Urine -> should be same unit (uM))
              # quantifiedResult <- quantification(final_PeakProfile, lib.peakcal, internalStd, f.sample, stype=SampleType, unitConv=1000) # 1 or 1000 mM, uM
              quantifiedResult <- quantification(final_PeakProfile.screened, lib.peak, lib.calicurv, internalStd, f.sample, stype=SampleType, unitConv="mM") 
              # nrow(quantifiedResult)  
            
              if (DEBUG) {
                cat("## quantified Result:\n")
                print(quantifiedResult)
                # hmdbID, Area, Concentration 
              }

              cat("\t >> Generating FinalReport for", basename(f.sample), "\n")
              finalReport <- genFinalReport(final_PeakProfile.screened, quantifiedResult) 
              colnames(finalReport)[1] <- "HMDB_ID"
              
              if (DEBUG) { cat("finalReport$Concentration:\n"); print(finalReport$Concentration) }
              Concentration2 <- check.Concentration(finalReport$Concentration) 
              finalReport <- cbind(finalReport, Concentration2) 
              if (DEBUG) { cat("# finalReport:\n"); print(finalReport); }
              
              names(finalReport) 
              finalReport.All <- merge(cmpdlist, finalReport, by=c('HMDB_ID','CompoundWithTMS'), all.x=TRUE)
              finalReport.All <- finalReport.All[order(finalReport.All$SeqIndex), ]
              rownames(finalReport.All) <- c(1:nrow(finalReport.All))
              
              ofilename <- paste(sub(".mzXML|.CDF","", f.sample.basename, ignore.case = TRUE),"_profiled.csv", sep='')
              # names(finalReport.All)
              finalReport.All <- finalReport.All[,c("SeqIndex", "HMDB_ID", "Compound", "CompoundWithTMS", "RT_min","RT","RI","Intensity",
                                                    "MatchFactor", "RI.Similarity","Area","RT.start","RT.end","Concentration2")]                  
              if (DEBUG) { 
                  cat("finalReport All:\n"); print(head(finalReport.All)); 
              }
               
              ## exclude NA or MP(Multiple Peak) cases  
              finalReport.All <- finalReport.All[which( (!is.na(finalReport.All$Concentration2)) & 
                                                        (finalReport.All$Concentration2 != "MP") ), ]   

              outColnames <- c("SeqIndex", "HMDB_ID", "Compound", "CompoundWithTMS", "RT_min","RT","RI","Intensity",
                               "MatchFactor", "RI.Similarity","Area","RT.start","RT.end","Concentration")
              write.table(finalReport.All, file=ofilename, quote=TRUE, row.names=FALSE, col.names=outColnames, sep=",")
              if (DEBUG) { cat("# finalReport All:\n"); print(finalReport.All); }
              
              ## concentration summary table
              tmp.Concentration <- finalReport.All[, c("HMDB_ID","Compound","Concentration2")]
              na.length <- length(which(is.na(tmp.Concentration$Concentration2)))
              if( na.length > 0 ) {
                  # cat("length(NA):", na.length, "\n")
                  tmp.Concentration <- tmp.Concentration[- which(is.na(tmp.Concentration$Concentration2)), ]
              }
              if( length(which(tmp.Concentration$Concentration2 == "MP")) > 0 ) {
                  tmp.Concentration <- tmp.Concentration[- which(tmp.Concentration$Concentration2 == "MP"), ]
              }
              
              colnames(tmp.Concentration) <- c('HMDB_ID', 'Compound', basename(sub(".mzXML|.CDF", "", f.sample, ignore.case = TRUE)) )
              if( is.null(final.Concentration) ) {
                  final.Concentration <- tmp.Concentration
              } else {
                  final.Concentration <- merge(final.Concentration, tmp.Concentration, by=c('HMDB_ID', 'Compound'), sort=FALSE, all=TRUE)
              }
              
              if (DEBUG) { cat("\n\n final.Concentration:\n"); print(final.Concentration) }
              
        } else {
              ## No internal STD found --> should this be kept???????
              
              cat("\t >> Generating FinalReport for", basename(f.sample), " without Quantification\n")
              finalReport <- merge(cmpdlist, final_PeakProfile, by = c('Compound'), sort=FALSE) 
              finalReport <- finalReport[order(as.numeric(as.character(finalReport$SeqIndex))), ]
              PeakNo <- c(1:nrow(finalReport))
              finalReport <- cbind(PeakNo, finalReport)
              rownames(finalReport) <- c(1:nrow(finalReport))
              if(DEBUG) {  cat("finalReport:\n"); print(head(finalReport)) }

              Concentration <- NA
              finalReport <- cbind(finalReport, Concentration)
              
              ofilename <- paste(sub(".mzXML|.CDF","", f.sample.basename, ignore.case = TRUE),"_profiled.csv", sep='')
              if ( DEBUG ) {
                  # if (TRUE) { finalReport <- finalReport[,-c(3,11,12,13,14,15,19,20)] }
                  if (TRUE) { finalReport <- finalReport[,-c(3,4,12,13,14,15,19,22)] }
                  cat("head(finalReport):\n"); print(head(finalReport)); 
                  write.csv(finalReport, file=ofilename, quote=TRUE, row.names=FALSE)
              } else {
                  # finalReport <- finalReport[,-c(3,11,12,13,14,15,19,20)]
                  finalReport <- finalReport[,-c(3,4,12,13,14,15,19,22)]
                  outColnames <- c("PeakNo","Compound","HMDB ID","RT(min)","RT","RI","Intensity","MatchFactor","RI.Similarity","Area","RT.start","RT.end", "Concentration")
                  write.table(finalReport, file=ofilename, quote=TRUE, row.names=FALSE, col.names=outColnames, sep=",")
              }
              
              # if some of sample does not have Internal Standard, all the concentration will be null
              # this is only for combined results
              if ( internalStd != 'NONE' & checkInternalStd == FALSE) {
                  tmp.Concentration <- finalReport[, c('Compound','HMDB_ID', 'Concentration')]
                  
                  colnames(tmp.Concentration) <- c('Compound', 'HMDB_ID', basename(sub(".mzXML|.CDF", "", f.sample, ignore.case = TRUE)) )
                  if( is.null(final.Concentration) ) {
                      final.Concentration <- tmp.Concentration
                  } else {
                      final.Concentration <- merge(final.Concentration, tmp.Concentration, by=c('Compound','HMDB_ID'), sort=FALSE, all=TRUE)
                  }
              }
              if (DEBUG) { cat("\n\n final.Concentration:\n"); print(final.Concentration) }
        }
    }
    
    # cat("cmpdlist:\n"); print(cmpdlist)
    # cat("lib.peakcal[,c(3,2)]:\n"); print(lib.peakcal[,c('HMDB_ID','Compound','RI')])
    # cat("final.Concentration[,-2]:\n"); print(final.Concentration[,-2])
    
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
    }
    
    ofile.note <- paste(basename(sampleFileDir),"_note.txt", sep='')
    saveProfilingInfo(ofile.note, Version)    
    saveProfilingInfo(ofile.note, RI.Variation)    

}




  
