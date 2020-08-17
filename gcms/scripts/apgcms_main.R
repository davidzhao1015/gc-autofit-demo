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

time1 <- Sys.time()
options(echo=FALSE) # if you want see commands in output file
options("width"=120)
options(warn = -1) # ignore the warnings
# options(warn = 2) # ignore the warnings

library(parallel) # for using multiple cores

## argument check for standalone version
##===========================================================================================
## Ref: http://www.r-bloggers.com/parse-arguments-of-an-r-script/

args <- commandArgs(trailingOnly = FALSE)
# args <- commandArgs(TRUE)
# cat("length(args):", length(args),"\n")
# cat("argsL:\n"); print(argsL)

# get program directory
#RProgram_dir <- normalizePath(dirname(sub("^--file=", "", args[grep("^--file=", args)])))
#lib_dir <- file.path(RProgram_dir, "lib")

#source( file.path(RProgram_dir, "envVars.cfg") )  ## loading packages, libraries, and definitions
#source( file.path(lib_dir, libfunc.SetOpts.file) )  ## loading packages, libraries, and definitions
script.dirname <- dirname(strsplit(commandArgs(trailingOnly = FALSE)[4],"=")[[1]][2]) 
setwd(script.dirname)
#print(script.dirname)
root_path <- dirname(script.dirname)
#print(root_path)

lib_mz_intensity_dir <- file.path(root_path, "gcmsdb/mz_intensity")
lib_calibration_dir <- file.path(root_path, "gcmsdb/calibrations")

source(file.path(root_path, "envVars.cfg"))  ## loading packages, libraries, and definitions
source(file.path(script.dirname, libfunc.SetOpts.file))  ## loading packages, libraries, and definitions


showProgramInfo(Version)

if (length(args) == 5) {  args <- c("--help") } 
if ("--help" %in% args) {  helpMessage()  }

## parsing input arguments
parsingArgument(args)
# cat("\n## AFTER PARSING ARGs\n")

## create and set working directory (save output/result files) 
## may be changed: upload_user_temp_dir \data, \Result
dir.create(user.outdir, showWarnings=FALSE, recursive=TRUE) # expected full path
sampleName <- basename(user.outdir)
dirProfileResult <- normalizePath(user.outdir)
setwd(dirProfileResult)  ## Set working directory    

## collect error log details
cat(file=File.ErrorLog, "## Error Log Details ##\n\n")

if(ISDEBUG) { cat("## dirProfileResult:"); print(dirProfileResult) }

# loading libraries

# cat("USE_INTERNAL_LIBRARY => ", USE_INTERNAL_LIBRARY, "\n")

if( USE_INTERNAL_LIBRARY == 'NONE') {  ## use user's own library  (need to be tested) 
    lib.peak <- getLibInfo(userLibFile)
    if( !is.null(userCalFile) ) {
        lib.calicurv <- getLibInfo(userCalFile)
    }
} else{  
    SampleType <- setSampleType(USE_INTERNAL_LIBRARY)
    CalCurveType <- setCalCurveType(USE_INTERNAL_LIBRARY, internalStd.in)
    cat("internalStd.in:", internalStd.in, "\n")
    cat("SampleType:", SampleType,"\n")
    cat("CalCurveType:", CalCurveType,"\n")
    
    ## load libraries (profiling/identification)
    # fname.lib.peak <- file.path(file.path(lib_dir, LibFile[SampleType]))
    fname.lib.peak <- file.path(lib_mz_intensity_dir, LibFile[SampleType])
    cat("fname.lib.peak:", fname.lib.peak, "\n")
    lib.peak <- getLibInfo(fname.lib.peak)
    
    ## load calibration curve library
    if(internalStd.in == 'None' | CalCurveType == 0) {
        lib.calicurv <- NULL
    } else {
        #fname.lib.calicurve <- file.path(file.path(lib_dir, LibCaliCurveFile[CalCurveType]))
        fname.lib.calicurve <- file.path(lib_calibration_dir, LibCaliCurveFile[CalCurveType])
        lib.calicurv <- getLibInfo(fname.lib.calicurve)
    }
    
    ## making Subset of Internal Library 
    ## if USER_SELECTED_CMPDS is not NULL (user selected some compounds of internal library)
    if (! is.null(USER_SELECTED_CMPDS)) {
        userSelectedCmpdList <- unlist(strsplit(USER_SELECTED_CMPDS, ","))
        # cat("\n\n# lib.peak:\n"); print(as.character(lib.peak$HMDB_ID)) 
        # cat("\n\n# lib.calicurv:\n"); print(as.character(lib.calicurv$HMDB_ID))
        lib.peak <- lib.peak[which(as.character(lib.peak$HMDB_ID) %in% userSelectedCmpdList), ]
        if(internalStd.in == 'None' | CalCurveType == 0) {
            lib.calicurv <- NULL
        } else {
            lib.calicurv <- lib.calicurv[which(as.character(lib.calicurv$HMDB_ID) %in% userSelectedCmpdList), ]
        }
    }
}  

## load library for the functions
#source( file.path(lib_dir, libfunc.file) )  ## loading packages, libraries, and definitions
#source( file.path(lib_dir, libfunc.Alkane.file) )  ## loading packages, libraries, and definitions
#source( file.path(lib_dir, libfunc.JSON.file) )  ## loading packages, libraries, and definitions
source(file.path(script.dirname, libfunc.file))  ## loading packages, libraries, and definitions
source(file.path(script.dirname, libfunc.Alkane.file))  ## loading packages, libraries, and definitions
source(file.path(script.dirname, libfunc.JSON.file))  ## loading packages, libraries, and definitions
## set Debug Mode (TRUE / FALSE)
setDebugMode(ISDEBUG);  

# get Internal Standard Compound Name using input internal Std (compound name or HMDB ID)
if (toupper(internalStd.in) == "NONE")  {
    cat("\n\n## Internal Standard was not assigned. Proceed to profilie the compound only! \n")
    internalStd <- "NONE"
} else {  
    internalStd <- getInternalStdCmpdName(lib.peak, internalStd.in)

    if ( is.null(internalStd) ) {
        stopMessage(paste("\n\t Cannot find the selected Internal Standard [", internalStd.in,"]",
                  "\n\t in the library [ Biofluid type:",USE_INTERNAL_LIBRARY,"].",
                  "\n\n\t Please use the correct Internal Standard with Biofluid type.") )
    } else {
        cat("\n\n## Internal Standard [", internalStd,"] is in the library\n")
    }
}

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
if (DEBUG.TIME) { t.beg = Sys.time() }


if (processOption == 'PREPROCESSING') {
  
    ## =======================================================================================
    ## Alkane Peak Handling (Profiling & Spectrum Plot)
    ## =======================================================================================
  
    #source( file.path(lib_dir, libfunc.Alkane.file) )  ## loading packages, libraries, and definitions
    source(file.path(script.dirname, libfunc.Alkane.file)) ## loading packages, libraries, and definitions
    # Alkane Peak Profiling
    #fname.lib.alkane <- file.path(lib_dir, LibFile.Alkane)
    fname.lib.alkane <- file.path(lib_mz_intensity_dir, LibFile.Alkane)
    peak_alkane_std <- do_AlkanePeakProfile(fname.lib.alkane, fileList$alkaneFile, setAdjustAlkanePeakCn=IS_AlkanePeakCnAdjust, userDefined.Cn=user.AlkaneRT, userEstAlkaneRT=TRUE) 

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
        peaks.blank <- extract_peak_list_blank(xset.blank, ctype="TIC", offset=1.5) # offset > 2 then use mean; offset < 2 then use median

        ## RI calculation using Alkane Std
        peak_blank_ri <- get_RI_for_samples2(peaks.blank, peak_alkane_std)    

        ## compound Identification
        profiled_peaks_blank <- compoundIdentify4(peak_blank_ri, xset.blank, lib.peak, alkaneInfo, RI.Variation, isBLANK=TRUE)
        #if (DEBUG) { cat("\n\n## profiled_peaks_blank\n"); print(head(profiled_peaks_blank)) }
        if (is.null(profiled_peaks_blank)) {
            xset.blank <- NULL
            USE_BLANK_SPECTRUM <- FALSE
            final_PeakProfile_blank <- NULL
            ofilename <- paste(specFilename.blank,"_profiled.csv", sep='')
            file.create(ofilename)
            ofilename <- paste(specFilename.blank,"_spectrum.json", sep='')
            file.create(ofilename)
        }
        else {
            colnames(profiled_peaks_blank)[1] <- "HMDB_ID"
        
            if( DEBUG ) {
                 ofilename <- paste(specFilename.blank,"_profiledPeaksTMP.tsv", sep='')
                 write.table(profiled_peaks_blank, file=ofilename, quote=TRUE, row.names=FALSE, sep="\t")
            }
        
            final_PeakProfile_blank <- arrangeProfiledPeaks2(profiled_peaks_blank)
            if (DEBUG) { cat("\n\n## final_PeakProfile_blank\n"); print(head(final_PeakProfile_blank)) }

            final_PeakProfile_blank <- final_PeakProfile_blank[which(final_PeakProfile_blank$MatchFactor > MF_THRESHOLD), ]
        
            if(nrow(final_PeakProfile_blank) == 0) {
                cat("\n## No significant compound was identified in Blank Sample (will be used as no blank)\n")
                xset.blank <- NULL
                USE_BLANK_SPECTRUM <- FALSE
                final_PeakProfile_blank <- NULL
            } else {
        
                final_PeakProfile_blank.json <- cbind(final_PeakProfile_blank[, c("HMDB_ID","CompoundWithTMS","RT_min","RI","Intensity",
                                                                              "MatchFactor","TScore","RI.Similarity","Corr.Spearman","TargetIon","QIon",
                                                                              "Area.EICTarget","Area.EICQualification","AreaRatio","mz","mzInt")], 
                                                  Concentration2="NA") # keep mass Spectrum Info (m/z, Intensity)
            
            
                final_PeakProfile_blank <- cbind(final_PeakProfile_blank[,c("HMDB_ID", "CompoundWithTMS", "RT_min","RT","RI","Intensity",
                                                                        "MatchFactor","RI.Similarity","TScore","matchMZrate","TargetIon","TargetIon.intensity","QIon",
                                                                        "Area.EICTarget","Area.EICQualification","AreaRatio")], 
                                             Concentration="NA")
            
            
                ofilename <- paste(specFilename.blank,"_profiled.csv", sep='')
                outColnames <- c("HMDB_ID", "CompoundWithTMS", "RT_min","RT","RI","Intensity",
                                                  "MatchFactor","RI.Similarity","TScore","matchMZrate","TargetIon","TargetIon.intensity","QIon",
                                                  "Area.EICTarget","Area.EICQualification","AreaRatio", "Concentration") 
                write.table(final_PeakProfile_blank, file=ofilename, quote=TRUE, row.names=FALSE, col.names=outColnames, sep=",")
            
                if (CREATE_JSON_FILE) {
                    ofilename <- paste(specFilename.blank,"_spectrum.json", sep='')
                    create_json_file(ofilename, round(xset.blank@scantime/60,3), xset.blank@tic, final_PeakProfile_blank.json)
                }
                    
                # rmCompoundStr <- ifelse(SampleType %in% c(SERUM,SALIVA), 'Ribitol', 'Cholesterol')     
                if( (internalStd != "NONE") && !is.null(internalStd) ) {
                      rmCompoundStr <- internalStd
                      # check whether or not exist the Internal Standard
                      if (nrow(final_PeakProfile_blank[which(final_PeakProfile_blank$Compound == rmCompoundStr), ]) == 0) {
                          if(DEBUG) {
                            cat("## Warning: There is no Internal Standard in Blank.\n## >> Running without Blank ! \n")
                          }
                          xset.blank <- NULL
                          USE_BLANK_SPECTRUM <- FALSE
                      }
                  
                      final_PeakProfile_blank <- final_PeakProfile_blank[- which(final_PeakProfile_blank$Compound == rmCompoundStr), ]
                  
                      if( nrow(final_PeakProfile_blank) == 0  ) {
                          if(DEBUG) {
                            cat("## Warning: There is no coumpounds in Blank.\n## >> Running without Blank ! \n")
                          }
                          xset.blank <- NULL
                          USE_BLANK_SPECTRUM <- FALSE
                      }
                }
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
    time3 <- Sys.time()
    ## for Quantification Process, loading preprocessed information from saved files
    cat("\n##### Loading Preprocessed Information Files \n")

    ## New scenario. A big batch with many cases submission. It is not 
    ## possible to wait for preprocess finish then manually click on 
    ## the profile button to initiate profiling process. In batch mode,
    ## preprocess and profiling are triggered at the same time. But profiling 
    ## has to wait for some files from preprocess. So has to check the files of
    ## proprocess exists or not. 
    
    infoFileDir.alkane <- paste(infoFileDir, "/alkaneInfo.Rdata", sep='')
    if(DEBUG) cat(infoFileDir.alkane,"\n")
    
    
    infoFileDir.alkanePeak <- paste(infoFileDir, "/alkanePeakInfo.Rdata", sep='')
    if(DEBUG) cat(infoFileDir.alkanePeak,"\n")
    
    
    infoFileDir.blank <- paste(infoFileDir, "/blankInfo.Rdata", sep='')
    if(DEBUG) cat(infoFileDir.blank,"\n")
    #waiting is just for 1200 seconds /20 mins.
    for (i in 1:400) {
      if (file.exists(infoFileDir.alkane) && 
          file.exists(infoFileDir.alkanePeak) && 
          file.exists(infoFileDir.blank) ){
        Sys.sleep(3)
        break
      }
      Sys.sleep(3)
      next
    }

    file.exists(infoFileDir.alkanePeak)
    file.exists(infoFileDir.alkane)
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
    time4 <- Sys.time()
    cat("\nFinish first part!\n")
    difftime(time4, time3, units = "auto")
}

## =======================================================================================
## peak identifying for samples
## =======================================================================================
# cat("\n## Processing Sample Spectra Files ...\n")
# print(head(lib.peak[,c(1:5)]))
HMDBID <- as.character(unique(lib.peak[, c('HMDB_ID')]))
cmpdlist <- as.data.frame( lib.peak[, c("SeqIndex","HMDB_ID","Compound",'CompoundWithTMS')] )
# cmpdlist
# print(unique(cmpdlist$HMDB_ID))

final.Concentration <- NULL
nCores <- detectCores() - 1  # get the number of cores, -1 for other use

## with Preprocessing Option
## Alkane & Blank & Plots
if ( processOption == "PREPROCESSING" ) {
    ## ALKANE
    ## BLANK
  
    ## Image Plot generation      
    cat("\n#####################################################################\n")
    cat("## Spectrum plot generation only ...\n\n")

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
    time5 <- Sys.time()
    ## Loading the Generated Alkane & Blank Profile Information in PREPROCESSING 
    cat("\n#####################################################################\n")
    cat("## Profiling and quantifying for each sample ...\n\n")

    checkInternalStd <- FALSE    
   
    # For supporting multiple cores. However, it was changed because process as queue structure
    if ( FALSE ) {
        # if(DEBUG) cat("\n\n## Profiling with multi cores\n")
        # flag.blank_spectrum <- ifelse( USE_BLANK_SPECTRUM, TRUE, FALSE)
        # conc.each <- quantification.multicore(nCores, fileList$sampleFiles, use.blank=flag.blank_spectrum, threshold.matchFactor=MF_THRESHOLD, 
        #                              internalStd=internalStd, lib.calicurv=lib.calicurv, cmpdlist=cmpdlist, final_PeakProfile_blank=final_PeakProfile_blank)  
        # if(DEBUG) cat("## Profiling for each sample -- Done\n")
    } else {
        # SINGLE CORE
        if(DEBUG) cat("\n\n## Profiling with single core\n")
        cat("sampleFiles:\n"); print(fileList$sampleFiles)
        # filter out others CDF files.
        filtered_sample_files <- grep(sampleName, fileList$sampleFiles, value = TRUE)
        cat("After, sampleFiles:\n"); print(filtered_sample_files)
        conc.each <- lapply(filtered_sample_files, quantificationFunc, print.on=TRUE, 
                            use.blank=USE_BLANK_SPECTRUM, threshold.matchFactor=MF_THRESHOLD, 
                            internalStd=internalStd, lib.calicurv=lib.calicurv, cmpdlist=cmpdlist, 
                            final_PeakProfile_blank=final_PeakProfile_blank, PRINT_MZINT4DB=TRUE, process=processOption)        
    }
   
  
    ## merge concentration for all samples
    # conc.each <- conc.each[-which(is.null(conc.each))]
    # cat("\n\n## conc.each:\n"); print(conc.each);
    time9 <- Sys.time()
    cat("\nFinish before mergeConcTable part!\n")
    cat(difftime(time9, time5, units = "auto"))
    # one or multiple samples;
    final.Concentration <- mergeConcTable( conc.each )
    time7 <- Sys.time()
    cat("\nFinish mergeConcTable part!\n")
    cat(difftime(time7, time5, units = "auto"))
    ### changed to be used in mergeConcTale();
    
    #if (length(conc.each) >= 2) {
    #    final.Concentration <- mergeConcTable( conc.each )
        # cat("final.Concentration (>=2):\n"); print(final.Concentration)
   # } else {
        # final.Concentration <- conc.each;
    #    final.Concentration <- as.data.frame( conc.each );
        # cat("final.Concentration (=1):\n"); print(final.Concentration)
    #}
    
    

    ## Collect concentrations only and combine all samples
    ## ==================================================================================================
    ## Combining  all concentration and Generate Files for Concentration Table with Sample ID
    ## use the "final.Concentration" data frame
    if ( (internalStd != 'NONE') && !is.null(internalStd) ) {
        # ofile.merged <- paste(basename(sampleFileDir),"_profiledAll.csv", sep='')
        ofile.merged <- "profiled_All.csv"
        cat("\n## Generate Files for Concentration Table for All Profiled Samples:", ofile.merged, "\n")
        ## order by RI as in library; merge compound name with RI and sort by RI

        if(is.null(final.Concentration)) {
            if (DEBUG) {
                cat("\n\n## final.Concentration\n")
                cat("nrow:", 0, "\n")
            }
            #stopMessage("\n\n## Error: there is no final concentration data (empty or not identified at all)\n\n")
        }
        
        if(DEBUG) {
            cat("\n\n## merging Concentration\n");
            cat("## final.Concentration before merged:\n"); 
            # print(tail(final.Concentration));
            print(final.Concentration);
        }
        
        # Calibration Curve Lib and Identify Lib need to match the HMDB ID and Compound Name
        # final.Concentration <- merge(lib.calicurv[,c('HMDB_ID','Compound','SeqIndex')], final.Concentration, by=c('HMDB_ID', 'Compound'), sort=FALSE, all.x=TRUE)
        if(!is.null(final.Concentration)) {
            final.Concentration <- final.Concentration[, - which(names(final.Concentration) == "Compound")] # excluding for merge with Lib.CaliCurve
            final.Concentration <- merge(lib.calicurv[,c('HMDB_ID','Compound','SeqIndex')], final.Concentration, by=c('HMDB_ID'), sort=FALSE, all.x=TRUE)
        }
        time8 <- Sys.time()
        cat("\nFinish final.Concentration part!\n")
        cat(difftime(time8, time5, units = "auto"))
        
        # cat("\n\n## final.Concentration at line 409:\n"); print(final.Concentration);

        
        # cat("## final.Concentration after merged with lib:\n"); print(final.Concentration)
        if (DEBUG) { 
            cat("## final.Concentration after merged with lib:\n"); 
            # print(tail(final.Concentration)) 
            print(final.Concentration) 
            cat("## lib.calicurv:\n"); print(lib.calicurv[,c('HMDB_ID','Compound','SeqIndex')])        
        }
        
        # print(lib.calicurv$Compound)
        # cat("## diff - compname1:\n"); print(setdiff(lib.calicurv$Compound, final.Concentration$Compound))
        # cat("## diff - compname2:\n"); print(setdiff(final.Concentration$Compound, lib.calicurv$Compound))
        # cat("## intersect - compname1:\n"); print(intersect(lib.calicurv$Compound, final.Concentration$Compound))
        if(!is.null(final.Concentration)) {
            final.Concentration <- final.Concentration[order(as.integer(as.character(final.Concentration$SeqIndex)), decreasing=FALSE), ]
            # cat("## final.Concentration after order:\n"); print(final.Concentration)
  
            rm.SeqIndex <- which(names(final.Concentration) == "SeqIndex")
            final.Concentration <- final.Concentration[ , - rm.SeqIndex]
            rownames(final.Concentration) <- c(1:nrow(final.Concentration))
            # write.table(final.Concentration, file="final.Concentration.405.csv", sep=",", col.names=TRUE, quote=TRUE)
        
            if( DEBUG ) { cat("\n\n## final.concentration at 433:\n"); print(final.Concentration)  }
        
            write.table( t(final.Concentration), file=ofile.merged, sep=",", col.names=FALSE, quote=TRUE)
        }
        else{
            file.create(ofile.merged)
        }
        cat("\n==============================================================\n")
        cat("## Done:", length(fileList$sampleFiles), "spectrum files\n")
        cat("==============================================================\n\n")
        
    } else {
        ## OPTION or if no internal std, then just report without quantification 
        if (internalStd == 'NONE') {
            ostr <- paste("\n\n################################################################################\n",
                          "## NOTE: The profiled_ALL file was not generated due to NO internal Standard\n", 
                          "################################################################################\n", sep='')
        } else {
            ostr <- paste("\n\n################################################################################\n",
                          "## NOTE: The profiled_ALL file was not generated due to missing internal Standard\n", 
                          "\t internalStd:", internalStd.in,"\n",
                          "################################################################################\n", sep='')
        }
        ofile.merged <- "profiled_All.csv"
        cat(ostr, file=ofile.merged)            
        cat(ostr)
    }

    ofile.note <- paste(basename(sampleFileDir),"_note.txt", sep='')
    saveProfilingInfo(ofile.note, Version, MF_THRESHOLD)   
    
    if (DEBUG.TIME) { spentTime(t.beg, "very end") }
    time6 <- Sys.time()
    cat("\nFinish second part!\n")
    difftime(time6, time5, units = "auto")


}

time2 <- Sys.time()
difftime(time2, time1, units = "auto")
