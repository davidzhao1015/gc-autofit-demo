###########################################################################################
## Functions for the Option and Global Environment
## Author: Beomsoo Han
## Latest Update: 20150325
############################################################################################

showProgramInfo <- function(VersionStr)
{
  # show version string
  cat("\n\n#########################################################\n")
  cat("##   GC-AutoFit: Automatic Profiling GC-MS Spectrum\n")
  cat("##   Version:", VersionStr,"\n")
  cat("#########################################################\n\n")
}

parseArgs <- function(x) {
  strsplit(sub("^--", "", x), "=")
}

helpMessage <- function() {
  cat("
      The R Script
 
      Requested Arguments:
        --infiledir='spectrum file directory' 
        --userlib='user_library_file.csv'       # only need if not use internal lib
        --usercal='user_calibration_file.csv'   # only need if not use internal lib
        --internalstd='Compound Name' or 'None' # Internal Standard Compound Name in user library and calibration curve
                                                # e.g., Cholesterol or Succinate-D4
        --process='PREPROCESSING' or 'PROFILING'  # set a processing mode 

      Optional Arguments:
        --lib.internal=URINE  # SERUM, URINE, SALIVA, ... default=Urine; 
        --selectedCmpd=NULL # List of HMDB IDs (comma separated), default=NULL (which means ALL) 
                            # Selected HMDB IDs should be a subset of the selected internal library
        --useblank=TRUE   # boolean (TRUE/FALSE); toggle for using Blank Spectrum
        --MFscore=400     # set MF score threshold (Default: 400)
        --RIoffset=0.03   # set RI variation/offset (Default: 0.03)
        --AlkaneRT='10,11,12,13,14,...20,22,24,26,28,30,32,34'  # set RI variation/offset (Default: 0.03)
        --outdir=<user_defined_output_directory>    # Assign a 'user defined output directory' (recommended to use full path). 
                                                      It is allowed to use the recursive directory creation 
        --infoFileDir=<information_file_directory>  # Assign the directory of the information files (Alkane Standard 
                                                      and Blank Sample's Profiles). It is required for PROFILING process. 
        --CalCurveVersion='NEW'  # Cholesterol - New & Old Calibration Curves (Temporary to compare) 
        --help  # print this text
 
      Example:
      ## Processing
      > Rscript apgcms_main.R --infiledir='./data/sample_sep09' --userlib='user_profiledb.csv' --usercal='user_calibration.csv' 
               --internalstd='Ribitol' --process='PREPROCESSING' --useblank=TRUE --MFscore=400 --RIoffset=0.03  

      ## Profiling
      > Rscript apgcms_main.R --infiledir='./data/sample_sep09' 
               --lib.internal='SERUM' --selectedCmpd='HMDB00067,HMDB00094,HMDB00115,HMDB02142'
               --internalstd='Cholesterol' --process='PROFILING'
               --outdir='/Users/gcms/example/output_profiling'
               --useblank=TRUE --MFscore=400 --RIoffset=0.03
               
      \n\n")
  
  q(save="no")  
}


# Define Global Variables
# Parsing arguments
parsingArgument <- function(args.in)
{
  argsDF <- as.data.frame(do.call("rbind", parseArgs(args.in)))
  argsL <- as.list(as.character(argsDF$V2))
  names(argsL) <- argsDF$V1

  # cat("argsDF:\n"); print(argsDF);
  # cat("argsL:\n"); print(argsL);
  
  if(is.null(argsL$infiledir)) {
    showErrMessage("  Error in argument:\n\t There is no input spectrum file directory")
    helpMessage()
  } else {
    sampleFileDir <<- normalizePath(argsL$infiledir) # should be absolute path
  }
  
  
  if(is.null(argsL$lib.internal)) {  ## use user's own library
      USE_INTERNAL_LIBRARY <<- 'NONE' # argsL$lib.internal
      
      if(is.null(argsL$userlib)) {
          showErrMessage("  Error in argument:\n\t There is no user defined library file")
          helpMessage()
      } else {
          userLibFile <<- argsL$userlib
      }
      
      if(is.null(argsL$usercal)) {
          showErrMessage("  Error in argument:\n\t There is no user defined calibration curve file")
          helpMessage()
      } else {
          userCalFile <<- argsL$usercal
      }
    
  } else { ## use internal library
      # SERUM, URINE, SILIVA, ... default = URINE
      if (argsL$lib.internal %in% c('SERUM', 'URINE', 'SALIVA', 'MILK')) {
          USE_INTERNAL_LIBRARY <<- argsL$lib.internal
      
          # get user selected compound list (HMDB IDs with comma separate)
          if(! is.null(argsL$selectedCmpd))    {
              USER_SELECTED_CMPDS <<- argsL$selectedCmpd;
              # cat("\n\n## USER_SELECTED_CMPDS:\n"); print(USER_SELECTED_CMPDS);
          } else {
              USER_SELECTED_CMPDS <<- NULL;
              # showErrMessage("  No USER_SELECTED_CMPDS: Using All Compounds in the Library\n");
          }
          
      } else {
          showErrMessage("  Error in argument:\n\t see the help to correctly use the internal library option (lib.internal)")
          helpMessage()
      } 
  }
  
  # Internal Std in Library (HMDB ID or Compound Name; Final, Compound Name will be used for the analysis)
  if(is.null(argsL$internalstd)) {
       showErrMessage("  Error in argument:\n\t There is no internal standard compound. 
                   If you want profile only, then please set with 'None' ")
       helpMessage()
  } else {
       internalStd.in <<- argsL$internalstd
  }
  
  # use calibration curve of Cholesterol (OLD or NEW); Only for Urine/Organic acids
  # cat("argsL$CalCurveVersion:", argsL$CalCurveVersion,"\n")
  if(is.null(argsL$CalCurveVersion) ) {
    USE_NEW_CALCURVE <<- TRUE
  } else {
    if(argsL$CalCurveVersion == "NEW") {
        USE_NEW_CALCURVE <<- TRUE  
        if(ISDEBUG) { cat("## calibration curve: use NEW Urine/Organic Acid Calibration Curves") }
    } else {
        USE_NEW_CALCURVE <<- FALSE
        if(ISDEBUG) { cat("## calibration curve: use OLD Urine/Organic Acid Calibration Curves") }
    }
  }
  
  if(argsL$process %in% c('PREPROCESSING','PROFILING')) {
    processOption <<- argsL$process;
  } else {
    showErrMessage("  Error in argument:\n\t see the help to correctly use the process option")
    helpMessage()
  }
  
  # optional arguments
  if(is.null(argsL$useblank) ) {
    # showErrMessage("  Warning in argument:\n\t Blank file option is not assigned. Program will use default (TRUE)")
    USE_BLANK_SPECTRUM <<- TRUE
  } else {
    USE_BLANK_SPECTRUM <<- as.logical(argsL$useblank)
  }
  
  if(is.null(argsL$MFscore)) {
    # showErrMessage("  Warning in argument:\n\t MFscore option is not assigned. Program will use default 400")
    MF_THRESHOLD <<- MF_THRESHOLD_DEFAULT
  } else {
    MF_THRESHOLD <<- as.numeric(argsL$MFscore)
  }

  if(is.null(argsL$RIoffset)) {
    # showErrMessage("  Warning in argument:\n\t RI offset option (RIoffset) is not assigned. Program will use default 0.03")
    RI.Variation <<- RI.VARIATION.DEFAULT     
  } else {
    RI.Variation <<- as.numeric(argsL$RIoffset)
  }
  
  if(is.null(argsL$AlkaneRT)) {
    user.AlkaneRT <<-  NULL
  } else {
    user.AlkaneRT <<- as.numeric(unlist(strsplit(argsL$AlkaneRT,",")))
    if(ISDEBUG) { cat("## user.AlkaneRT:"); print(user.AlkaneRT) }
  } 
  
  if(is.null(argsL$outdir)) {
    # default output directory
    user.outdir <<- paste(dirname(sampleFileDir), "/", sub("_input","",basename(sampleFileDir)), '_result', sep='')
  } else {
    user.outdir <<- argsL$outdir # expected full path
    if(ISDEBUG) { cat("## user.outdir:"); print(user.outdir) }
  }
  
  # In profiling process, it needs to have the Alkane and Blank information from the Preprocessing process.
  if( processOption == 'PROFILING' ) {
    # cat("\n\n## argsL$infoFileDir:"); print(argsL$infoFileDir)
    if(is.null(argsL$infoFileDir)) {
      showErrMessage("  Error in argument (--infoFileDir):\n\t Alkane Standard and Blank Profiled Information are required \n\t with '--infoFileDir' option")
      helpMessage()
    } else {
      infoFileDir <<- normalizePath(argsL$infoFileDir)
      if(ISDEBUG) { cat("\n\n## infoFileDir (will be created):"); print(infoFileDir) }
    }
  }
}


showErrMessage <- function(s) {
  cat("\n################################################################################\n")  
  cat(s)
  cat("\n################################################################################\n") 
  
  amsg <- "\n################################################################################\n"
  cat(file=File.ErrorLog, amsg, append=TRUE)
  cat(file=File.ErrorLog, paste(s), append=TRUE)
  cat(file=File.ErrorLog, amsg, append=TRUE)
}

stopMessage <- function(s, stopflag=TRUE) {
  if (stopflag == TRUE) {
      amsg <- "\n\n## STOP due to the following reason ##\n"
      cat(file=File.ErrorLog, paste(amsg, s), append=TRUE)
      
      stop(s, call. = FALSE )
  } else {
      amsg <- "\n\n## Warning - see the following reason ##\n"
      cat(file=File.ErrorLog, paste(amsg, s), append=TRUE)
  }
  
}

## set running model to check each procedure
# setMode(FALSE); print(DEBUG)
setDebugMode <- function(mode = TRUE) {
  if (mode) {
    cat("## DEBUG mode is ON\n")
  } else {
    cat("## DEBUG mode is OFF\n")
  }
  DEBUG <<- mode;
}

setSampleType <- function(internalLibType)
{
  if (internalLibType == 'SERUM') {
    SampleType <- 1
  } else if (internalLibType == 'URINE')  {
    SampleType <- 2
  } else if (internalLibType == 'SALIVA') {
    SampleType <- 3
  } else if (internalLibType == 'MILK') {
    SampleType <- 4
  } else {
    showErrMessage("  Error in argument:\n\t see the help to correctly use the internal library option (lib.internal)")
    helpMessage()
  }
  
  return(SampleType)
}

## assign calibration curve library
setCalCurveType <- function(internalLibType, strInternalStd)
{
    if (strInternalStd == 'None') {
        calCurveType <- 0
        return (calCurveType)
    }
  
    if (internalLibType == 'SERUM') {
        calCurveType <- CALICURVE_IDX_SERUM
    } else if (internalLibType == 'URINE') {
        if(length(grep('Cholesterol',strInternalStd)) > 0) {
            calCurveType <- CALICURVE_IDX_URINE_CHOLESTEROL
        } else if (length(grep('Succinate-D4', strInternalStd)) > 0) {
            calCurveType <- CALICURVE_IDX_URINE_SUCCINICACIDD4
        } else {
            stopMessage(paste("  Error in argument:\n",
                                 "\t Please check the internal standard [",strInternalStd,"]",
                                 "\n\t that should be exist in the selected biofluid library [",internalLibType,"]."
                                 , sep=''))
            # helpMessage()
        }
    } else if (internalLibType == 'SALIVA') {
        calCurveType <- CALICURVE_IDX_SALIVA
    } else {
        stopMessage("  Error in argument:\n\t see the help to correctly use the internal Calibration Curve option (lib.internal)")
    }

    return (calCurveType)
}

# get compound Info library
getLibInfo <- function(fname.lib)
{    
    lib.table <- read.csv(file=fname.lib, head=TRUE, sep=",", quote = "\"");
    if(nrow(lib.table) == 0) {
      stopMessage( paste("Cannot load the library",basename(fname.lib)))
    }
    
    return(lib.table)  
}

## get the file list from sample file dir.
## alkane std is always the first one in file_list, only one alkane std file in 
## one case.
## args:  sample_file_dir
## return: a vetor of files
## infileDir <- dirCDFSample
get_file_list <- function(infileDir, procStatus) {
  
  if(DEBUG) cat("## 'get_file_list' from ", infileDir,"\n\n")
  
  ## .CDF or .mzXML    
  file_list_tmp <- list.files(path = infileDir, pattern=".mzXML|.CDF", ignore.case=TRUE, full.names=TRUE);
  if(length(file_list_tmp) > 0) {
    alk_file_index <- grep("ALKSTD", basename(file_list_tmp), ignore.case=TRUE, perl=TRUE, value=FALSE);  
    if (length(alk_file_index) == 0) {
      alk_file_index <- grep("ALK", basename(file_list_tmp), ignore.case=TRUE, perl=TRUE, value=FALSE);
      if (procStatus == "PREPROCESSING" & length(alk_file_index) == 0) {
          stopMessage("Could not find any Alkane Standard file (filename: ALK* or ALKSTD*)")
      }    
    }
    blank_file_index <- grep("BLANK", basename(file_list_tmp), ignore.case=TRUE, perl=TRUE, value=FALSE);  
    if (length(blank_file_index) == 0) {
      blank_file_index <- grep("BLK", basename(file_list_tmp), ignore.case=TRUE, perl=TRUE, value=FALSE);
      if (procStatus == "PREPROCESSING" & length(blank_file_index) == 0) {
        cat("Could not find any Blank file (filename: BLK* or BLANK*)")
      }    
    }
  } else {
      stopMessage("There is no CDF/mzXML files")
  }
  
  ## BHAN: if more than two alkane files, then just use first one and exclude others.give message note
  ## if (length(alk_file_index) >1) stop("More than one alkane standard file in the case. Check!!!") 
  if (DEBUG) { cat("filelist:\n"); print(file_list_tmp) }
  if(length(alk_file_index) == 0L) {    
      alkaneFile <- NULL
  } else {
      if (length(alk_file_index) > 1) {
          # stop("More than one alkane standard file in the case. Check!!!") 
          if(DEBUG) {
              cat("\n\tCaution: More than one alkane standard files.\n\tProgram will use only first one!\n\n");       
          }
          alkaneFile <- file_list_tmp[alk_file_index[1]]
      } else {
          alkaneFile <- file_list_tmp[alk_file_index]
      }
      # alkaneFiles <- file_list_tmp[alk_file_index]
  }

  if(length(blank_file_index) == 0L) {
      blankFile <- NULL
  } else {
      if (length(blank_file_index) > 1) {
        if(DEBUG) {
            cat("\n\tCaution: More than one blank files.\n\tProgram will use only first one!\n\n"); 
        }
        blankFile <- file_list_tmp[blank_file_index[1]]
      } else {
        blankFile <- file_list_tmp[blank_file_index]
      }
      # blankFiles <- file_list_tmp[blank_file_index]
  }
  
  alkaneBlankList <- c(alk_file_index, blank_file_index)
  if( length(alkaneBlankList) > 0) {
      # cat("alkaneBlankList is not null\n"); 
      sampleFiles <- file_list_tmp[-alkaneBlankList]    
  } else {
      if (DEBUG) { cat("alkaneBlankList is null\n") }
      sampleFiles <- file_list_tmp
  }
  
  # cat("sampleFiles:\n"); print(sampleFiles)
  # print(list ( alkaneFile = alkaneFile, blankFile=blankFile, sampleFiles = sampleFiles ))
  
  return( list ( alkaneFile = alkaneFile, blankFile=blankFile, sampleFiles = sampleFiles ) )
  
}


#save profiling information
saveProfilingInfo <- function(fname, VersionStr, mfScore)
{
  outstr <- paste("## Done by GC-AutoFit (Version", VersionStr,")", sep='') 
  outstr <- paste(outstr, "\n  - Date:", date())  
  outstr <- paste(outstr, "\n  - Match Factor Score: >", mfScore)    
  outstr <- paste(outstr, "\n\n\n")
  cat(outstr, file=fname, append=FALSE)
  cat(outstr)
}


## HMDB ID or Compound Name; No case senstive  
ucfirst <- function (str) {
    paste(toupper(substring(str, 1, 1)), tolower(substring(str, 2)), sep = "")  
}

ucHMDB <- function (str) {
    paste(toupper(substring(str, 1, 4)), tolower(substring(str, 5)), sep = "")
}    

# return Internal Standard Compound Name
getInternalStdCmpdName <- function (alib, std.str) {
    if ( toupper(substring(std.str,1,4)) == "HMDB" ) {      
        cmpdname <- as.character( alib[which(alib$HMDB_ID == ucHMDB(std.str)), "Compound"] )
        cat("\n\n## Matched Internal Standard Compound for HMDB ID:", std.str,"is", cmpdname,"\n")
    } else {
        # cat("grep length:", length(grep("ISTD", std.str)), "\n")
        if (length(grep("ISTD", std.str)) == 0) {
            cat("# Add (ISTD) to internal std compound name (in parameter)\n\n")
            std.str <- paste(std.str, " (ISTD)", sep='')
        }
        cmpdname <- as.character( alib[which(tolower(alib$Compound) == tolower(std.str)), "Compound"] )
    } 

    if (length(cmpdname) == 1) {  
        return (cmpdname) 
    } else {  return ( NULL ) }
}

# mass (m/z) values convert to integer
toIntMZ.sample <- function(mz)
{
    # mz.num <- as.numeric(unlist(strsplit(as.character(mz.vec), split=" ")))
    mz.int <- ifelse( (mz - floor(mz)) <= 0.7, floor(mz), ceiling(mz))
    # paste(noquote(mz.int), collapse=" ")
    
    return (mz.int)
}

spentTime <- function(t.beg, ostr=NULL)
{
  td = as.numeric(Sys.time() - t.beg, "secs")
  cat("## Spending Running Time :", td, "sec", ifelse(!is.null(ostr), paste(" at", ostr), ""), "\n")
}

