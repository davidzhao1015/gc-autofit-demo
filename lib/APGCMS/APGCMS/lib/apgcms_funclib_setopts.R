###########################################################################################
## Functions for the Option and Global Environment
## Author: Beomsoo Han
## Latest Update: 20150325
############################################################################################

showProgramInfo <- function(VersionStr)
{
  # show version string
  cat("##### GC-AutoFit: Automatic Profiling GC-MS Spectra #####\n")
  cat("## Version:", VersionStr,"\n\n")
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
        --internalstd='Compound Name' or 'NONE' # Internal Standard Compound Name in user library and calibration curve
        --process='PREPROCESSING' or 'PROFILING'  # set a processing mode 

      Optional Arguments:
        --lib.internal=NONE  # NONE, SERUM, URINE, SALIVA, ... default = NONE; 
        --useblank=TRUE   # boolean (TRUE/FALSE); toggle for using Blank Spectrum
        --MFscore=400     # set MF score threshold (Default: 400)
        --RIoffset=0.03   # set RI variation/offset (Default: 0.03)
        --AlkaneRT='10,11,12,13,14,...20,22,24,26,28,30,32,34'  # set RI variation/offset (Default: 0.03)
        --outdir=<user_defined_output_directory>    # Assign a 'user defined output directory' (recommended to use full path). 
                                                      It is allowed to use the recursive directory creation 
        --infoFileDir=<information_file_directory>  # Assign the directory of the information files (Alkane Standard 
                                                      and Blank Sample's Profiles). It is required for PROFILING process. 



      --help  # print this text
 
      Example:
      > Rscript test.R --infiledir='./data/sample_sep09' --userlib='user_profiledb.csv' --usercal='user_calibration.csv' 
               --internalstd='Ribitol' --process='PREPROCESSING' --useblank=TRUE --MFscore=400 --RIoffset=0.03  
      > Rscript test.R --infiledir='./data/sample_sep09' --lib.internal='SERUM' --internalstd='Cholesterol' --process='PROFILING'
               --outdir='/Users/gcms/example/output_profiling'
      \n\n")
  
  q(save="no")  
}

showErrMessage <- function(s) {
  cat("\n################################################################################\n")  
  cat(s)
  cat("\n################################################################################\n") 
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


# get compound Info library
getLibInfo <- function(fname.lib)
{    
    lib.table <- read.csv(file=fname.lib, head=TRUE, sep=",", quote = "\"");
    if(nrow(lib.table) == 0) stop( paste("Cannot load the library",basename(fname.lib)) )
    
    return(lib.table)  
}

## get the file list from sample file dir.
## alkane std is always the first one in file_list, only one alkane std file in 
## one case.
## args:  sample_file_dir
## return: a vetor of files
## infileDir <- dirCDFSample
get_file_list <- function(infileDir){
  
  if(DEBUG) cat("'get_file_list' from ", infileDir,"\n\n")
  
  ## .CDF or .mzXML    
  file_list_tmp <- list.files(path = infileDir, pattern=".mzXML|.CDF", ignore.case=TRUE, full.names=TRUE);
  if(length(file_list_tmp) > 0) {
    alk_file_index <- grep("ALKSTD", basename(file_list_tmp), ignore.case=TRUE, perl=TRUE, value=FALSE);  
    if (length(alk_file_index) == 0) {
      alk_file_index <- grep("ALK", basename(file_list_tmp), ignore.case=TRUE, perl=TRUE, value=FALSE);
      if (length(alk_file_index) == 0) {
        stop("Could not find any Alkane Standard file (filename: ALK* or ALKSTD*)")
      }    
    }
    blank_file_index <- grep("BLANK", basename(file_list_tmp), ignore.case=TRUE, perl=TRUE, value=FALSE);  
    if (length(blank_file_index) == 0) {
      blank_file_index <- grep("BLK", basename(file_list_tmp), ignore.case=TRUE, perl=TRUE, value=FALSE);
      if (length(blank_file_index) == 0) {
        cat("Could not find any Blank file (filename: BLK* or BLANK*)")
      }    
    }
  } else {
    stop("There is no CDF/mzXML files")
  }
  
  ## BHAN: if more than two alkane files, then just use first one and exclude others.give message note
  ## if (length(alk_file_index) >1) stop("More than one alkane standard file in the case. Check!!!") 
  if (length(alk_file_index) > 1) {
    # stop("More than one alkane standard file in the case. Check!!!") 
    if(DEBUG) 
      cat("\n\tCaution: More than one alkane standard files.\n\tProgram will use only first one!\n\n"); 
    
    alkaneFile <- file_list_tmp[alk_file_index[1]]
  } else {
    alkaneFile <- file_list_tmp[alk_file_index]
  }
  # alkaneFiles <- file_list_tmp[alk_file_index]
  
  if (length(blank_file_index) > 1) {
    if(DEBUG) 
      cat("\n\tCaution: More than one blank files.\n\tProgram will use only first one!\n\n"); 
    blankFile <- file_list_tmp[blank_file_index[1]]
  } else {
    blankFile <- file_list_tmp[blank_file_index]
  }
  # blankFiles <- file_list_tmp[blank_file_index]
  
  sampleFiles <- file_list_tmp[-c(alk_file_index, blank_file_index)]
  
  # return( list (alkaneFile = alkaneFile, blankFile=blankFile, sampleFiles = sampleFiles ) )
  return( list (alkaneFile = alkaneFile, blankFile=blankFile, sampleFiles = sampleFiles ) )
}


#save profiling information
saveProfilingInfo <- function(fname, VersionStr)
{
  outstr <- paste("## GC-AutoFit Version", VersionStr) 
  outstr <- paste(outstr, "\n  - Date:", date())
  outstr <- paste(outstr, "\n")
  cat(outstr, file=fname, append=FALSE)
  cat(outstr)
}


