############################################################################################
## To updating Library of the automatic GC-MS profiling
## update: Nov 13, 2014
############################################################################################

## Loading Packages
## Install packages:
##    source("http://bioconductor.org/biocLite.R")
##    biocLite(module)
##    biocLite("BiocUpgrade")
##    biocLite("xcms")

# library(xcms)
suppressMessages(require(xcms));

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


# file.alkane <- fileList$alkaneFile
extractAlkaneStdInfo <- function(file.alkane) {
  if (DEBUG)  cat("\n## Reading data from Alkane standard spectrum (", basename(file.alkane), ")\n")
  
  #### xset <- xcmsSet(file.alkane, method='centWave',ppm=1,snthresh=10,peakwidth=c(1,10))
  ## profMethod: bin, binlin, binlinbase, or intlin.
  # xset <- xcmsRaw(file.alkane, profstep=1, profmethod='bin', scanrange=NULL)  ## without smooth  
  xset <- xcmsRaw(file.alkane, profstep=1, profmethod='binlin', scanrange=NULL) ## smooth  
  return(xset)  
}

# file.alkane <- fileList$alkaneFile
extractBlankInfo <- function(file.blank) {
  if (DEBUG)  cat("\n## Reading data from Blank spectrum (", basename(file.blank), ")\n")  
  xset <- xcmsRaw(file.blank, profstep=1, profmethod='binlin', scanrange=NULL) ## smooth  
  return(xset)  
}


# xset <- xset.alkane
extract_peak_list_alkane2 <- function(xset, numOfAlkane, ctype="EIC", offset=7, plotFile=TRUE)  {
  
  ## This is the important part to detect peak correctly; it depends on mzrange
  # mzrange <- t(range(xset@peaks[,"mz"]))            
  mzrange <- t(range(xset@mzrange))
  rtrange <- t(range(xset@scantime))
  # eic_obj <- getEIC(xset, mzrange=mzrange, rtrange=rtrange, rt="corrected")
  # eic_obj <- getEIC(xset, mzrange=mzrange, rtrange=rtrange, rt="raw")  
  eic_obj <- getEIC(xset, mzrange=mzrange, rtrange=rtrange)   ## when xset is xcmsRaw class
  
  # EIC plot generate & save  
  if ( plotFile ) {
      sampleFile <- sub(".mzXML|.CDF", "", basename(xset@filepath), ignore.case=TRUE)  
      # png(filename = paste("Plot_", ctype,"_", sampleFile,".png", sep=''), width = 1000, height = 800, units = "px", pointsize = 10)
      png(filename = paste("Plot_EIC_", sampleFile,".png", sep=''), width = 1000, height = 800, units = "px", pointsize = 10)
        plotEIC(xset, mzrange=mzrange, rtrange=rtrange) ## same as chemstation
      dev.off()
  }
  
  ## get the RT and Intensity from EIC object
  rt_int_matrix <- eic_obj@eic[1][[1]][[1]]         
  if (is.null(rt_int_matrix[, "intensity"])) stop(paste("rt_int_matrix is null in ", xset@filepaths[1]))
  
  ## select significant peaks & screening apexes only to mapping compounds
  ## use TIC instead of EIC
  if(ctype=="TIC") {
    rt_peak <- extract_RT_forEachPeak(rt_int_matrix, offset, ctype="TIC", xset); 
    
    ## exclude noise peaks
    idx.max <- which(rt_peak$intensity == max(rt_peak$intensity)) ## C20
    
    peakIntSorted <- sort(rt_peak[1:idx.max-1, "intensity"], decreasing=TRUE)
    minPeakIntensity <- peakIntSorted[10] # assume the peak start from C10        
    threshold <- round(0.5 * minPeakIntensity, 0)    
    idx.remove <- which(rt_peak[1:idx.max, "intensity"] < threshold)
    if(length(idx.remove) > 0) {
      if(DEBUG) {
          cat("## Alkane Standard Peaks: some peaks are removed\n")
          cat("\t = threshold intensity:", threshold, "\n")
      }
      rt_peak <- rt_peak[-idx.remove, ]   
      rownames(rt_peak) <- c(1:nrow(rt_peak))
    }
  } else {
    rt_peak <- extract_RT_forEachPeak(rt_int_matrix, offset, ctype); 
    if (DEBUG) print(rt_peak/60);    
  }
    
  if( FALSE )  {
    sampleName <- basename(xset@filepath);  
    plot(round(rt_peak[,1]/60,1), rt_peak[,2], type="h", xaxp=c(12,60,n=48), xlim=c(12,60), ylim=c(0,max(rt_peak[,2])), main=sampleName ) 
  }
  
  return(rt_peak)
}


# extract_rt_w_highest_peaks_on_outliers --> extract_RT_forEachPeak_Alkane
# Use TIC or EIC
extract_RT_forEachPeak <- function(rt_int_matrix, offset=7, ctype="EIC", xset=NULL) {

  if( ctype == "TIC") {
    if(is.null(xset)) {
      stop("TIC type analysis is required raw dataset (xset)")
    }
    # rtint.o <- rt_int_matrix
    rt_int_matrix <- data.frame(rt=xset@scantime, intensity=xset@tic)
  } 
  
  # cat("## xset:\n")
  # print(str(xset))
  
  ## to reduce size, using cut off values is very important to remain significant peak      
  dstat <- summary(rt_int_matrix[,"intensity"])      
  if (FALSE & DEBUG) {
      cat("## summary of dstat:\n"); print(dstat)
      offset <- 1.5
      cutoff <- dstat[3] + offset * (dstat[5] - dstat[2]) # median        
      cat("cutoff:", cutoff, "offset:", offset, "\n")
      
      offset <- 1.0
      cutoff <- dstat[3] + offset * (dstat[5] - dstat[2]) # median        
      cat("cutoff:", cutoff, "offset:", offset, "\n")      
  }

  if (offset > 2) {
      # for alkane standard
      cutoff <- dstat[3] + offset * (dstat[5] - dstat[2]) # median        
  } else {
      # for sample and blank spectrum
      cutoff <- dstat[3] # median
  }
  idx <- which(rt_int_matrix[,"intensity"] > cutoff)
  if (DEBUG) cat("cutoff:", cutoff, "offset:", offset, "\n")
  # cat("rt_int_matrix/60:\n"); print(rt_int_matrix[1:20,]/60)
        
  # plot(rt_int_matrix, type="l")
  # plot(rt_int_matrix[idx, ], type="o", ylim=c(0,max(rt_int_matrix[idx, 2])))
  
  ## for adding LIN/OLE or other ?? RT:39.98 in Serum 
  ## this need to be test with new algorithm
  ## ========================================================================================
  # if (FALSE & SampleType == 1)  { ## serum in case --> should be updated with molecular weight
  #  rtRange.lowerIntensity <- which(rt_int_matrix[,"rt"] > 2390 & rt_int_matrix[,"rt"] < 2410)
  #  # plot(rt_int_matrix[rtRange.lowerIntensity, ])
  #  # rt_int_matrix[rtRange.lowerIntensity, ]
  #  remainPeakIdx <- which( rt_int_matrix[rtRange.lowerIntensity, "intensity"] > cutoff)
  #  # true_false_vec[rtRange.lowerIntensity[remainPeakIdx]] <- TRUE
  #}  
  
  # peak_matrix <- rt_int_matrix[c(idx,remainPeakIdx), ]  
  peak_matrix <- rt_int_matrix[idx, ]
  # plot(peak_matrix, type="h")
  ## if (DEBUG) { cat("## peak_matrix:\n"); print(peak_matrix)  }
    
  # detect the RT of each peak
  # peak_true_false_vec <- get_peak_true_false_vec2(peak_matrix)  
  peak_matrix <- get_RTofeachPeak(peak_matrix, wsize=5) 
  
  if(FALSE) {
    plot(peak_matrix, type="h")
  }
  
  return (peak_matrix)
}


# get_peak_true_false_vec2 <- get_RTofeachPeak
get_RTofeachPeak <- function(peak_matrix, wsize=5) {
  
  ## step 1: detect peak's apex
  rt <- peak_matrix[,"rt"]
  intensity <- peak_matrix[,"intensity"]
    
  nRT <- length(rt)     
  rtPeak <- NULL 
  intPeak <- NULL 
  intPrv <- intensity[1] 
  for (i in 1:nRT) { 
    intNext<- ifelse(i==nRT,intensity[nRT],intensity[i+1]) 
    if(intPrv < intensity[i] & intensity[i] > intNext) { #found local peak 
      rtPeak<- c(rtPeak,rt[i]) 
      intPeak<- c(intPeak,intensity[i]) 
    } 
    intPrv<- intensity[i] 
  } 
  
  apexes <- data.frame(rt=rtPeak, intensity=intPeak)
  if(FALSE & DEBUG) { 
      cat("apexes:\n"); print(cbind(rt_min=round(apexes$rt/60,2), apexes) );  
  }
  # plot(apexes) 
  
  ##  step 2: select only apex within a peak using window size
  num <- length(rtPeak);
  rt.new <- NULL;
  int.new <- NULL;
  for(i in 1:num) {
    tmp <- apexes[which(apexes$rt > (apexes$rt[i] - wsize) & apexes$rt < (apexes$rt[i] + wsize)), ];
    # cat("tmp\n"); print(tmp)
    if(nrow(tmp) > 1) {
      idx.max <- which(tmp$intensity == max(tmp$intensity))
      if( length(which(rt.new == tmp$rt[idx.max])) == 0) {          
        rt.new <- c(rt.new, tmp$rt[idx.max]);
        int.new <- c(int.new, tmp$intensity[idx.max]);
      }
    } else {
      rt.new <- c(rt.new, apexes$rt[i]);
      int.new <- c(int.new, apexes$intensity[i]);
    }
  }
  return( data.frame(rt=rt.new, intensity=int.new) );  
}


## Get the Alkane Standard and return number of alkane, first alkane RT, last alkane RT.
## It will be used for ehecking whether it covers full range or not
check_alkane_std <- function(alkane_std_rt) {  
  ## Differerent types of alkane
  ## 1) short than sample, 2) mixed continuous carbon # and even # (eg., c(10:20,22,24,...))
  ## 3) smaller than 11
  
  RTmin <- min(alkane_std_rt[, 'ALKRT'])
  RTmax <- max(alkane_std_rt[, 'ALKRT'])
  numOfAlkanes <- nrow(alkane_std_rt)
  
  ## this result will be used for without alkane std
  RImin <- calc_RI(RTmin, alkane_std_rt)
  RImax <- calc_RI(RTmax, alkane_std_rt)
  # cat("RImin:", RImin, "RImax:", RImax, "\n")
  
  return( list(RTmin=RTmin, RTmax=RTmax, RImin=RImin, RImax=RImax, numOfAlkanes=numOfAlkanes) )
}


calc_RI <- function(sample_rt, alkane_std_rt) {
  # alkane_std_rt <- alkane_std
  # sample_rt <- 1945.502
  # sample_rt <- aPeak
  
  if ( sample_rt < alkane_std_rt[1, "ALKRT"] || sample_rt > alkane_std_rt[nrow(alkane_std_rt), "ALKRT"] ){    
    return("NA")
  } 
  if( sample_rt == min(alkane_std_rt[, "ALKRT"]) )  {
    idx.lower <- max( which(alkane_std_rt[,"ALKRT"] <= sample_rt) )
    idx.upper <- min( which(alkane_std_rt[,"ALKRT"] > sample_rt) )
  } else if ( sample_rt == max(alkane_std_rt[, "ALKRT"]) )  {
    idx.lower <- max( which(alkane_std_rt[,"ALKRT"] < sample_rt) )
    idx.upper <- min( which(alkane_std_rt[,"ALKRT"] >= sample_rt) ) 
  } else {
    idx.lower <- max( which(alkane_std_rt[,"ALKRT"] < sample_rt) )
    idx.upper <- min( which(alkane_std_rt[,"ALKRT"] > sample_rt) )
  }
  
  n <- alkane_std_rt[idx.lower, "Cn"]; RTn <- alkane_std_rt[idx.lower, "ALKRT"]; 
  N <- alkane_std_rt[idx.upper, "Cn"]; RTN <- alkane_std_rt[idx.upper, "ALKRT"];
  
  # from http://en.wikipedia.org/wiki/Kovats_retention_index
  ## I = 100 x [ n + (N-n) {tr_unknown - tr_n} / {tr_N - tr_n}]
  if ( TRUE ) { 
    ri <- 100 * (n + (N - n) * (log(sample_rt) - log(RTn)) / (log(RTN) - log(RTn)) )
  } else {
    ri <- 100 * (n + (N - n) * (sample_rt - RTn) / (RTN - RTn) )
  }
  
  return(ri)    
}


extractSampleInfo2 <- function(aSampleFile) {
  # if using xcmsSet
  # xcmsSet objects do not hold any mz or intensity data, either in raw or binned (profile) form; 
  #  Creating (and destroying) xcmsRaw objects to access the mz and intensity data from the source .mzXML files
  #  Only scantime vectors (in the @rt slot) are held in xcmsSet objects, as these are used for retention time correction
  
  ## xcmsRaw will be used for Match Factor/Correlation/... using its m/z and intensity   
  xraw <- xcmsRaw(aSampleFile, profstep=1, scanrange=NULL) 
  ## peaks <- findPeaks.centWave(xraw, ppm=30, peakwidth=c(5,10), snthresh=10, prefilter=c(3,100), fitgauss=TRUE)
  peaks <- NULL
  # xset.samples <- xcmsSet(file_list[-1],method='centWave',ppm=1,snthresh=1,peakwidth=c(1,10),scanrange=c(1,3800))
  # peak.ranges <- findPeaks.centWave(xset.samples, ppm=25, peakwidth=c(1,10), snthresh=10, prefilter=c(3,100), fitgauss=FALSE)
  xset <- list( xraw=xraw, peaks=peaks )
  
  return(xset)  
}


# xset <- xset.asample
extract_peak_list_blank <- function(xset, ctype="EIC", offset=1.5, plotFile=TRUE)  {
  mzrange <- t(xset@mzrange)
  rtrange <- t(range(xset@scantime))
  ## Generate multiple extracted ion chromatograms for m/z values of interest. 
  ## For xcmsSet objects, reread original raw data and apply precomputed 
  ## retention time correction, if applicable.
  eic_obj <- getEIC(xset, mzrange=mzrange, rtrange=rtrange)
  # eic_obj <- getEIC(xset, mzrange=mzrange, rtrange=rtrange, sampleidx=i, rt="corrected")
  
  # plot EIC generation
  if (plotFile) {
    sampleFile <- sub(".mzXML|.CDF", "", basename(xset@filepath[1]), ignore.case=TRUE)
    png(filename = paste("Plot_EIC_", sampleFile,".png", sep=''), width = 1000, height = 800, units = "px", pointsize = 10)
        plotEIC(xset, mzrange=mzrange, rtrange=rtrange); # same as plotTIC when it uses all m/zs
    dev.off()
  }
  
  ## get the RT and Intensity from EIC object
  rt_int_matrix <- eic_obj@eic[1][[1]][[1]]         
  if (is.null(rt_int_matrix[, "intensity"])) stop(paste("rt_int_matrix is null in ", xset@filepath[1])) 
  ## sample case
  
  ## select significant/compound peaks & screening higher peak only to mapping compounds
  ## Peak detection depends on the 'extract_rt_w_highest_peaks_on_outliers' !!!   
  if(ctype=="TIC") {
    rt_peak <- extract_RT_forEachPeak(rt_int_matrix, offset, ctype, xset)
  } else {
    rt_peak <- extract_RT_forEachPeak(rt_int_matrix, offset, ctype)  
  }
  # rt_peak/60
  
  if( FALSE )  {
    sampleName <- basename(xset@filepath[1]);
    plot(round(rt_peak[,1]/60,1), rt_peak[,2], type="h", xlim=c(08,60), ylim=c(0,max(rt_peak[,2])), main=sampleName)
    # round(rt_peak[,1]/60,3)
  }
  return(rt_peak)
}


# xset <- xset.asample
extract_peak_list_samples2 <- function(xset, ctype="EIC", offset=1.5, plotFile=TRUE)  {
  # mzrange <- t(c(65, mzrange.origin[2]))
  mzrange <- t(xset$xraw@mzrange)
  rtrange <- t(range(xset$xraw@scantime))
  ## Generate multiple extracted ion chromatograms for m/z values of interest. 
  ## For xcmsSet objects, reread original raw data and apply precomputed 
  ## retention time correction, if applicable.
  eic_obj <- getEIC(xset$xraw, mzrange=mzrange, rtrange=rtrange)
  # eic_obj <- getEIC(xset, mzrange=mzrange, rtrange=rtrange, sampleidx=i, rt="corrected")

  # plot EIC generation
  if (plotFile) {
        sampleFile <- sub(".mzXML|.CDF", "", basename(xset$xraw@filepath[1]), ignore.case=TRUE)
        png(filename = paste("Plot_EIC_", sampleFile,".png", sep=''), width = 1000, height = 800, units = "px", pointsize = 10)
          plotEIC(xset$xraw, mzrange=mzrange, rtrange=rtrange); # same as plotTIC when it uses all m/zs
        dev.off()
  }
  
  ## get the RT and Intensity from EIC object
  rt_int_matrix <- eic_obj@eic[1][[1]][[1]]         
  if (is.null(rt_int_matrix[, "intensity"])) stop(paste("rt_int_matrix is null in ", xset$xraw@filepath[1])) 
  ## sample case
  
  ## select significant/compound peaks & screening higher peak only to mapping compounds
  ## Peak detection depends on the 'extract_rt_w_highest_peaks_on_outliers' !!!   
  if(ctype=="TIC") {
    rt_peak <- extract_RT_forEachPeak(rt_int_matrix, offset, ctype, xset$xraw)
  } else {
    rt_peak <- extract_RT_forEachPeak(rt_int_matrix, offset, ctype)  
  }
  # rt_peak/60
  
  if( FALSE )  {
    sampleName <- basename(xset$xraw@filepath[1]);
    plot(round(rt_peak[,1]/60,1), rt_peak[,2], type="h", xlim=c(08,60), ylim=c(0,max(rt_peak[,2])), main=sampleName)
    # round(rt_peak[,1]/60,3)
  }
  return(rt_peak)
}


# peak_list_sample <- peaks.sample; 
# alkane_std <- peak_alkane_std;
# alkane_std: (ALKRT, Cn)
get_RI_for_samples2 <- function(peak_list_sample, alkane_std) {
  ## calculate RI of alkane standard and all the samples.
  ## augs: object matrix of rts and peaks: peak_list_samples
  ## return : object matrix  peak_list_samples with RI column
  
  #  for(i in 1:length(peak_list_sample)) {
  sample_rt_vec <- as.numeric(peak_list_sample[,"rt"])
  # ri_vec <- get_RIs(sample_rt_vec, alkane_std)
  ri_vec <- NULL;
  for ( j in 1:length(sample_rt_vec) ) {
    RI <- calc_RI(sample_rt_vec[j], alkane_std)
    ri_vec <- c(ri_vec, RI)
    # ri_vec[length(ri_vec)+1] <- get_RI(sample_rt_vec[i], peak_list_sample1)
    ## get_RI() is replace with calc_RI
  }
  ri_vec[which(ri_vec=="NA")] <- "-1"
  ri_vec <- as.numeric(ri_vec)
  # print(ri_vec)
  
  # peak_list_samples[[i]] <- cbind(peak_list_samples[[i]], RI=round(as.numeric(ri_vec),3) )    
  peak_list_sample <- cbind(peak_list_sample, rt_min=round(sample_rt_vec/60, 3), RI=round(as.numeric(ri_vec),3) )
  #  }
  
  return(peak_list_sample)
}


# combine RT range detection algorithms
# step 1) calc RTrange with original method
# step 2) calc half and 1/3 of intensity range
# step 3) comparing those range and decision final range
# using half of peak's intensity
# apex.idx <- peak.scanidx <- 2032 # cholesterol
getPeakRange2 <- function(xset.one, nScanIndex, apex.idx)
{
    DEBUG2 <- FALSE
    
    # set search range
    peak.rt <- xset.one@scantime[apex.idx]
  
    tmp.idx1 <- ifelse((apex.idx-30) <= 1, 2, (apex.idx-30))
    tmp.idx2 <- ifelse((apex.idx+30) >= (nScanIndex-1), nScanIndex-1, (apex.idx+30))    
  
    # step 1  
    {
        tics <- xset.one@tic[tmp.idx1 : tmp.idx2]
        # plot(tics)
        # s <- summary(tics)
        threshold <- 10000 # It depends on the baseline correction and noise ratio    
        
        # left side to find a RT.start
        idx <- apex.idx
        diff.intensity <- 9999999999;
        while ( (idx > 1) & ( (abs(apex.idx - idx) < 3) | (diff.intensity > threshold)) ) {
          # 1) slope < threshold or 2) change the sigh of slope to positive/negative
          diff.intensity <- (xset.one@tic[idx] - xset.one@tic[idx-1])
          peak.rtmin <- xset.one@scantime[idx]
          if(FALSE & DEBUG) cat("## idx:", idx," RT start:", peak.rtmin, " RT start (min):", peak.rtmin/60, " diff.intensity:", diff.intensity, "\n")
          # if( diff.intensity < 0 | diff.intensity < threshold ) cat("reached the shoulder")
          idx <- idx - 1
        } 
        
        # right side to find a RT.end
        idx <- apex.idx
        diff.intensity <- 9999999999;
        n.idx <- length(xset.one@scantime);
        while ( (idx < n.idx) & ( (abs(apex.idx - idx) < 3) | (diff.intensity > threshold))  ) {
          # 1) slope < threshold or 2) change the sigh of slope to positive/negative
          diff.intensity <- (xset.one@tic[idx] - xset.one@tic[idx+1])
          peak.rtmax <- xset.one@scantime[idx]
          if(DEBUG2 & DEBUG) cat("## RT end:", peak.rtmax, " RT end (min):", peak.rtmax/60, " diff.intensity:", diff.intensity, "\n")
          idx <- idx + 1
        }        

        if (DEBUG2 & DEBUG) cat("## Peak RT Range (RT:", peak.rt/60,") :\n\t Step1 - RTmin:", peak.rtmin/60," RTmax:", peak.rtmax/60, "\n\n")
    }
  
    # step 2    
    {
        # get half of peak intensity
        halfIntensity <- round(xset.one@tic[apex.idx] / 2, 0)
        oneThirdIntensity <- round(xset.one@tic[apex.idx] / 3, 0) 
        
        # left side to find a RT.start
        idx <- apex.idx
        FLAG_HALF <- FALSE
        peak.rtmin_half <- NULL
        while ( (idx > tmp.idx1) & (oneThirdIntensity < xset.one@tic[idx-1])  ) {
          # cat("half:", halfIntensity, "  ith:", xset.one@tic[idx-1],"\n" )
          if (!FLAG_HALF & (halfIntensity > xset.one@tic[idx-1])) {
              peak.rtmin_half <- xset.one@scantime[idx-1]
              FLAG_HALF <- TRUE
          }
          # peak.rtmin <- xset.one$xraw@scantime[idx]
          # cat("## idx:", idx, "RTmin half:", peak.rtmin_half/60, "RT start:", peak.rtmin, " RT start (min):", peak.rtmin/60, "\n")          

          idx <- idx - 1
        }
        # if there is very close between half and one thirds intensity, it cann't find a half RT time; Then just use same RTmin/max
        if (!FLAG_HALF) peak.rtmin_half <- xset.one@scantime[idx]
        peak.rtmin_onethird <- xset.one@scantime[idx]
        
        # right side to find a RT.end
        idx <- apex.idx
        FLAG_HALF <- FALSE
        peak.rtmax_half <- NULL
        while ( (idx < tmp.idx2) & (!is.na(xset.one@tic[idx+1]) & (oneThirdIntensity < xset.one@tic[idx+1]))  ) {
          # peak.rtmax <- xset.one$xraw@scantime[idx]
          # cat("idx:", idx, " RT:", peak.rt/60,"tic:", xset.one@tic[idx+1], "\n")
          if (!FLAG_HALF & (halfIntensity < xset.one@tic[idx+1])) {
            peak.rtmax_half <- xset.one@scantime[idx+1]
            FLAG_HALF <- TRUE
          }
          idx <- idx + 1
        }

        if (!FLAG_HALF) peak.rtmax_half <- xset.one@scantime[idx]
        peak.rtmax_onethird <- xset.one@scantime[idx]        
        
        if (DEBUG2 & DEBUG) { 
          cat("## Peak RT Range using half intensity:\n\t Step2 - RTmin:", peak.rtmin_half/60," RTmax:", peak.rtmax_half/60, "\n\n")
          cat("## onethird - RTmin:", peak.rtmin_onethird/60," RTmax:", peak.rtmax_onethird/60, "\n\n")
        }
    }
    
    # to covering something wrong in RT range detection

    if ( (peak.rtmin > peak.rtmin_half) | (peak.rtmax < peak.rtmax_half) ) {
        peakWidth.left <- (peak.rt - peak.rtmin_half)
        peakWidth.right <- (peak.rtmax_half - peak.rt)
        widthRatio <- max(peakWidth.left, peakWidth.right) / min(peakWidth.left, peakWidth.right)
        if(widthRatio > 2) {
           peakType = "SKEWED" # (using 1/3)
           if (DEBUG2 & DEBUG) cat("## Peak RT Range: peak type : ", peakType, "\n\t RTmin:", peak.rtmin/60," RT:",  peak.rt/60," RTmax:", peak.rtmax/60, "\n")
          
           peak.rtmin = peak.rtmin_onethird
           peak.rtmax = peak.rtmax_onethird
        } else {
           peakType = "SYMMETRIC" #  (using 1/2)
           if (DEBUG2 & DEBUG) cat("## Peak RT Range: peak type : ", peakType, "\n\t RTmin:", peak.rtmin/60," RT:", peak.rt/60, " RTmax:", peak.rtmax/60, "\n")
          
           peak.rtmin = peak.rtmin_half
           peak.rtmax = peak.rtmax_half
        }
        
        if (DEBUG2 & DEBUG) {
           cat("\t RTmin_1/2:", peak.rtmin_half/60," RTmax_1/2:", peak.rtmax_half/60, "\n")
           cat("\t RTmin_1/3:", peak.rtmin_onethird/60," RTmax_1/3:", peak.rtmax_onethird/60, "\n")
        }
        
    } else {
        peakType = "NORMAL"
    }
      
    return (list(RTmin=peak.rtmin, RT=peak.rt, RTmax=peak.rtmax, peakType = peakType))
}      


getMZIntensityofEachPeak2 <- function(xset.one, peak_rt_vec) {
  
  mzIntList <- list()
  nPeakRT <- length(peak_rt_vec)
  nScanIndex <- max(xset.one@scanindex)
  
  if (DEBUG) { cat("### nScanIndex:", nScanIndex, "\n\n") }
  
  # i <- 322
  for ( i in 1:nPeakRT) {    
    peak.scanidx <- which(xset.one@scantime == peak_rt_vec[i]) 
    # peak_rt_vec[i]/60
    if(FALSE & DEBUG) cat("i:", i, "peak.scanidx:",peak.scanidx, "\n")
    
    if ( length(peak.scanidx) == 0) {
      cat("There is no matched RT - rt:", peak_rt_vec[i], " --- skipped\n")
      # stop("There is no matched RT in xcmsRaw data") 
      next
    }
            
    ## NEW ========================================================================
    # find a range of a peak    
    peakRange <- getPeakRange2(xset.one, nScanIndex, peak.scanidx)
    
    ## area calculation (Intensity, Range)    
    peakArea <- getPeakArea3(xset.one, xset.one@tic[peak.scanidx], peakRange) 
    # peakArea <- getPeakArea2(xset.one$xraw, peak_rt_vec[i], peakRange$RTmin, peakRange$RTmax) 
    # cat("### peak Area:", peakArea, "peak.scanidx:", peak.scanidx, peakRange$RTmin, peakRange$RTmax, "\n")

    mzIntensity <- getScan(xset.one, peak.scanidx) # *** give all m/z & intensity spectrum         
    ## @@@ ## switch with TIC instead of sum of intensity
    # peakIntensity <- sum(mzIntensity[,"intensity"])
    peakIntensity <- xset.one@tic[peak.scanidx]

    mzIntList[[length(mzIntList)+1]] <- list(peak_index=i, rt=peak_rt_vec[i], mzInt=mzIntensity
                                             , peakIntensity=peakIntensity, peakArea=peakArea
                                             , peakRTStart=round(peakRange$RTmin/60,3), peakRTEnd=round(peakRange$RTmax/60,3) 
                                             , peakType = peakRange$peakType)
    # scan<-getScan(xr, 1) # *** give all m/z & intensity spectrum
    # head(scan1)
    # plotScan(xr, 4) 
  }

  return (mzIntList)
}


## peak identification
# alkane.peakInfo <- peaks.alkane
# xset.one <- xset.alkane
# lib.peak.alkane <- lib.peak.alkane
peakIdentify.alkane <- function(alkane.peakInfo, xset.one, lib.peak.alkane)
{
  # get m/z and intensity using xcmsRaw() for a sample spectra
  
  ## varifying peak compound check
  ## get peaks' RIs and RTs 
  ## get m/z and intensity  
  ## for RT, it should be used raw RT instead of corrected
  # cat("asample.peakInfo:\n"); print(asample.peakInfo)
  peak_rt_vec <- alkane.peakInfo[, "rt"] 
  ## get rt/mz/intensity/area/... for each peak
  peak_mzInt_list <- getMZIntensityofEachPeak2(xset.one, peak_rt_vec)
  
  ## note: the length of each vector should be same. if not, the function will be stop and give message
  maxItensity <- max(alkane.peakInfo[,'intensity'])  
  rtRange <- range(xset.one@scantime)  
  
  # get mz/int vec for all standard peaks in alkane library
  mzIntLib <- list()
  for(i in 1:nrow(lib.peak.alkane)) {  ## repeat for selected candidates
      alib <- lib.peak.alkane[i,]
        
      ref_MZS_vec <- as.numeric(unlist(strsplit(as.character(alib$MZ), split=" ")))
      ref_INT_vec <- as.numeric(unlist(strsplit(as.character(alib$Intensity), split=" ")))
      ref_MZS_top20_vec <- as.numeric(unlist(strsplit(as.character(alib$MZ20), split=" ")))
      ref_INT_top20_vec <- as.numeric(unlist(strsplit(as.character(alib$Intensity20), split=" ")))

      mzIntLib[[length(mzIntLib)+1]] <- list(index=i, Compound=alib$Compound, Cn=alib$Cn
                                               , MZvec=ref_MZS_vec, INTvec=ref_INT_vec   
                                               , MZ20vec=ref_MZS_top20_vec, INT20vec=ref_INT_top20_vec)     
  }
  # print(mzIntLib)  
  
  ## for each peak of alkane spectrum
  ## identifying compounds using each peak's m.z/intensity with alkane library
  peakTotal <- length(peak_rt_vec)
  identifiedList <- NULL # collecting identified information for all peaks in a sample spectrum
  for (j in 1:peakTotal) {
        # j <- 2
      identified <- NULL
  
      ## will only use m/z and intensity
      # use peak location/index for no alkane covering
      RT.sample <- as.numeric(alkane.peakInfo[j,"rt"])      
      sample_mzs_vec <- peak_mzInt_list[[j]]$mzInt[,"mz"] ## from xcmsRaw
      sample_mz_int_vec <- peak_mzInt_list[[j]]$mzInt[,"intensity"] ## from xcmsRaw
      peakIntensity <- peak_mzInt_list[[j]]$peakIntensity # just for adding addition information

      ## step 2) for selected candidate compounds, calculate scores like Match Factor and others?
      TScore <- 0;
      identified.trace <- NULL; ## for trace
      for(k in 1:nrow(lib.peak.alkane)) {  ## repeat for selected candidates
          aMZIntLib <- mzIntLib[[k]]
        
          # finding matched m/z for a peak
          round.digit <- 0 # 0 or 2; not 1
          lst <- find_similar_peaks(aMZIntLib$MZvec, aMZIntLib$INTvec, sample_mzs_vec, sample_mz_int_vec, round.digit)
          lst20 <- find_similar_peaks(aMZIntLib$MZ20vec, aMZIntLib$INT20vec, sample_mzs_vec, sample_mz_int_vec, round.digit)
          if (length(lst$MZS_vec_tmp)==0) { 
            cat("# ERROR - compound:", alib.matched$Compound, "\n");
            stop("No m/z matched")
          } else {
            if (DEBUG & FALSE) {         
              cat("## Match Factor) # of m/z matched:", length(lst$MZS_vec_tmp), " ref m/z:", length(ref_MZS_vec)
                  , " sample m/z:", length(sample_mzs_vec), "\n")
            }
          }
          
          ## Match Factor (1000)
          # MFscore <- get_mathc_factor(MZS_vec, INTS_vec, mzs_vec, mz_int_vec)
          ## split the values of the mzs and intensities both library and sample                                    
          MFscore <- calcMFscore(lst$MZS_vec_tmp, lst$INTS_vec_tmp, lst$mzs_vec_tmp, lst$mz_int_vec_tmp)
          MFscore20 <- calcMFscore(lst20$MZS_vec_tmp, lst20$INTS_vec_tmp, lst20$mzs_vec_tmp, lst20$mz_int_vec_tmp)
          
          # correlation (100%) with matched intensity 
          cor.pearson <- cor(lst$INTS_vec_tmp, lst$mz_int_vec_tmp, method="pearson")
          cor.spearman <- cor(lst$INTS_vec_tmp, lst$mz_int_vec_tmp, method="spearman")
          cor.spearman20 <- cor(lst20$INTS_vec_tmp, lst20$mz_int_vec_tmp, method="spearman")
          
          tmp.TScore = 0.5 * MFscore + 0.5 * (cor.spearman + cor.spearman20)/2 * 1000
          
          # if(round(RT.sample,0) == 2262) cat("RT_min:", round(RT.sample/60, 3), " Compound:", as.character(aMZIntLib$Compound),  "MFScore:", MFscore, "MFscore20:", MFscore20, "Corr:", round(cor.spearman20 * 100, 2), "Tscore:", tmp.TScore,"\n" )
          
          if ( ( !is.na(tmp.TScore) & (tmp.TScore > TScore)) ) {
              TScore <- tmp.TScore
                        
              identified <- c(
                Compound=as.character(aMZIntLib$Compound),
                Cn=aMZIntLib$Cn,
                RT_min=round(RT.sample/60, 3),
                RT=RT.sample,
                Intensity=peakIntensity,
                MatchFactor=MFscore,
                MatchFactor20=MFscore20,
                Corr.Spearman=round(cor.spearman * 100, 2),
                TScore=round(TScore, 2)
              )              
          }
      } ## end of for nrow(lip.peak.alkane)
      
      identifiedList <- rbind(identifiedList, identified)           
  }
  
  rownames(identifiedList) <- c(1:nrow(identifiedList))
  # print(identifiedList)
  
  ## return identified compounds with additional informations 
  ## such as (RT, RI, Intensity, Area (RT start/end), Similarity Measures)    
  return( as.data.frame(identifiedList) )
}


## peak identification wiht molecular weight
# alkane.peakInfo <- peaks.alkane
# xset.one <- xset.alkane
# lib.peak.alkane <- lib.peak.alkane
peakIdentify.alkane2 <- function(alkane.peakInfo, xset.one, lib.peak.alkane)
{
  # get m/z and intensity using xcmsRaw() for a sample spectra
  
  ## varifying peak compound check
  ## get peaks' RIs and RTs 
  ## get m/z and intensity  
  ## for RT, it should be used raw RT instead of corrected
  # cat("asample.peakInfo:\n"); print(asample.peakInfo)
  peak_rt_vec <- alkane.peakInfo[, "rt"] 
  ## get rt/mz/intensity/area/... for each peak
  peak_mzInt_list <- getMZIntensityofEachPeak2(xset.one, peak_rt_vec)
  # cat("peak_mzInt_list:\n"); print(peak_mzInt_list)
  
  ## note: the length of each vector should be same. if not, the function will be stop and give message
  maxItensity <- max(alkane.peakInfo[,'intensity'])  
  rtRange <- range(xset.one@scantime)  
  
  # get mz/int vec for all standard peaks in alkane library
  mzIntLib <- list()
  for(i in 1:nrow(lib.peak.alkane)) {  ## repeat for selected candidates
    alib <- lib.peak.alkane[i,]
    
    ref_MZS_vec <- as.numeric(unlist(strsplit(as.character(alib$MZ), split=" ")))
    ref_INT_vec <- as.numeric(unlist(strsplit(as.character(alib$Intensity), split=" ")))
    ref_MW <- alib$MW
    
    mzIntLib[[length(mzIntLib)+1]] <- list(index=i, Compound=alib$Compound, Cn=alib$Cn,
                                           MW=ref_MW, MZvec=ref_MZS_vec, INTvec=ref_INT_vec)     
  }
  # cat("mzIntLib:\n"); print(mzIntLib)  
  
  ## for each peak of alkane spectrum
  ## identifying compounds using each peak's m.z/intensity with alkane library
  peakTotal <- length(peak_rt_vec)
  identifiedList <- NULL # collecting identified information for all peaks in a sample spectrum
  for (j in 1:peakTotal) {
    # j <- 2
    identified <- NULL
    
    ## will only use m/z and intensity
    # use peak location/index for no alkane covering
    RT.sample <- as.numeric(alkane.peakInfo[j,"rt"])      
    sample_mzs_vec <- round(peak_mzInt_list[[j]]$mzInt[,"mz"], 0) ## from xcmsRaw
    sample_mz_int_vec <- peak_mzInt_list[[j]]$mzInt[,"intensity"] ## from xcmsRaw
    peakIntensity <- peak_mzInt_list[[j]]$peakIntensity # just for adding addition information
    
    peakMZlength = length(sample_mzs_vec)
    # cat("RT.sample:", RT.sample,"\n MZ:\n"); print(sample_mzs_vec); cat("\nIntensity:\n"); print(sample_mz_int_vec)
    # if (DEBUG) cat("RT.sample:", RT.sample,"\n length(MZ):",length(sample_mzs_vec),"\n"); 
    
    ## step 2) for selected candidate compounds, calculate scores like Match Factor and others?
    identified.trace <- NULL; ## for trace
    if(FALSE & DEBUG ) { cat("\n\n# new peak - RT:", RT.sample, "\n Sample MZ:", sample_mzs_vec,"\n") }
    # cat("\n\n# new peak - RT:", RT.sample, "\n")
    for(k in 1:nrow(lib.peak.alkane)) {  ## repeat for selected candidates
      aMZIntLib <- mzIntLib[[k]]
      
      # finding matched molecular weight of m/z for a peak
      
      if (aMZIntLib$MW %in% sample_mzs_vec) {
          # if (DEBUG) { cat(k, ") Found ", aMZIntLib$MW, "in\n", sample_mzs_vec, "\n") }
          flag_found <- TRUE
      } else {
          # cat(k, ") Cannot find ", aMZIntLib$MW, "in\n", sample_mzs_vec, "\n")
          flag_found <- FALSE
      }
      
      if ( flag_found ) {
          round.digit <- 0 # 0 or 2; not 1
          lst <- find_similar_peaks(aMZIntLib$MZvec, aMZIntLib$INTvec, sample_mzs_vec, sample_mz_int_vec, round.digit)
          
          if (length(lst$MZS_vec_tmp)==0) { 
            cat("# ERROR - compound:", aMZIntLib$Compound, "\n");
            stop("No m/z matched")
          } else {
            if (FALSE & DEBUG) {
              cat("## Match Factor) # of m/z matched:", length(lst$MZS_vec_tmp), " ref m/z:", length(ref_MZS_vec)
                  , " sample m/z:", length(sample_mzs_vec), "\n")
            }
          }
          
          # cat("lst$MZS_vec_tmp:\n"); print(lst$MZS_vec_tmp); print(lst$mzs_vec_tmp)
        
          ## Match Factor (1000)
          # MFscore <- get_mathc_factor(MZS_vec, INTS_vec, mzs_vec, mz_int_vec)
          ## split the values of the mzs and intensities both library and sample                                    
          MFscore <- calcMFscore(lst$MZS_vec_tmp, lst$INTS_vec_tmp, lst$mzs_vec_tmp, lst$mz_int_vec_tmp)
          # cor.pearson <- cor(lst$INTS_vec_tmp, lst$mz_int_vec_tmp, method="pearson")
          cor.spearman <- cor(lst$INTS_vec_tmp, lst$mz_int_vec_tmp, method="spearman")
          
          identified <- c(
            Compound=as.character(aMZIntLib$Compound),
            Cn=aMZIntLib$Cn,
            RT_min=round(RT.sample/60, 3),
            RT=RT.sample,
            MW=aMZIntLib$MW,
            MatchFactor=MFscore,        
            # Corr.Pearson=round(cor.pearson * 100, 2),
            Corr.Spearman=round(cor.spearman * 100, 2),          
            Intensity=peakIntensity,
            MZlength=peakMZlength
          )              
          identifiedList <- rbind(identifiedList, identified)
      }
    } ## end of for nrow(lip.peak.alkane)
  }
  rownames(identifiedList) <- c(1:nrow(identifiedList))
  # print(identifiedList)
  ## return identified compounds with additional informations 
  ## such as (RT, RI, Intensity, Area (RT start/end), Similarity Measures)    
  return( as.data.frame(identifiedList) )
}


## peak identification wiht molecular weight
# alkane.peakInfo <- peaks.alkane
# xset.one <- xset.alkane
# lib.peak.alkane <- lib.peak.alkane
peakIdentify.alkane.old <- function(alkane.peakInfo, xset.one, lib.peak.alkane)
{
  # get m/z and intensity using xcmsRaw() for a sample spectra
  
  ## varifying peak compound check
  ## get peaks' RIs and RTs 
  ## get m/z and intensity  
  ## for RT, it should be used raw RT instead of corrected
  # cat("asample.peakInfo:\n"); print(asample.peakInfo)
  peak_rt_vec <- alkane.peakInfo[, "rt"] 
  ## get rt/mz/intensity/area/... for each peak
  peak_mzInt_list <- getMZIntensityofEachPeak2(xset.one, peak_rt_vec)
  # print(peak_mzInt_list)
  
  ## note: the length of each vector should be same. if not, the function will be stop and give message
  maxItensity <- max(alkane.peakInfo[,'intensity'])  
  rtRange <- range(xset.one@scantime)  
  
  # get mz/int vec for all standard peaks in alkane library
  mzIntLib <- list()
  for(i in 1:nrow(lib.peak.alkane)) {  ## repeat for selected candidates
    alib <- lib.peak.alkane[i,]
    
    ref_MZS_vec <- as.numeric(unlist(strsplit(as.character(alib$MZ), split=" ")))
    ref_INT_vec <- as.numeric(unlist(strsplit(as.character(alib$Intensity), split=" ")))
    ref_MW <- alib$MW
    
    mzIntLib[[length(mzIntLib)+1]] <- list(index=i, Compound=alib$Compound, Cn=alib$Cn,
                                           MW=ref_MW, MZvec=ref_MZS_vec, INTvec=ref_INT_vec)     
  }
  # print(mzIntLib)  
  
  ## for each peak of alkane spectrum
  ## identifying compounds using each peak's m.z/intensity with alkane library
  peakTotal <- length(peak_rt_vec)
  identifiedList <- NULL # collecting identified information for all peaks in a sample spectrum
  for (j in 1:peakTotal) {
    # j <- 2
    identified <- NULL
    
    ## will only use m/z and intensity
    # use peak location/index for no alkane covering
    RT.sample <- as.numeric(alkane.peakInfo[j,"rt"])      
    sample_mzs_vec <- round(peak_mzInt_list[[j]]$mzInt[,"mz"], 0) ## from xcmsRaw
    sample_mz_int_vec <- peak_mzInt_list[[j]]$mzInt[,"intensity"] ## from xcmsRaw
    peakIntensity <- peak_mzInt_list[[j]]$peakIntensity # just for adding addition information
    
    ## step 2) for selected candidate compounds, calculate scores like Match Factor and others?
    MW_prev <- 0
    identified.trace <- NULL; ## for trace
    # !if( DEBUG ) { cat("\n\n# new peak - RT:", RT.sample, "\n Sample MZ:", sample_mzs_vec,"\n") }
    for(k in 1:nrow(lib.peak.alkane)) {  ## repeat for selected candidates
      aMZIntLib <- mzIntLib[[k]]
      
      # finding matched molecular weight of m/z for a peak
      
      if (aMZIntLib$MW %in% sample_mzs_vec) {
          if (DEBUG) { cat(k, ") Found ", aMZIntLib$MW, "in\n", sample_mzs_vec, "\n") }
          flag_found <- TRUE
      } else {
          # cat(k, ") Cannot find ", aMZIntLib$MW, "in\n", sample_mzs_vec, "\n")
          flag_found <- FALSE
      }
        
      if ( flag_found & (aMZIntLib$MW > MW_prev) ) {
        MW_prev <- aMZIntLib$MW
        
        identified <- c(
          Compound=as.character(aMZIntLib$Compound),
          Cn=aMZIntLib$Cn,
          RT_min=round(RT.sample/60, 3),
          RT=RT.sample,
          MW=aMZIntLib$MW,
          Intensity=peakIntensity
        )              
      }
    } ## end of for nrow(lip.peak.alkane)
    
    identifiedList <- rbind(identifiedList, identified)           
  }
  
  rownames(identifiedList) <- c(1:nrow(identifiedList))
  # print(identifiedList)
  
  ## return identified compounds with additional informations 
  ## such as (RT, RI, Intensity, Area (RT start/end), Similarity Measures)    
  return( as.data.frame(identifiedList) )
}


## without extended Alkane Standard
# asample.peakInfo <- peak_samples_ri
# xset.one <- xset.asample
compoundIdentify3 <- function(asample.peakInfo, xset.one, lib.peak, alkaneInfo, RI.Variation=0.03, isBLANK=FALSE)
{
  if(isBLANK) {
      xset.one <- xset.one
  } else {    
      xset.one <- xset.one$xraw
  }
    
  # get m/z and intensity using xcmsRaw() for a sample spectra
  
  ## varifying peak compound check
  ## get peaks' RIs and RTs 
  ## get m/z and intensity  
  ## for RT, it should be used raw RT instead of corrected
  # cat("asample.peakInfo:\n"); print(asample.peakInfo)
  peak_rt_vec <- asample.peakInfo[, "rt"] 
  ## get rt/mz/intensity/area/... for each peak
  peak_mzInt_list <- getMZIntensityofEachPeak2(xset.one, peak_rt_vec)
  
  # cat("peak_rt_vec\n"); print(peak_rt_vec)
  # cat("peak_mzInt_list"); print(peak_mzInt_list)
  
  ## @@@@@@
  ## used for mz/int DB update

  ofile <- paste(basename(xset.one@filepath[1]),"-mzint4DB.csv", sep='')
  cat("PeakNO,rt,rt_min,RI,mz,intensity\n", file=ofile, append=FALSE)
  for (i in 1:length(peak_mzInt_list))  {
    peakrt <- as.character(peak_mzInt_list[[i]]$rt)
    peakri <- as.character(asample.peakInfo[i,"RI"])
    # asample.peakInfo[65,]
    # peak_mzInt_list[[65]]$mzInt[,"mz"]
    
    peakmz <- paste(noquote(round(peak_mzInt_list[[i]]$mzInt[,"mz"],2)), collapse=" ")
    peakmzint <- paste(peak_mzInt_list[[i]]$mzInt[,"intensity"], collapse=" ")
    
    oinfo <- paste(c(i, peakrt, round(as.numeric(peakrt)/60,2), peakri, peakmz, peakmzint), collapse=",")
    cat(oinfo, "\n", file=ofile, append=TRUE)
  }
  
  cat("## peaks:", length(peak_mzInt_list), "\n\n")
}


## area calculation
# xr <- xset.one$xraw;
# peak.rt <- peak_rt_vec[i]
getPeakArea3 <- function(xraw, peakIntensity, peakRange)  { 
    if (peakRange$peakType %in% c("SYMMETRIC", "SKEWED") ) {
        area <- peakIntensity * (peakRange$RTmax - peakRange$RTmin)
    } else if (peakRange$peakType == "NORMAL") {
        area <- getPeakArea2(xraw, peakRange$RT, peakRange$RTmin, peakRange$RTmax)
    } else { # Strange
        area <- 0.0  
    }
    
    return ( round( area * 9.5, 0) )
}

getPeakArea2 <- function(xr, peak.rt, peak.rtmin, peak.rtmax)  {
  # peak.rt <- 1202.074; peak.rtmin <- 1194.735;  peak.rtmax <- 1209.413 
  scantime.indices <- which(xr@scantime >= peak.rtmin & xr@scantime <= peak.rtmax) 
  # plot(xr@scantime[scantime.indices]/60, xr@tic[scantime.indices], type="h")
  # cat("scantime.indices:\n");   print(scantime.indices)
  # max(xr@tic[scantime.indices])
  
  if( length(scantime.indices) == 0)  {
    cat("## No matced scan time in getPeakArea() - Peak's RT:", peak.rt, "(", peak.rt/60,")\n")
    return ( list (area=NA, rt.start=NA, rt.end=NA))
    # stop("No matced scan time index in getPeakArea()")
  }
  
  area <- 0
  for ( s in scantime.indices[- length(scantime.indices)]) {
    # mzIntensity <- getScan(xr, s) # *** give all m/z & intensity spectrum
    # peakIntensity <- sum(mzIntensity[,"intensity"])    
    # mzIntensity_next <- getScan(xr, s+1) 
    # peakIntensity_next <- sum(mzIntensity_pre[,"intensity"])
    tic <- xr@tic[s]  ## same as TIC == sum (mzIntensity)    
    tic_next <- xr@tic[s+1]  ## same as TIC == sum (mzIntensity)    
    
    width <- abs(xr@scantime[s] - xr@scantime[s+1])
    if( tic < tic_next ) {
      area_rec <- width * tic
    } else {
      area_rec <- width * tic_next
    }   
    area_triangle <- width * abs(tic - tic_next) / 2 
    area.temp <- area_rec + area_triangle
    area <- area + area.temp    
  } 
  
  # cat("rt:", peak.rt, "\t area:", area, "\n")
  
  if (DEBUG & area==0) {
    cat("## getPeakArea() - area is zero! rt:",peak.rt, "scantime.index:", scantime.indices, "\n")
  }
  
  return ( area )
}


# MZS_vec <- ref_MZS_vec
# INTS_vec <- ref_INT_vec
# mzs_vec <- sample_mzs_vec
# mz_int_vec <- sample_mz_int_vec

find_similar_peaks <- function(MZS_vec, INTS_vec, mzs_vec, mz_int_vec, round.digit=2){
  MZS_vec_tmp <- c()
  INTS_vec_tmp <- c()
  mzs_vec_tmp <- c()
  mz_int_vec_tmp <- c()
  
  for(i in 1:length(mzs_vec)) {
    sample_mz <- round(mzs_vec[i], round.digit) # this will be used for M in Match Factor (MF) calcuration    
    for(j in 1:length(MZS_vec)) {
      ## if( MZS_vec[j] - mz_off_set  <= mzs_vec[i] &&  mzs_vec[i] <= MZS_vec[j] + mz_off_set){
      if( round(MZS_vec[j], round.digit) == sample_mz ){
        MZS_vec_tmp[length(MZS_vec_tmp)+1] <- sample_mz
        INTS_vec_tmp[length(INTS_vec_tmp)+1] <- INTS_vec[j]
        mzs_vec_tmp[length(mzs_vec_tmp)+1] <- sample_mz
        mz_int_vec_tmp[length(mz_int_vec_tmp)+1] <-  mz_int_vec[i]
        j <- length(MZS_vec) 
      }
    }
  }
  
  # this is wrong I think; 70:30 linear combination means
  # that " MF score = 0.7 * Pure MF + 0.3 * Impure MF "
  if (FALSE) {
    # now keep the length(MZS_vec) : output'length  70:30 ratio
    if(length(MZS_vec_tmp) / length(MZS_vec) > 3.0/7.0){ # MZS_vec_tmp has more
      matrx <- as.matrix(data.frame(MZS_vec_tmp=MZS_vec_tmp,INTS_vec_tmp=INTS_vec_tmp, 
                                    mzs_vec_tmp=mzs_vec_tmp, mz_int_vec_tmp=mz_int_vec_tmp ))
      for(i in seq(0.001, 1, 0.001)){
        matrx <- matrx[which(matrx[,"mz_int_vec_tmp"] > i),]
        if (nrow(matrx)/length(MZS_vec) <= 3.0/7.0) break
      }
      MZS_vec_tmp <- matrx[,"MZS_vec_tmp"]; INTS_vec_tmp <- matrx[,"INTS_vec_tmp"]
      mzs_vec_tmp <- matrx[,"mzs_vec_tmp"]; mz_int_vec_tmp <- matrx[,"mz_int_vec_tmp"]
    }
  }
  
  return(list(MZS_vec_tmp=MZS_vec_tmp, INTS_vec_tmp=INTS_vec_tmp, mzs_vec_tmp=mzs_vec_tmp, 
              mz_int_vec_tmp=mz_int_vec_tmp))
}

## get_mathc_factor --> calcMFscore()
# purity m/z peak: only use for mapping m/z with intensity
calcMFscore <- function(ref_MZS_vec, ref_INT_vec, qry_mzs_vec, qry_int_vec) {
  ## calculate match factor with equation 7 of http://chemdata.nist.gov/mass-spc/amdis/docs/method.pdf
  ## args: mzs from DB:MZS_vec, intensity from db: INTS_vec, 
  ##       queried mzs from sample: mzs_vec, queried intensity from sample: mz_int_vec
  ## return: match factor score
  
  ref_INT_vec <- round(ref_INT_vec / max(ref_INT_vec), 3)
  qry_int_vec <- round(qry_int_vec / max(qry_int_vec), 3)
  
  a <- 0; b <- 0; c <- 0;  
  MF.weight <- 0.9 # for Match Factor (MF)
  # over only library m/z values for the impure match factor
  for(i in 1:length(ref_MZS_vec)){
    a <- a + qry_mzs_vec[i] * qry_int_vec[i]
    b <- b + ref_MZS_vec[i] * ref_INT_vec[i]
    c <- c + MF.weight * ref_MZS_vec[i] * (ref_INT_vec[i] * qry_int_vec[i]) ** 0.5    
  }
  
  MF_score <- round((1000 * c ** 2)/ (a * b), 3)
  
  return(MF_score)
}


arrangeProfiledPeaks2.alkane <- function(profiled_peaks, stype=NULL)
{
  profiled_final <- NULL
  # unique_complist <- unique(profiled_peaks$Compound)
  unique_RTlist <- unique(profiled_peaks$RT)  
  # cat("unique_RTlist:\n"); print(unique_RTlist)
  
  profiled_peaks$Cn <- as.numeric(as.character(profiled_peaks$Cn)) 
  profiled_peaks$RT <- as.numeric(as.character(profiled_peaks$RT)) 
  profiled_peaks$MatchFactor <- as.numeric(as.character(profiled_peaks$MatchFactor)) 
  profiled_peaks$Corr.Spearman <- as.numeric(as.character(profiled_peaks$Corr.Spearman)) 
  
  for (i in 1:length(unique_RTlist))  {
    tmpdata <- profiled_peaks[which(profiled_peaks$RT == unique_RTlist[i]), ]
    tmpdata <- tmpdata[order(tmpdata$Cn, decreasing=TRUE), ]
    # cat("\n\ntmpdata RT:", as.numeric(as.character(unique_RTlist[i])),"\n"); print(tmpdata)    
    
    j <- 1
    while ( (j < nrow(tmpdata)) 
             & (tmpdata$MatchFactor[j] <= tmpdata$MatchFactor[j+1]) 
             & (tmpdata$Corr.Spearman[j] <= tmpdata$Corr.Spearman[j+1]) ) {
       if(DEBUG) { cat("Excluding:\n"); print(tmpdata[j, ]) }
       tmpdata <- tmpdata[-j, ]
    }
    final.comp <- tmpdata[1, ]
    profiled_final <- rbind(profiled_final, final.comp)
  }  
  rownames(profiled_final) <- c(1:nrow(profiled_final))
  # profiled_final <- profiled_final[order(as.double(as.character(profiled_final$RT))), ]
  return(profiled_final)
}


arrangeProfiledPeaks2.alkane.old <- function(profiled_peaks, stype=NULL)
{
  profiled_final <- NULL
  unique_complist <- unique(profiled_peaks$Compound)
  for (j in 1:length(unique_complist))  {
    tmpdata <- profiled_peaks[which(profiled_peaks$Compound == unique_complist[j]), ]    
    tmpdata <- tmpdata[order(as.numeric(as.character(tmpdata$Intensity)), decreasing=TRUE), ] 
    final.comp <- tmpdata[1, ]
    profiled_final <- rbind(profiled_final, final.comp)
  }  
  rownames(profiled_final) <- c(1:nrow(profiled_final))
  # profiled_final <- profiled_final[order(as.double(as.character(profiled_final$RT))), ]
  
  return(profiled_final)
}

arrangeProfiledPeaks2 <- function(profiled_peaks, stype=NULL)
{
  
#  if(FALSE & (stype == SERUM)) {
#    wrongRibitol <- which( profiled_peaks$Compound == "Ribitol" 
#                          & as.double(as.character(profiled_peaks$Intensity)) < 100000)
    
#    cat("## wrong Ribitol:\n"); print(wrongRibitol);
#    if(length(wrongRibitol) > 0) {
#      cat("## wrong Ribitol - excluded\n")
#      profiled_peaks <- profiled_peaks[- which(profiled_peaks$Compound == "Ribitol" 
#                                               & as.double(as.character(profiled_peaks$Intensity)) < 100000), ]       
#    }
#  }
  
  profiled_final <- NULL
  unique_complist <- unique(profiled_peaks$Compound)
  for (j in 1:length(unique_complist))  {
    # j <- 12
    tmpdata <- profiled_peaks[which(profiled_peaks$Compound == unique_complist[j]), ]    
    tmpdata <- tmpdata[order(tmpdata$TScore, decreasing=TRUE), ] 
    final.comp <- tmpdata[1, ]
    profiled_final <- rbind(profiled_final, final.comp)
  }  
  profiled_final <- profiled_final[order(as.double(as.character(profiled_final$RI)), decreasing=FALSE), ] 
  # profiled_final <- profiled_final[order(as.double(as.character(profiled_final$RT))), ]
  rownames(profiled_final) <- c(1:nrow(profiled_final))
  
  return(profiled_final)
}

##################################################################################
## Quantification

## get AREA for each peak

# find matching library & call calc concentration ftn
calibration <- function(dbLib, compound, relative_area_comp) {
    # cat("## compound:", compound, "\t area_ratio:", relative_area_comp,"\n")  
    libVec <- dbLib[which(as.character(dbLib$Compound) == compound), ];
    # cat("### libVec:\n"); print(libVec);
    
    if(nrow(libVec) == 1) {
        conc <- calcConcentration(relative_area_comp, slope=libVec$Slope, intercept=libVec$Intercept);  
    } else {
        # cat("## Error to find a record in library\n\n");
        conc <- "NA"
    }
    return(conc)
}

# area: after divided by internal standard area
calcConcentration <- function(area, slope, intercept)
{  
  if ( (area - intercept) > 0 ) {
      round( (area - intercept) / slope , 3);
  } else {
      round( 0.0001 / slope , 3);
  }
}

# profiled.result <- final_PeakProfile
# lib <- lib.peak
# stype <- ORGANIC_ACID  
quantification <- function(profiled.result, lib, internalStdCmpd, sample_file, stype, unitConv=1) {
    # profiled.result <- as.matrix(profiled.result)
    ## cat("\n\t sample type is ", stype, "\n")
  
    # if(stype == SERUM) {
    #    cat("\t\t # SERUM is using Ribitol as internal standard\n")
    #    area.internalStd <-  profiled.result[which(profiled.result$Compound == "Ribitol"), "Area"]
    #} else if (stype == ORGANIC_ACID) {
    #    ## Need to check whether the Internal Std is exist
    #    cat("\t\t # Organic Acid (Urine) is using Cholesterol as internal standard\n")
    #    area.internalStd <-  profiled.result[which(profiled.result$Compound == "Cholesterol"), "Area"]
    #} else if (stype == SALIVA) {
    #    cat("\t\t # Saliva is using Ribitol as internal standard\n")
    #    area.internalStd <-  profiled.result[which(profiled.result$Compound == "Ribitol"), "Area"]
    #} else if (stype == MILK) {
    #    cat("\t\t # MILK is using Ribitol as internal standard\n")
    #    area.internalStd <-  profiled.result[which(profiled.result$Compound == "Ribitol"), "Area"]
    #}
  
    if( DEBUG) cat("\t   # selected internal standard compound:", internalStdCmpd,"\n")
    area.internalStd <-  as.numeric(as.character(profiled.result[which(as.character(profiled.result$Compound) == internalStdCmpd), "Area"]))
    cat("area.internalStd:", area.internalStd,"\n")
    #cat("internalStdCmpd:\n"); print(profiled.result[which(as.character(profiled.result$Compound) == internalStdCmpd), ])
    
    if (length(area.internalStd) == 0 ) {
      stop("
           ## Error: Cannot find internal standard compound '", internalStdCmpd, "' in the library.
           Please check the internal standard compound name both sample peak and library ") 
    }
    
    # area.internalStd    
    # area.internalStd <- as.numeric(as.character(area.internalStd))  
    if(length(area.internalStd) == 0) {
        area.internalStd <- 100000000 
        # cat("\n## ERROR: no internal standard (Ribitol or Cholesterol) area\n\n")
        stop("\n##\n## ERROR: no internal standard area was found\n >> File:", sample_file,"##\n")
    } 
    # cat(paste(colnames(profiled.result), sep="\t"), "Concentration\n", file=ofile, append=FALSE)
    
    quantifiedList <- NULL
    for ( i in 1:nrow(profiled.result) ) {
        compound <- as.character(profiled.result$Compound[i])
        area_ratio <- as.double(as.character(profiled.result$Area[i])) / as.double(as.character(area.internalStd))
        if (DEBUG) { 
             cat("### compound:", compound, "\t area:", as.double(as.character(profiled.result$Area[i])), "\t area.internalStd:", as.double(as.character(area.internalStd)), "\t area_ratio:", area_ratio,"\n")
        }
                
        # if ( as.double(as.character(profiled.result$Area[i])) > 0) {        
        # if ( (compound != internalStdCmpd) & (area_ratio > 0) ) {  
        if ( (compound != internalStdCmpd) ) {  
            if(area_ratio > 0) {
                calc <- calibration(lib, compound, area_ratio) / unitConv
                calc <- ifelse(calc < 0, 0, calc)  
            } else {
                calc <- 0
            }
            # cat("### call calibration - compound:", compound, "\t area_ratio:", area_ratio, "\t conc:", calc,"\n")
            
            # Convert Concentration Scale
            # if(stype == SERUM) {
            #   calc <- calc / 1000 # uM -> mM    
            #}
        } else if ( compound == internalStdCmpd ) {
            calc <- 'ISTD' # internal standard
        }
        #else {
        #    # cat("### NOT call calibration - compound:", compound, "\t area_ratio:", area_ratio, "\n")
        #    calc <- 'NA'
        #}
        
        quantifiedList <- rbind(quantifiedList, cbind(profiled.result[i, ], Concentration=calc) ) 
    }
    
    return (quantifiedList)
}


## report final for list all with non-detected
getFinalReport <- function(cmpdlist, quantifiedResult, MFthreshold) {
  # colnames(cmpdlist) <- c("Compound","SeqIndex")
  
  quantifiedResult <- quantifiedResult[which(as.numeric(as.character(quantifiedResult$MatchFactor)) > MFthreshold), ]  
  ## print/save all metabolites in the library
  # report.profile <- merge(cmpdlist, as.data.frame(quantifiedResult), by = c('Compound'), all.x=TRUE, sort=FALSE)
  ## print/save matched metabolites
  report.profile <- merge(cmpdlist, as.data.frame(quantifiedResult), by = c('Compound'), sort=FALSE) 
  report.profile <- report.profile[order(as.numeric(as.character(report.profile$RT)), decreasing=FALSE), ]
  rownames(report.profile) <- c(1:nrow(report.profile))
  # report.profile <- report.profile[, c(1,3,4,5,2,6:14)]
  # colnames(report.profile)[4] <- "RI"
  # colnames(report.profile)[5] <- "RI.lib"
  
  return(report.profile)
}

spectrumToJSON.old <- function(d)
{    
  NROW <- nrow(d)        
  ostr <- "{\n \"spectrum_xy\": [\n"
  for(i in 1:(NROW-1)) {
    ostr <- paste(ostr, "\t{ \"x\":", d$x[i], ",\n\t  \"y\":", d$y[i], " },\n", sep="")
  }
  ostr <- paste(ostr, "\t{ \"x\":", d$x[NROW], ",\n\t  \"y\":", d$y[NROW], " } \t ] }", sep="")
  # cat(ostr)
  return(ostr)
}


## generate JSON format string for Spectrum View
spectrumToJSON.fullSpectrum <- function(d)
{    
  name.value <- function(i){
    quote <- '';
    # if(class(dtf[, i])!='numeric'){
    if(class(d[, i])!='numeric' && class(d[, i])!= 'integer'){ # I modified this line so integers are also not enclosed in quotes
      quote <- '"';
    }
    
    paste('"', i, '" : ', quote, d[,i], quote, sep='')
  }
  
  objs <- apply(sapply(c("x","y"), name.value), 1, function(x) {paste(x, collapse=', ')})
  objs <- paste('\t\t {', objs, '}')
  
  res <- paste(objs, collapse=',\n')
  res <- paste("\t\"spectrum_xy\": [\n", res, " ]")
  
  return(res)
}

## generate JSON format string for Spectrum View
spectrumToJSON.profile <- function(d)
{    
  name.value <- function(i) {
    quote <- '';
    if(class(d[, i])!='numeric' && class(d[, i])!= 'integer'){ # I modified this line so integers are also not enclosed in quotes
        quote <- '"';
    }
    
    paste('"', i, '" : ', quote, d[,i], quote, sep='')
  }
  
  objs <- apply(sapply(c("x","y","peak"), name.value), 1, function(x) {paste(x, collapse=', ')})
  objs <- paste('\t\t {', objs, '}')
  
  res <- paste(objs, collapse=',\n')
  res <- paste("\t\"spectrum_xypeak\": [\n", res, " ]")
  
  return(res)
}

create_json_file <- function(ofilename, fullspec.x, fullspec.y, profile.x, profile.y, profile.compound)
{
    x <- fullspec.x
    y <- fullspec.y
    spectrum.xy <- data.frame(x,y)
    fullspec.json <- spectrumToJSON.fullSpectrum(spectrum.xy)            
    
    x <- profile.x
    y <- profile.y
    peak <- profile.compound
    spectrum.xypeak <- data.frame(x,y,peak)
    profiledspec.json <- spectrumToJSON.profile(spectrum.xypeak)
    
    finalspec.json <- paste("{\n", fullspec.json, ", \n", profiledspec.json, "}")
    cat(finalspec.json, file=ofilename, append=FALSE)    
}

create_json_file.alkane <- function(ofilename, fullspec.x, fullspec.y)
{
  x <- fullspec.x
  y <- fullspec.y
  spectrum.xy <- data.frame(x,y)
  fullspec.json <- spectrumToJSON.fullSpectrum(spectrum.xy)
  fullspec.json <- paste("{\n", fullspec.json, "}")
  
  cat(fullspec.json, file=ofilename, append=FALSE)    
}

# check whether the internal standard is exist or not
existInternalStd <- function(internalStd, profiledPeakSet, libDB)
{
   # cat("internalStd:", internalStd, "\n");
   # cat("profiledPeakSet:\n"); print(head(profiledPeakSet));
   # cat("libDB:\n"); print(head(libDB));
   
   if( ! (internalStd %in% profiledPeakSet$Compound) ) {
      cat("## Warning: Can not find the interstandard (",internalStd,") in Sample Spectrum.\n\t Program will report without quantification.\n\n")
      return (FALSE)
   }
   if( ! (internalStd %in% libDB$Compound) ) {
     cat("## Warning: Can not find the interstandard (",internalStd,") in the library database.\n\t Program will report without quantification.\n\n")     
     return (FALSE)
   }      
   
   return (TRUE)   
}

check.Concentration<-function(conc) 
{
    for (i in 1:length(conc)) {
        if(conc[i] == 0) {
            # cat(i, "conc[i]:", conc[i], "--> <LOD\n") 
            conc[i] <- '<LOD' 
        } 
        #else if (conc[i] > 0.90) {
        #    # cat(i, "conc[i]:", conc[i], "--> NA\n")   
        #    conc[i] <- 'NA'
        #}
    }
    
    return (conc)
}

###################################################
## The End of the File
###################################################

