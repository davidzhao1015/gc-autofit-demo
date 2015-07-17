############################################################################################
## Library for automatic GC-MS profiling
## update: August 8, 2014
############################################################################################

## Loading Packages
## Install packages:
##    source("http://bioconductor.org/biocLite.R")
##    biocLite(module)
##    biocLite("BiocUpgrade")
##    biocLite("xcms")

# library(xcms)
suppressMessages(require(xcms));

# Generate Spectrum Plots
# fname.list <- fileList$sampleFiles
# isPlotOnly=TRUE
generateSpectrumPlot <- function(fname.list, isPlotOnly=TRUE) {      
  if(DEBUG) cat("# Generate Spectrum Plots\n")
  
  for (i in 1:length(fname.list)) {   ## running per each sample         
    # i<-1
    f.sample <- fname.list[i]
    f.sample.basename <- basename(f.sample)
    cat(paste("\n", i, ") Profiling Processing file :", sep=''), f.sample.basename, "\n")
    
    cat("\t >> Extracting Spectrum Information \n")
    xset.asample <- extractSampleInfo2(f.sample)
    
    ## peak picking for samples using TIC or EIC (peak's RT & Intensity)
    cat("\t >> Extracting peak list \n")
    peaks.sample <- extract_peak_list_samples2(xset.asample, ctype="TIC", offset=1.5, isPlotOnly)
  }
  
  # To check the process is done or not
  cat("Done -", length(fname.list),"Samples's Image Files were Generation\n", file="Done_ImageFiles.txt", append=FALSE)      
}


### 
###  Updated May20, 2015
###
## for uning multiple cores 
extractFunc <- function(f.sample, opt.plotonly=TRUE) {
   xset.asample <- extractSampleInfo2(f.sample)
   peaks.sample <- extract_peak_list_samples2(xset.asample, ctype="TIC", offset=1.5, opt.plotonly)
}
 
### 
###  Updated May20, 2015
###
generateSpectrumPlot.multicore <- function(nCluster, fnames, isPlotOnly=TRUE) {      
   if(DEBUG) cat("# Generate Spectrum Plots with Multiple Core\n")
   #print(fname.list)
   
   # Calculate the number of cores
   ncl <- makeCluster(nCluster)  
   
   clusterExport(ncl, list("extractFunc", "extractSampleInfo2", "xcmsRaw", "extract_peak_list_samples2",
                           "getEIC", "plotEIC","extract_RT_forEachPeak", "DEBUG","get_RTofeachPeak"))
   
   clusterApply(ncl, fnames, extractFunc, opt.plotonly=isPlotOnly)
   # parLapply(ncl, fname.list, extractFunc, opt.plotonly=isPlotOnly)
   stopCluster(ncl)
   
   # To check the process is done or not
   cat("Done -", length(fnames),"Samples's Image Files were Generation\n", file="Done_ImageFiles.txt", append=FALSE)      
}



### 
###  Updated May21, 2015
###
## for uning multiple cores 
## print.on : multicore does not support this cat/print because it doesn't make sense 
quantifictionFunc <- function(f.sample, print.on=FALSE, use.blank, threshold.matchFactor, internalStd, lib.calicurv, cmpdlist, final_PeakProfile_blank) 
{
      RunPlotOnly <- FALSE
      not_PRINT_MZINT4DB <- FALSE 
  
      f.sample.basename <- basename(f.sample)
      if(print.on) {
          cat(paste("\n", i, ") Compound Profiling and Quantifying:", sep=''), f.sample.basename, "\n")
      }
      
      ## should be updated (remove peakFind function; data structure because it doesnot use anymore)  
      if( print.on ) { cat("\t >> Extracting Spectrum Information \n") }
      xset.asample <- extractSampleInfo2(f.sample)
      
      ## peak picking for samples using TIC or EIC (peak's RT & Intensity)
      if( print.on & DEBUG ) {  cat("\t >> Extracting peak list \n")  }
      peaks.sample <- extract_peak_list_samples2(xset.asample, ctype="TIC", offset=1.5, RunPlotOnly)
      # cat("peaks.sample:\n"); print(peaks.sample/60)
      
      ## to check missing peak
      if (FALSE & DEBUG) {
          ofilename <- paste(sub(".mzXML|.CDF","", f.sample.basename, ignore.case = TRUE),"_extractedPeaks_temp.csv", sep='')
          write.csv(peaks.sample/60, file=ofilename, quote=FALSE)
      }
      
      ## RI calculation using Alkane Std
      if ( print.on ) { cat("\t >> Get RI for each peak \n") }
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
    if (print.on) {  cat("\t >> Identifying peaks \n") }
    ## Including Additional Information: Area, RTstart & RTend
    profiled_peaks <- compoundIdentify3(peak_samples_ri, xset.asample, lib.peak, alkaneInfo, RI.Variation, isBLANK=FALSE, print_mzInt=not_PRINT_MZINT4DB)
    # head(profiled_peaks)
    if (DEBUG) {
      ofilename <- paste(sub(".mzXML|.CDF","", basename(f.sample), ignore.case = TRUE),"_profiledPeaksTMP.csv", sep='')
      write.csv(profiled_peaks, file=ofilename, quote=TRUE)
    }
    
    ## -- select only the highest score peaks
    if (print.on) { cat("\t >> arrange profiled peaks \n")  }
    final_PeakProfile <- arrangeProfiledPeaks2(profiled_peaks, SampleType)
    if (print.on & DEBUG) { cat("final_PeakProfile:"); print(final_PeakProfile) }
    
    # Subtract BLANK Peak Areas
    # if there is a blank sample, then substract the area from sample's area
    if( use.blank &  ! is.null(xset.blank) ) {            
      if( print.on ) {  cat("\t >> subtract blank peaks' area \n") }
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
          if(print.on & DEBUG) {
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
          if(print.on & DEBUG) {
            cat("\t subtracted sample area:", final_PeakProfile[sp.idx, 'Area'],"\n")
          } 
        } 
      }
    }
    
    if (print.on & DEBUG) { cat("## final_peakProfile after blank subtraction:\n"); print(final_PeakProfile)  }
    
    # making JSON file for Profiled Peak View
    # if (CREATE_JSON_FILE) {
      
      ofilename <- paste(sub(".mzXML|.CDF","", basename(f.sample), ignore.case = TRUE),"_spectrum.json", sep='')
      create_json_file(ofilename, xset.asample$xraw@scantime, xset.asample$xraw@tic,
                       final_PeakProfile$RT, final_PeakProfile$Intensity, final_PeakProfile$Compound )
    # }
    
    ## Quantification        
    checkInternalStd <- existInternalStd(internalStd, final_PeakProfile, lib.peak)
    # if (i==1) checkInternalStd <- FALSE
    
    if ( internalStd != 'NONE' & checkInternalStd == TRUE ) {
      ## screening with Match Factor using threshold 
      if (print.on) { cat("\t >> Screening with Match Factor\n") }
      final_PeakProfile.screened <- screeningWithMatchFactor(final_PeakProfile, threshold.matchFactor)
      # head(final_PeakProfile.screened)
      # nrow(final_PeakProfile); nrow(final_PeakProfile.screened)
      
      if (print.on) { cat("\t >> Quantifying identified peaks\n") }
      # unitConv 1 - uM, 1000 - mM ( Serum and Urine -> should be same unit (uM))
      # quantifiedResult <- quantification(final_PeakProfile, lib.peakcal, internalStd, f.sample, stype=SampleType, unitConv=1000) # 1 or 1000 mM, uM
      quantifiedResult <- quantification(final_PeakProfile.screened, lib.peak, lib.calicurv, internalStd, f.sample, stype=SampleType, unitConv="mM") 
      # nrow(quantifiedResult)  
      
      if (print.on & DEBUG) {
        cat("## quantified Result:\n")
        print(quantifiedResult)
        # hmdbID, Area, Concentration 
      }
      
      if (print.on) { cat("\t >> Generating FinalReport for", basename(f.sample), "\n") }
      finalReport <- genFinalReport(final_PeakProfile.screened, quantifiedResult) 
      colnames(finalReport)[1] <- "HMDB_ID"
      
      if (print.on & DEBUG) { cat("finalReport$Concentration:\n"); print(finalReport$Concentration) }
      Concentration2 <- check.Concentration(finalReport$Concentration) 
      finalReport <- cbind(finalReport, Concentration2) 
      if (print.on & DEBUG) { cat("# finalReport:\n"); print(finalReport); }
      
      names(finalReport) 
      finalReport.All <- merge(cmpdlist, finalReport, by=c('HMDB_ID','CompoundWithTMS'), all.x=TRUE)
      finalReport.All <- finalReport.All[order(finalReport.All$SeqIndex), ]
      rownames(finalReport.All) <- c(1:nrow(finalReport.All))
      
      ofilename <- paste(sub(".mzXML|.CDF","", f.sample.basename, ignore.case = TRUE),"_profiled.csv", sep='')
      # names(finalReport.All)
      finalReport.All <- finalReport.All[,c("SeqIndex", "HMDB_ID", "Compound", "CompoundWithTMS", "RT_min","RT","RI","Intensity",
                                            "MatchFactor", "RI.Similarity","Area","RT.start","RT.end","Concentration2")]                  
      if (print.on & DEBUG) { 
        cat("finalReport All:\n"); print(head(finalReport.All)); 
      }
      
      ## exclude NA or MP(Multiple Peak) cases  
      finalReport.All <- finalReport.All[which( (!is.na(finalReport.All$Concentration2)) & 
                                                  (finalReport.All$Concentration2 != "MP") ), ]   
      
      outColnames <- c("SeqIndex", "HMDB_ID", "Compound", "CompoundWithTMS", "RT_min","RT","RI","Intensity",
                       "MatchFactor", "RI.Similarity","Area","RT.start","RT.end","Concentration")
      write.table(finalReport.All, file=ofilename, quote=TRUE, row.names=FALSE, col.names=outColnames, sep=",")
      if (print.on & DEBUG) { cat("# finalReport All:\n"); print(finalReport.All); }
      
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
  
      ## Disabled because of multiple cores
      if ( FALSE ) 
      {
            if( is.null(final.Concentration) ) {
              final.Concentration <- tmp.Concentration
            } else {
              final.Concentration <- merge(final.Concentration, tmp.Concentration, by=c('HMDB_ID', 'Compound'), sort=FALSE, all=TRUE)
            }
            
            if (print.on & DEBUG) { cat("\n\n final.Concentration:\n"); print(final.Concentration) }
      }      
    } else {
      ## No internal STD found --> should this be kept???????
      if (FALSE)  {
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

    return (tmp.Concentration)
}

mergeConcTable <- function( conc.list )
{
      # column names: HMDB_ID, Compound, sample_ID
       conc.all <- conc.list[[1]]
      for (i in 2:length(conc.list))  {
        conc.all <- merge(conc.all, conc.list[[i]], by=c("HMDB_ID","Compound"), sort=FALSE, all=TRUE)
      }
      
      return (conc.all)
}

### 
###  Updated May21, 2015
###
quantification.multicore <- function(nCluster, fnames, use.blank, threshold.matchFactor, internalStd, lib.calicurv, cmpdlist, final_PeakProfile_blank) {      
  # if(DEBUG) 
    cat("\n# Quantifying spectrum peaks with Multiple Core (nCores:", nCluster, ")\n")
  #print(fname.list)
  
  # Calculate the number of cores
  ncl <- makeCluster(nCluster)  
  
  clusterExport(ncl, list("DEBUG", "extractSampleInfo2", "xcmsRaw","extract_peak_list_samples2", "getEIC","extract_RT_forEachPeak", "get_RTofeachPeak",
                          "get_RI_for_samples2","calc_RI","peak_alkane_std", "compoundIdentify3", "getMZIntensityofEachPeak2", "getPeakRange2","getPeakArea3",
                          "getScan","getPeakArea2","RI.Variation","lib.peak","find_similar_peaks","calcMFscore","arrangeProfiledPeaks2","xset.blank",
                          "create_json_file","spectrumToJSON.fullSpectrum","spectrumToJSON.profile","existInternalStd","screeningWithMatchFactor",
                          "quantification","getSamePeakArea", "calibration","calcConcentration","genFinalReport","check.Concentration","alkaneInfo"))
  
  conc.set <- clusterApply(ncl, fnames, quantifictionFunc, print.on=FALSE, use.blank=use.blank, threshold.matchFactor=threshold.matchFactor, 
                           internalStd=internalStd, lib.calicurv=lib.calicurv, cmpdlist=cmpdlist, final_PeakProfile_blank=final_PeakProfile_blank)
  
  stopCluster(ncl)

  return( conc.set )
}


# file.alkane <- fileList$alkaneFile
extractBlankInfo <- function(file.blank) {
  if (DEBUG)  cat("\n## Reading data from Blank spectrum (", basename(file.blank), ")\n")  
  xset <- xcmsRaw(file.blank, profstep=1, profmethod='binlin', scanrange=NULL) ## smooth  
  return(xset)  
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
  peak_matrix <- get_RTofeachPeak(peak_matrix, wsize=3) 
  
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
    if(nrow(tmp) > 1) {
      tmp.apex <- tmp[order(tmp$intensity, decreasing=TRUE),][1,]      
      if( !(tmp.apex$rt %in% rt.new) ) { 
        rt.new <- c(rt.new, tmp.apex$rt);
        int.new <- c(int.new, tmp.apex$intensity);
      }
    } else {
      rt.new <- c(rt.new, apexes$rt[i]);
      int.new <- c(int.new, apexes$intensity[i]);
    }
  }  
  return( data.frame(rt=rt.new, intensity=int.new) );  
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
  
  return(as.numeric(as.character(ri)))    
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



## without extended Alkane Standard
# asample.peakInfo <- peak_samples_ri
# xset.one <- xset.asample
# print_mzInt=FALSE; to make library update 
compoundIdentify3 <- function(asample.peakInfo, xset.one, lib.peak, alkaneInfo, RI.Variation=0.03, isBLANK=FALSE, print_mzInt=FALSE)
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
  if( print_mzInt ) {
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
  }
  
  if( FALSE) {
    cat("peak_mzInt_list\n"); print(peak_mzInt_list)
    for(k in 1:length(peak_mzInt_list)) {
      cat(peak_mzInt_list[[k]]$rt/60,"\t",peak_mzInt_list[[k]]$peakArea,"\t",peak_mzInt_list[[k]]$peakRTStart,"\t",peak_mzInt_list[[k]]$peakRTEnd,"\n")
    }
  }
  ## note: the length of each vector should be same. if not, the function will be stop and give message
  maxItensity <- max(asample.peakInfo[,'intensity'])  
  rtRange <- range(xset.one@scantime)

  ## for each peak
  ## identifying compounds using each peak's RI and RT/m.z/intensity with library
  peakTotal <- length(peak_rt_vec)
  identifiedList <- NULL # collecting identified information for all peaks in a sample spectrum
  for (j in 1:peakTotal) {
      if (FALSE) { ## for testing 
          # j <- 12
          peak_mzInt_list[[j]]$rt/60
          cat("## peak", j ,"th RT:", peak_rt_vec[j],"\n")
      }
      
      identified <- NULL
      
      # use peak location/index for no alkane covering
      peakRT <- as.numeric(asample.peakInfo[j,"rt"])
      relativePeakRT <- (peakRT - rtRange[1]) / (rtRange[2] - rtRange[1])
      
      ## step 1) using RI, screening candidate 
      RI.sample <- as.numeric(asample.peakInfo[j,"RI"])
      RT.sample <- as.numeric(asample.peakInfo[j,"rt"])  
      
      if( FALSE ) cat(j, "th   RI:",RI.sample, "RT:", round(RT.sample/60,2), "\n" )
      
      if (RI.sample > 0) {  ## not -1  # if -1 then exclude in the previous modules!!!
          ## get candidate screening with RI
          RI.offset <- RI.sample * RI.Variation ## +/- 3% variation
          lib.matched <- lib.peak[which(lib.peak$RI > (RI.sample - RI.offset) & lib.peak$RI < (RI.sample + RI.offset)), ]
          if ( FALSE & DEBUG) {
            cat("RI.offset:",RI.offset ," RI Range:",(RI.sample - RI.offset), " ~ ", (RI.sample + RI.offset),"\n" )
            cat("lib.matched: \n"); print(lib.matched[,  c('Compound','RI','RT')])
          }
          
          sample_mzs_vec <- peak_mzInt_list[[j]]$mzInt[,"mz"] ## from xcmsRaw
          sample_mz_int_vec <- peak_mzInt_list[[j]]$mzInt[,"intensity"] ## from xcmsRaw
          peakIntensity <- peak_mzInt_list[[j]]$peakIntensity # just for adding addition information
          peakArea <- peak_mzInt_list[[j]]$peakArea
          peakRTStart <- peak_mzInt_list[[j]]$peakRTStart
          peakRTEnd <-  peak_mzInt_list[[j]]$peakRTEnd
          peakType <- peak_mzInt_list[[j]]$peakType
        
          ## step 2) for selected candidate compounds, calculate scores like Match Factor and others?
          if (length(lib.matched$RI) > 0) {          
              # cat("\n##### Peak ", j, " - Sample's RT:", RT.sample, "(", round(RT.sample/60, 2), ")  RI:", RI.sample, "\n"
              #     , file=ofile.identify, append=TRUE)
              
              ## identifying a best matched compound from library
              ## add final decision (compound name) with the similarity scores 
              TScore <- 0;
              RIScore <- 0;
              # nearRelPeakRTscore <- 0;
              ## nearRelPeakRT.min <- min(abs(lib.matched$relativePeakRT - relativePeakRT))
          
              identified.trace <- NULL; ## for trace
              for(k in 1:length(lib.matched$RI)) {  ## repeat for selected candidates
                # k<-1
                ## calculate scores (MF_Score, Prob, ...)      
                alib.matched <- lib.matched[k,]
                
                # RI closest (100%) using RI.offset, 2*RI.offset is the maximum range
                RI.similarity <- round((2*RI.offset - abs(alib.matched$RI - RI.sample)) / (2*RI.offset) * 100, 2)
                
                ref_MZS_vec <- as.numeric(unlist(strsplit(as.character(alib.matched$MZ), split=" ")))
                ref_INT_vec <- as.numeric(unlist(strsplit(as.character(alib.matched$Intensity), split=" ")))                
                
                if( FALSE ) {
                    cat("ref_MZS_vec\n"); print(ref_MZS_vec)
                    cat("ref_INT_vec\n"); print(ref_INT_vec)
                    cat("sample_mzs_vec\n"); print(sample_mzs_vec)
                    cat("sample_mz_int_vec\n"); print(sample_mz_int_vec)
                }
                
                # finding matched m/z for a peak
                round.digit <- 0 # 0 or 2; not 1
                lst <- find_similar_peaks(ref_MZS_vec, ref_INT_vec, sample_mzs_vec, sample_mz_int_vec, round.digit)
                if (length(lst$MZS_vec_tmp)==0) { 
                    cat("# ERROR - compound:", alib.matched$CompoundWithTMS,"\n");
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
                # print(MFscore); 
                
                ## calculate matching number of m/z which is more important than anything else
                matchMZrate <- length(lst$mzs_vec_tmp) / length(ref_MZS_vec) * 100
                matchMZnum  <- length(lst$mzs_vec_tmp)
                sampleMZnum <- length(sample_mzs_vec)
                
                # nearRelPeakRT    <- abs(alib.matched$relativePeakRT - relativePeakRT)
                ## nearRelPeakRTscore.tmp <- 100 * nearRelPeakRT.min / abs(alib.matched$relativePeakRT - relativePeakRT)
                
                # correlation (100%) with matched intensity 
                cor.pearson <- cor(lst$INTS_vec_tmp, lst$mz_int_vec_tmp, method="pearson")
                cor.spearman <- cor(lst$INTS_vec_tmp, lst$mz_int_vec_tmp, method="spearman")
                
                # tmp.TScore =  0.3 * MFscore/10 + 0.1 * (cor.spearman * 100) + 0.6 * matchMZrate
                # tmp.TScore =  0.3 * RI.similarity + 0.2 * MFscore/10 + 0.5 * matchMZrate
                # tmp.TScore =  0.4 * RI.similarity + 0.3 * nearRelPeakRTscore.tmp + 0.3 * MFscore/10 
                
                ## tmp.TScore =  0.7 * RI.similarity + 0.3 * MFscore/10 
                tmp.TScore =  0.3 * MFscore/10 + 0.2 * RI.similarity + 0.5*matchMZrate
                
                ## cat(MFscore, nearRelPeakRTscore.tmp, RI.similarity, tmp.TScore, "\n")
                
                # if ( nearRelPeakRTscore < nearRelPeakRTscore.tmp 
                # RI.similarity: 70 --> 90 (Dec 16, 2014)
                if ( (RI.similarity > 90) & (RIScore < RI.similarity) & (!is.na(tmp.TScore) & (tmp.TScore > TScore)) ) {
                    TScore <- tmp.TScore
                    RIScore <- RI.similarity
                    # nearRelPeakRTscore <- nearRelPeakRTscore.tmp
                                
                    identified <- c(
                        hmdbID=as.character(lib.matched[k,]$HMDB_ID),
                        CompoundWithTMS=as.character(lib.matched[k,]$CompoundWithTMS),
                        RT_min=round(RT.sample/60, 3),
                        RT=RT.sample,
                        RI=RI.sample,
                        Intensity=peakIntensity,
                        MatchFactor=MFscore,
                        RI.Similarity=RI.similarity,
                        Corr.Spearman=round(cor.spearman * 100, 2),
                        matchMZrate=round(matchMZrate,2), 
                        matchMZnum=matchMZnum,
                        sampleMZnum=sampleMZnum,
                        nearRelPeakRTscore=NULL,
                        TScore=round(TScore, 2),
                        Area=peakArea,
                        RT.start= peakRTStart,
                        RT.end= peakRTEnd,
                        peakType = peakType
                    )
                    # print(identified)
                }
                
                ## to verify result
                tmp.identified <- c(
                    hmdbID=as.character(lib.matched[k,]$HMDB_ID),
                    CompoundWithTMS=as.character(lib.matched[k,]$CompoundWithTMS),                  
                    RI=alib.matched$RI,
                    Intensity=peakIntensity,
                    MatchFactor=MFscore,
                    RI.Similarity=RI.similarity,
                    Corr.Spearman=round(cor.spearman * 100, 2),
                    matchMZrate=round(matchMZrate,2),   
                    matchMZnum=matchMZnum,
                    sampleMZnum=sampleMZnum,
                    nearRelPeakRTscore=NULL,
                    TScore=round(tmp.TScore, 2),
                    Area=peakArea,
                    RT.start= peakRTStart,
                    RT.end= peakRTEnd,
                    peakType = peakType
                )
                
                identified.trace <- rbind(identified.trace, tmp.identified) 
              }
              ## ??    
              # lst_tmp <- check_db_match_RI_get_compound(lib.peak, ri, sample_mzs, sample_mz_int)  
              
              ## just show the list of identified candidates
              # rownames(identified.trace) <- c(1:nrow(identified.trace) )
              # identified.trace <- rbind(identified.trace, identified)  
              if( FALSE & DEBUG ) {
                  cat("##########################################################\n")
                  cat("fileName:", basename(xset.one@filepath[1]), "  RT:", round(RT.sample/60,3),"\n")
                  rownames(identified.trace) <- c(1:nrow(identified.trace))
                  print(identified.trace)
              }
          }
        
          identifiedList <- rbind(identifiedList, identified)        
        
      }
      
      ## ========================================================
      ## if no matched RI from library because of no alkane    
      if ( RI.sample < 0 ) {    
          # This part covers the peak which does not have alkane
          
          ## get candidate screening with RI of max/min alkane range
          if ( RT.sample < alkaneInfo$RTmin )  {                
              lib.matched <- lib.peak[which(lib.peak$RI < alkaneInfo$RImin), ]
          } else if ( RT.sample > alkaneInfo$RTmax ) {
              lib.matched <- lib.peak[which(lib.peak$RI > alkaneInfo$RImax), ]
          } else {
              cat("!!! Found another case RT:", RT.sample, "\n")
              cat("alkane detail:\n"); print(alkaneInfo)
              stop("Error in compound profiling")
          } 
          
          # cat("# N matched with lib compounds:", nrow(lib.matched), " a peak's RT:", asample.peakInfo[j,"rt"]/60, "\t j:", j, "\n");
          # print(lib.matched[,c("Compound","name_short","RT","RI")])
          
          sample_mzs_vec <- peak_mzInt_list[[j]]$mzInt[,"mz"] ## from xcmsRaw
          sample_mz_int_vec <- peak_mzInt_list[[j]]$mzInt[,"intensity"] ## from xcmsRaw
          peakIntensity <- peak_mzInt_list[[j]]$peakIntensity # just for adding addition information
          peakArea <- peak_mzInt_list[[j]]$peakArea
          peakRTStart <- peak_mzInt_list[[j]]$peakRTStart
          peakRTEnd <-  peak_mzInt_list[[j]]$peakRTEnd
          peakType <- peak_mzInt_list[[j]]$peakType
          
          
          ## step 2) for selected candidate compounds, calculate scores like Match Factor and others?
          if (length(lib.matched$RI) > 0) {
            ## identifying a best matched compound from library
            ## add final decision (compound name) with the similarity scores 
            TScore <- 0;
            RIScore <- 0;
            ## nearRelPeakRTscore <- 0;                
            ## nearRelPeakRT.min <- min(abs(lib.matched$relativePeakRT - relativePeakRT))
            
            identified.trace <- NULL; ## for trace
            for(k in 1:length(lib.matched$RI)) {  ## repeat for selected candidates
              ## k <- 6 
              ## calculate scores (MF_Score, Prob, ...)      
              alib.matched <- lib.matched[k,]
              
              # RI closest (100%) using RI.offset, 2*RI.offset is the maximum range
              RI.similarity <- NA
              
              ref_MZS_vec <- as.numeric(unlist(strsplit(as.character(alib.matched$MZ), split=" ")))
              ref_INT_vec <- as.numeric(unlist(strsplit(as.character(alib.matched$Intensity), split=" ")))                
              # cat(as.character(alib.matched$metabolite_name), "ref m/z:\n"); print(ref_MZS_vec)
              # cat("sample:\n"); print(sample_mzs_vec)
              
              if( FALSE ) {
                cat("ref_MZS_vec\n"); print(ref_MZS_vec)
                cat("ref_INT_vec\n"); print(ref_INT_vec)
                cat("sample_mzs_vec\n"); print(sample_mzs_vec)
                cat("sample_mz_int_vec\n"); print(sample_mz_int_vec)
              }
              
              # finding matched m/z for a peak
              round.digit <- 0 # 0 or 2; not 1
              lst <- find_similar_peaks(ref_MZS_vec, ref_INT_vec, sample_mzs_vec, sample_mz_int_vec, round.digit)
              if (length(lst$MZS_vec_tmp)==0) { 
                cat("# ERROR - compound:", alib.matched$CompoundWithTMS, "\n");
                stop("No m/z matched")
              } else {
                if (DEBUG & FALSE) {         
                  cat("## Match Factor) # of m/z matched:", length(lst$MZS_vec_tmp), " ref m/z:", length(ref_MZS_vec)
                      , " sample m/z:", length(sample_mzs_vec), "\n")
                }
              }
              #lst20 <- find_similar_peaks(ref.mzInt20$mz, ref.mzInt20$intensity, sample.mzInt20$mz, sample.mzInt20$intensity, round.digit)
              
              ## Match Factor (1000)
              # MFscore <- get_mathc_factor(MZS_vec, INTS_vec, mzs_vec, mz_int_vec)
              ## split the values of the mzs and intensities both library and sample                                    
              MFscore <- calcMFscore(lst$MZS_vec_tmp, lst$INTS_vec_tmp, lst$mzs_vec_tmp, lst$mz_int_vec_tmp)
              #MFscore20 <- calcMFscore(lst20$MZS_vec_tmp, lst20$INTS_vec_tmp, lst20$mzs_vec_tmp, lst20$mz_int_vec_tmp)
              
              # print(MFscore); 
              
              ## calculate matching number of m/z which is more important than anything else
              # matchMZrate <- length(lst$mzs_vec_tmp) / length(sample_mzs_vec) * 100
              matchMZrate <- length(lst$mzs_vec_tmp) / length(ref_MZS_vec) * 100
              matchMZnum  <- length(lst$mzs_vec_tmp)
              sampleMZnum <- length(sample_mzs_vec)
              
              #matchMZrate20 <- length(lst20$MZS_vec_tmp) / length(ref.mzInt20$mz) * 100
              
              # nearRelPeakRT    <- abs(alib.matched$relativePeakRT - relativePeakRT)
              # 100 = same relative rt location, 0 = very far
              ## nearRelPeakRTscore.tmp <- 100 * nearRelPeakRT.min / abs(alib.matched$relativePeakRT - relativePeakRT)
              
              # correlation (100%) with matched intensity 
              cor.pearson <- cor(lst$INTS_vec_tmp, lst$mz_int_vec_tmp, method="pearson")
              cor.spearman <- cor(lst$INTS_vec_tmp, lst$mz_int_vec_tmp, method="spearman")
              #cor.spearman20 <- cor(lst20$INTS_vec_tmp, lst20$mz_int_vec_tmp, method="spearman")
              
              # tmp.TScore =  0.4 * MFscore/10 + 0.1 * (cor.spearman * 100) + 0.5 * matchMZrate
              # tmp.TScore <-  0.5 * MFscore/10 + 0.1 * (cor.spearman * 100) + 0.4 * matchMZrate
              # tmp.TScore <-  0.5 * MFscore/10 + 0.5 * matchMZrate
              
              if (matchMZrate == 100) {
                tmp.TScore <-  0.5 * MFscore/10 + 0.5 * matchMZrate
              } else {
                ## tmp.TScore <-  0.5 * MFscore/10 + 0.5 * nearRelPeakRTscore.tmp
                tmp.TScore <-  0.5 * MFscore/10 + 0.5 * matchMZrate
              }
              
              # if (nearRelPeakRTscore < nearRelPeakRTscore.tmp & ( !is.na(tmp.TScore) & (tmp.TScore > TScore)) ) {
              if ( ( !is.na(tmp.TScore) & (tmp.TScore > TScore)) ) {
                TScore <- tmp.TScore
                RIScore <- RI.similarity
                ## nearRelPeakRTscore <- nearRelPeakRTscore.tmp
                
                identified <- c(
                  hmdbID=as.character(lib.matched[k,]$HMDB_ID),
                  CompoundWithTMS=as.character(lib.matched[k,]$CompoundWithTMS),
                  RT_min=round(RT.sample/60, 3),
                  RT=RT.sample,
                  RI=RI.sample,
                  Intensity=peakIntensity,
                  MatchFactor=MFscore,
                  RI.Similarity=RI.similarity,
                  Corr.Spearman=round(cor.spearman * 100, 2),
                  matchMZrate=round(matchMZrate,2),
                  matchMZnum=matchMZnum,
                  sampleMZnum=sampleMZnum,
                  nearRelPeakRTscore=NULL,
                  #matchMZrate20=matchMZrate20,
                  #MFscore20=MFscore20,
                  TScore=round(TScore, 2),
                  Area=peakArea,
                  RT.start= peakRTStart,
                  RT.end= peakRTEnd,
                  peakType = peakType
                )              
              }
              
              ## to verify result
              tmp.identified <- c(
                hmdbID=as.character(lib.matched[k,]$HMDB_ID),
                CompoundWithTMS=as.character(lib.matched[k,]$CompoundWithTMS),
                RI=alib.matched$RI,
                Intensity=peakIntensity,
                MatchFactor=MFscore,
                RI.Similarity=RI.similarity,
                Corr.Spearman=round(cor.spearman * 100, 2),
                matchMZrate=round(matchMZrate,2),    
                matchMZnum=matchMZnum,
                sampleMZnum=sampleMZnum,
                nearRelPeakRTscore=NULL,
                #matchMZrate20=matchMZrate20,
                #MFscore20=MFscore20,
                TScore=round(tmp.TScore, 2),
                Area=peakArea,
                RT.start= peakRTStart,
                RT.end= peakRTEnd,
                peakType = peakType
              )
              
              identified.trace <- rbind(identified.trace, tmp.identified) 
            } ## end of for(k in 1:length(lib.matched$RI)) 
            
            ## show the list of identified candidates
            if( ! TRUE ) {
              cat("##########################################################\n")
              cat("## NO matched RI list \n")
              cat("fileName:", basename(xset.one@filepath[1]), "  RT:", round(RT.sample/60,3),"\n")
              rownames(identified.trace) <- c(1:nrow(identified.trace))
              print(identified.trace)
            }
            
          }  # if (length(lib.matched$RI) > 0)
          
          ## to verify result 
          # print(list( tmp.identified ) )
          
          ## step 3) report data 
          # identifiedList[length(identifiedList)+1] <- identified        
          identifiedList <- rbind(identifiedList, identified)           
      }
      ## ========================================================
      
  } # for each peak  
  
  rownames(identifiedList) <- c(1:nrow(identifiedList))
  if( DEBUG ) {
      cat("identifiedList:\n"); print(identifiedList)
      # cat("lib.peak:\n"); print(lib.peak[, c("HMDB_ID","Compound","CompoundWithTMS")])
  }
  
  # no need anymore because the hmdb id is included in the set
  # identifiedList <- merge( lib.peak[, c("HMDB_ID","CompoundWithTMS")], identifiedList, by=c("CompoundWithTMS"), sort=FALSE)

  # as.data.frame(identifiedList)
  
  ## return identified compounds with additional informations 
  ## such as (RT, RI, Intensity, Area (RT start/end), Similarity Measures)    
  return( as.data.frame(identifiedList) )
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

## screening with highest TScore from the matched list 
arrangeProfiledPeaks2 <- function(profiled_peaks, stype=NULL)
{
             
  profiled_final <- NULL
  unique_complist <- unique(profiled_peaks$Compound)
  for (j in 1:length(unique_complist))  {
    # j <- 12
    tmpdata <- profiled_peaks[which(profiled_peaks$Compound == unique_complist[j]), ]    
    tmpdata <- tmpdata[order(as.double(as.character(tmpdata$TScore)), decreasing=TRUE), ] 
    final.comp <- tmpdata[1, ]
    if(! TRUE) { cat("compd:", unique_complist[j], "\n"); cat("tmpdata:\n"); print(tmpdata);  cat("final.comp:\n"); print(final.comp) }
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
calibration <- function(dbLib, hmdbID, relative_area_comp) {
    # cat("## compound:", compound, "\t area_ratio:", relative_area_comp,"\n")  
    libVec <- dbLib[which(as.character(dbLib$HMDB_ID) == hmdbID), ];
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

# profiled_peaks <- profiled.result  
getSamePeakArea <- function(profiled_peaks) {  
  profiled_final <- NULL
  unique_hmdbIDlist <- unique(profiled_peaks$hmdbID)
  # table(profiled_peaks$hmdbID)
  for (hmdbID in unique_hmdbIDlist)  {
    # hmdbID <- "HMDB00883"
    tmpdata <- profiled_peaks[which(profiled_peaks$hmdbID == hmdbID), ]
    
    if(FALSE) {cat("tmp data in getSamePeakArea func:\n"); print(tmpdata); print(typeof(tmpdata$Area)) }
    sumOfAreas <- sum(as.numeric(as.character(tmpdata$Area))) # Area is after subtract the blank 
    NPeaks <- nrow(tmpdata)
    final.comp <- c(hmdbID, sumOfAreas, NPeaks)
    profiled_final <- rbind(profiled_final, final.comp)
  }  
  rownames(profiled_final) <- c(1:nrow(profiled_final))
  colnames(profiled_final) <- c("hmdbID","Area","NPeaks")
  
  return(as.data.frame(profiled_final))
}


# profiled.result <- final_PeakProfile.screened
# lib <- lib.peak
# stype <- ORGANIC_ACID  
# unitConv <- "mM"/"uM"
# internalStdCmpd <- "Ribitol"
quantification <- function(profiled.result, lib, lib.curve, internalStdCmpd, sample_file, stype, unitConv="mM") {
  
    cat("## quantification input peaks:", nrow(profiled.result), "\n")

    # combine areas for the same compound
    hmdbIDwithArea <- getSamePeakArea(profiled.result)
    # hmdbIDwithArea
    nrow(hmdbIDwithArea)
      
    internalStd.hmdbID <- as.character(lib[which(lib$Compound == internalStdCmpd), 'HMDB_ID'])
    if( DEBUG) cat("\t# selected internal standard compound:", internalStdCmpd, "(", internalStd.hmdbID,")\n\n")
    area.internalStd <- as.double(as.character(hmdbIDwithArea[which(hmdbIDwithArea$hmdbID == internalStd.hmdbID), "Area"]))
    cat("\t# area.internalStd:", area.internalStd,"\n")
    
    if (DEBUG) { 
        cat("## internalStdCmpd:\n"); 
        print(profiled.result[which(as.character(profiled.result$Compound) == internalStdCmpd), ])
    }
    
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
    
    # hmdbIDwithArea: hmdbID, +Compound, +Concentration, Area,  NPeaks
    cat("\n## calculated concentration:\n")
    quantifiedList <- NULL
    for ( hmdbID in hmdbIDwithArea$hmdbID ) {
        aCmpd <- hmdbIDwithArea[which(hmdbIDwithArea$hmdbID == hmdbID), ]
        area_ratio <- as.double(as.character(aCmpd$Area)) / area.internalStd
                
        if ( (hmdbID != internalStd.hmdbID) ) {  
            if(area_ratio > 0) {
                calc <- calibration(lib.curve, hmdbID, area_ratio) # / unitConv  # unitConv 1 - uM, 1000 - mM
                calc <- ifelse(calc < 0, 0, calc)  
            } else {
                calc <- 0
            }
        } else {
            calc <- 'ISTD' # internal standard
        }
       
        if (DEBUG) { 
          cat("\t# ", hmdbID, "\t area:", as.double(as.character(aCmpd$Area)), "\t conc:", calc,"\n")
        }
        
        quantifiedList <- rbind(quantifiedList, cbind(aCmpd[,c(1,2)], Concentration=calc) ) 
    }
    
    return (quantifiedList)
}

screeningWithMatchFactor <- function(profiled.result, MFthreshold) {
  profiled.result.screened <- profiled.result[which(as.numeric(as.character(profiled.result$MatchFactor)) > MFthreshold), ]  
  rownames(profiled.result.screened) <- c(1:nrow(profiled.result.screened))
  
  return(profiled.result.screened)
}

## report final for list all with non-detected
# profiled.result <- final_PeakProfile.screened
# quantified.result<- quantifiedResult 
genFinalReport <- function(profiled.result, quantified.result) {
    profiled_final <- NULL
    hmdbIDlist <- as.character(quantified.result$hmdbID)
    # table(profiled_peaks$hmdbID)
    for (hmdbID in hmdbIDlist)  {
        # hmdbID <- "HMDB00162"
        tmpdata <- profiled.result[which(profiled.result$hmdbID == hmdbID), ]  
        tmpdata <- tmpdata[order(as.integer(as.character(tmpdata$Intensity)), decreasing=TRUE), ] 
        
        Concentration <- -999 # Multiple Peaks
        final.comp <- cbind(tmpdata, Concentration)
        conc <- quantified.result[which(quantified.result$hmdbID == hmdbID), "Concentration"]
        # cat("conc:", conc,"\n") 
        if (conc == "ISTD") {
            final.comp[1,"Concentration"] <- conc
        } else {
            final.comp[1,"Concentration"] <- as.double(conc)
        }  
          # final.comp
        profiled_final <- rbind(profiled_final, final.comp)
    }  
    if ( length(profiled_final[which(profiled_final$Concentration == -999), "Concentration"]) > 0 ) {
        profiled_final[which(profiled_final$Concentration == -999), "Concentration"] <- "MP" # Multiple Peaks
    }
    
    profiled_final <- profiled_final[order(as.numeric(as.character(profiled_final$RT))), ]
    return(profiled_final)
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
   
   if( ! (internalStd %in% profiledPeakSet$CompoundWithTMS) ) {
      cat("## Warning: Can not find the interstandard (",internalStd,") in Sample Spectrum.\n\t Program will report without quantification.\n\n")
      return (FALSE)
   }
   if( ! (internalStd %in% libDB$CompoundWithTMS) ) {
     cat("## Warning: Can not find the interstandard (",internalStd,") in the library database.\n\t Program will report without quantification.\n\n")     
     return (FALSE)
   }      
   
   return (TRUE)   
}

check.Concentration<-function(conc) 
{
    for (i in 1:length(conc)) {
        cat("conc[i]"); print(conc[i])
        if(!is.na(conc[i]) & conc[i] != "ISTD" & conc[i] != "NA" & conc[i] == 0) {
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

