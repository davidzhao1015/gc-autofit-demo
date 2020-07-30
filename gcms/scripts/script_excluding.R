

# asample.peakInfo <- peak_samples_ri
# xset.one <- xset.asample  
getPeakInfo_OrganicAcid <- function(asample.peakInfo, xset.one, RI.Variation=0.03)
{
  cat("\n# getPeakInfo_OrganicAcid - processing file:", basename(xset.one$xraw@filepath[1]), "...\n")
  
  peak_rt_vec <- asample.peakInfo[, "rt"]
  ## get rt/mz/intensity/area/... for each peak
  peak_mzInt_list <- getMZIntensityofEachPeak2(xset.one, peak_rt_vec) 
  
  identifiedList <- NULL # collecting identified information for all peaks in a sample spectrum
  for (j in 1:length(peak_rt_vec)) {
    RT.sample <- as.numeric(asample.peakInfo[j,"rt"])  
    peakIntensity <- peak_mzInt_list[[j]]$peakIntensity # just for adding addition information
    peakArea <- peak_mzInt_list[[j]]$peakArea
    peakRTStart <- peak_mzInt_list[[j]]$peakRTStart
    peakRTEnd <-  peak_mzInt_list[[j]]$peakRTEnd        
    
    identified <- c(
      RT_min=round(RT.sample/60, 3),
      RT=RT.sample,
      Intensity=peakIntensity,
      Area=peakArea,
      RT.start= peakRTStart,
      RT.end= peakRTEnd
    )
    identifiedList <- rbind(identifiedList, identified)   
  }
  
  rownames(identifiedList) <- c(1:nrow(identifiedList))
  return( as.data.frame(identifiedList) )  
}


# apex.idx <- peak.scanidx
getPeakRange <- funxtion(xset.one, apex.idx, nScanIndex, )
{
    # threshold <- xset.one$xraw@tic[apex.idx] * 0.03; # threshold
    tmp.idx1 <- ifelse((apex.idx-30) <= 0, 1, (apex.idx-30))
    tmp.idx2 <- ifelse((apex.idx+30) >= nScanIndex, nScanIndex, (apex.idx+30))
    tics <- xset.one$xraw@tic[tmp.idx1 : tmp.idx2] 
    s <- summary(tics)
    # threshold <- s[1] # 1st Quantile 
    threshold <- 10000
    
    # left side to find a RT.start
    idx <- apex.idx
    diff.intensity <- 9999999999;
    while ( (idx > 1) & (diff.intensity > threshold) & (diff.intensity > 0)  ) {
      # 1) slope < threshold or 2) change the sigh of slope to positive/negative
      diff.intensity <- (xset.one$xraw@tic[idx] - xset.one$xraw@tic[idx-1])
      peak.rtmin <- xset.one$xraw@scantime[idx]
      # cat("## RT start:", peak.rtmin, " RT start (min):", peak.rtmin/60, " diff.intensity:", diff.intensity, "\n")
      # if( diff.intensity < 0 | diff.intensity < threshold ) cat("reached the shoulder")
      idx <- idx - 1
    }      
    
    # right side to find a RT.end
    plot(xset.one$xraw@tic[5680:5695])
    idx <- apex.idx
    diff.intensity <- 9999999999;
    n.idx <- length(xset.one$xraw@scantime);
    while ( (idx < n.idx) & (diff.intensity > threshold) & (diff.intensity > 0)  ) {
      # 1) slope < threshold or 2) change the sigh of slope to positive/negative
      diff.intensity <- (xset.one$xraw@tic[idx] - xset.one$xraw@tic[idx+1])
      peak.rtmax <- xset.one$xraw@scantime[idx]
      cat("## RT end:", peak.rtmax, " RT end (min):", peak.rtmax/60, " diff.intensity:", diff.intensity, "\n")
      idx <- idx + 1
      ((idx < n.idx) & (diff.intensity > threshold) & (diff.intensity > 0) )
    }
    # peak.rtmin; peak.rtmax; 
    # peak.rtmin/60; peak.rtmax/60; 
}      


# getPeakArea2 --> getPeakArea3
## area calculation
# xr <- xset.one$xraw;
# peak.rt <- peak_rt_vec[i]
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
  
  ## ?? scale
  return ( round(( area * 9.5), 3) )
}



## from getMZIntensityofEachPeak2 <- function(xset.one, peak_rt_vec) 
## Now, it does not use the peak information from find.peaks()
if (FALSE) {
  rtrange <- xset.one$peaks[which(xset.one$peaks[,'rt']==peak_rt_vec[i]), c("rtmin","rtmax")]
  
  if ( length(rtrange) == 2 ) {
    peak.rtmin <- as.numeric(rtrange["rtmin"])
    peak.rtmax <- as.numeric(rtrange["rtmax"])
  } else if (length(rtrange) > 2) {
    peak.rtmin <- max(rtrange[,"rtmin"])
    peak.rtmax <- min(rtrange[,"rtmax"])          
    if(DEBUG) 
      cat("\t Found matched RT:", peak_rt_vec[i],":", round(peak_rt_vec[i]/60,3),"- peak's RT range:", round(peak.rtmin/60,3), "-", round(peak.rtmax/60,3), "\n")
  } else {
    ## ?? should I keep this which is not matched with peak RT        
    if(DEBUG) 
      cat("\t## cannot find a matched peaks with peak_rt_vec", peak_rt_vec[i],"(", peak_rt_vec[i]/60, ")\n")
    # peak.rtmin <- ifelse( peak.scanidx - 10 > 1, xset.one$xraw@scantime[peak.scanidx - 10], min(xset.one$xraw@scantime) )
    # peak.rtmax <- ifelse( peak.scanidx + 10 <= max(xset.one$xraw@scanindex), xset.one$xraw@scantime[peak.scanidx + 10], max(xset.one$xraw@scantime) )
    
    threshold <- xset.one$xraw@tic[peak.scanidx] * 0.03
    ## left side of a peak 
    diff <- 99999999
    peak.i <- 0
    while( diff > threshold ) {
      diff <- xset.one$xraw@tic[peak.scanidx - peak.i] - xset.one$xraw@tic[peak.scanidx - (peak.i+1)]
      peak.rtmin <- xset.one$xraw@scantime[peak.scanidx - peak.i]
      #cat("i:", peak.i, "diff:", diff, "rtmin:", peak.rtmin, "/", peak.rtmin/60, "int:",xset.one$xraw@tic[peak.scanidx - peak.i], "\n")
      peak.i <- peak.i + 1
    }
    ## right side of a peak 
    diff <- 99999999
    peak.i <- 0
    while( diff > threshold ) {
      diff <- xset.one$xraw@tic[peak.scanidx + peak.i] - xset.one$xraw@tic[peak.scanidx + (peak.i+1)]
      peak.rtmax <- xset.one$xraw@scantime[peak.scanidx + peak.i]
      # cat("i:", peak.i, "diff:", diff, "rtmin:", peak.rtmax, "/", peak.rtmax/60, "int:",xset.one$xraw@tic[peak.scanidx + peak.i], "\n")
      peak.i <- peak.i + 1
    }
  }          
} 






## not used
## alkane standard RT prediction 
if (FALSE) {
  predAlkaneRT <- function(d, maxCn, lastCn)
  {
    if( FALSE ) {
      ALKRT <- c(13.07, 16.27, 19.28, 22.0, 24.53, 26.88, 29.05, 31.1, 33.09, 34.89, 36.64)
      Cn <- c(10,11,12,13,14,15,16,17,18,19,20)
      # fit <- nls(ALKRT ~ a*exp(b*Cn), start=c(a=1, b=0.001))  
      # fit <- nls(ALKRT ~ a+b*log(Cn), start=c(a=1, b=0.001))  
    }
    fit <- nls(ALKRT ~ a+b*log(Cn), data=d, start=c(a=1, b=0.01)) 
    
    Cn.newlist <- c(maxCn:n)
    predALK <- predict(fit, newdata=list(Cn=Cn.newlist))
    
    return(as.data.frame(ALKRT=predALK, Cn=Cn.newlist) )
  }
  # if()
  alkane.ext <- predAlkaneRT(peak_alkane_std, maxCn=max(peak_alkane_std$Cn), lastCn=40)
  peak_alkane_std <- rbind(peak_alkane_std, alkane.ext)      
  cat("alkane.extended\n"); print(peak_alkane_std)
}