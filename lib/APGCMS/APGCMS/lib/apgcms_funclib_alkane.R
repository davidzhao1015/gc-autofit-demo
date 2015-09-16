###########################################################################################
## Functions for the GC-AutoFit 
## Author: Beomsoo Han
## Latest Update: 20150325
############################################################################################


## if program detected more than two Cns within the same peak, eliminated one from the table
getUniqueAlkanePeakList <- function(peak.list, offset.RT=50)
{
    # check highest and standard peak: C20
    idx.c20 <- which(peak.list[,"Cn"] == 20); 
    if (DEBUG) {
      cat("## getUniqueAlkanePeakList\n")
      cat("\t length(idx.c20):", length(idx.c20),"\n")
      cat("## peak.list:\n")
      print(peak.list)
    }    
   
    d.tmp <- peak.list
    d.down.screen <- NULL
    idx2remove <- NULL
    if (length(idx.c20) == 1) {
        for (i in (idx.c20-1):1 ) {
            if( abs(d.tmp[i+1,"RT"] - d.tmp[i,"RT"]) < offset.RT)  {
                idx2remove <- c(idx2remove, i)
            }
        }
        # some Alkane do not have more than C20 
        if( max(d.tmp$RT) > d.tmp$RT[idx.c20] ) { 
            for (i in (idx.c20+1):(nrow(d.tmp)) ) {
                if( abs(d.tmp[i-1,"RT"] - d.tmp[i,"RT"]) < offset.RT)  {
                    idx2remove <- c(idx2remove, i)
                }
            }
        } else {
            cat("\n\n## NOTE: Alkane Standard has upto C20 only\n\n")
        }
    } else {
        cat("\n## Error: cannot find the C20 as a standard in the alkane\n")
        cat("\t length(idx.c20):", length(idx.c20),"\n\n")
        stop()
    }
    
    if (length(idx2remove) > 0) {          
        if (DEBUG) { cat("idx2remove:"); print(idx2remove) }
        # d.tmp <- d.tmp[-idx2remove, ]
        peak.list.unique <- peak.list[-idx2remove, ]
        if (DEBUG) { 
            cat("## checked alkane peaks (eliminating within a peak):\n"); 
            print(peak.list.unique) 
        }
    } else {
      peak.list.unique <- peak.list
    }
    
    return(peak.list.unique)
}

## cleanning false peaks (samle peak area -> assign only one RT)
## if same Cn but quite different RT, then keep both
cleaningFalseAlkanePeaks <- function(peak.list, offset.RT=50)
{
    unique.Cnlist <- unique(peak.list$Cn)
    if (DEBUG) { cat("unique.Cnlist:\n"); print(unique.Cnlist) }
    
    ## for the same CN 
    peak.list.screen <- NULL
    for (Cn in unique.Cnlist) {
        d.tmp <- peak.list[which(peak.list$Cn == Cn), ]
        if (DEBUG) { cat("Cn:", Cn,"\n"); print(d.tmp) }
        
        if (nrow(d.tmp) >= 2) {
            apex.RT <- d.tmp[which(max(as.numeric(as.character(d.tmp$Intensity))) == d.tmp$Intensity), "RT"]
            if (DEBUG)  cat("apex.RT:", apex.RT, "\n")
            idx2remove <- NULL
            for (i in 1:nrow(d.tmp)) {
                # cat("d.tmp[i,RT]:", d.tmp[i,"RT"],"\n"); print(d.tmp[i,"RT"] != apex.RT);
                # cat("abs(apex.RT - d.tmp[i,RT]):", abs(apex.RT - d.tmp[i,"RT"]), "\n"); print( abs(apex.RT - d.tmp[i,"RT"]) < 50) ;
                
                if( (d.tmp[i,"RT"] != apex.RT) & ( abs(apex.RT - d.tmp[i,"RT"]) < 50) ) {
                    idx2remove <- c(idx2remove, i)
                }
            }
            if (length(idx2remove) > 0) {          
                d.tmp <- d.tmp[-idx2remove, ]
            }
        }
        if (DEBUG) print(d.tmp)
        peak.list.screen <- rbind(peak.list.screen, d.tmp)
    }

    # sort by RT
    peak.list.screen <- peak.list.screen[order(peak.list.screen[,"RT"]), ]
    if (DEBUG) { cat("## screened alkane peaks:\n"); print(peak.list.screen) }
        
    return(peak.list.screen)
}


## Adjust peak identification with standard point (C20)
adjustAlkanePeakCn <- function(peak.list, Cn.topIntensity=20) 
{
    if (DEBUG) { cat("## adjusting Alkane Peak Cn\n"); }
    # check whether the top intensity is C20 or Not
    # If not, then the Cn of top intensity should be assigned with C20
    peak.list <- as.data.frame(peak.list)
    CnMaxIntensity <- peak.list[which(peak.list$Intensity == max(peak.list$Intensity)), "Cn"]
    if (CnMaxIntensity != Cn.topIntensity) {
        peak.list[which(peak.list$Intensity == max(peak.list$Intensity)), "Cn"] <- Cn.topIntensity
    } 

    peak.list.adjusted <- cbind(peak.list, Cn_matched=peak.list[,"Cn"])  ## keep original matched Cn values (mz/intensity)
    idx.c20 <- which(peak.list.adjusted[,"Cn"] == Cn.topIntensity)
    if (DEBUG) cat("idx.c20:", idx.c20,"\n")
    
    k <- 1
    for(i in (idx.c20-1):1) {
        peak.list.adjusted[i,"Cn"] <- (20 - k)
        k <- k + 1
    }  
    
    if(nrow(peak.list.adjusted) > idx.c20) {
        k <- 2
        for(i in (idx.c20+1):nrow(peak.list.adjusted)) {
          peak.list.adjusted[i,"Cn"] <- (20 + k)
          k <- k + 2
        }
    }
    
    if (DEBUG) { cat("## alkane peaks (after adjustment Cn):\n"); print(round(peak.list.adjusted, 2)) }
    return(peak.list.adjusted)
}

## manually assigned alkane peak identification
userAssignedAlkanePeakCn <- function(peak.list, user.AlkaneRT)
{
    if (DEBUG) { 
       cat("## Assigning user defined alkane peak Cn\n")  
       write.table(as.character(user.AlkaneRT), file="userDefinedAlkane.txt", append=FALSE, row.names=FALSE, col.names=FALSE)
    }
    
    # check whether the lengths are same or not
    if ( length(user.AlkaneRT) != nrow(peak.list) ) {
        showErrMessage(" Error in argument:\n\t the length of user defined alkane standard peak Cn is different.")
        helpMessage()        
    }

    peak.list.assigned <- as.data.frame(peak.list)
    for (i in 1:nrow(peak.list.assigned)) {        
        if(is.na(user.AlkaneRT[i]) | (user.AlkaneRT[i] == "NA") ) {
              # cat(i," has NA:", peak_alkane_std$Cn[i], "\n")
              peak.list.assigned$Cn[i]<- -99
        } else {
              # cat(i," has value:", peak_alkane_std$Cn[i], "\n")
              peak.list.assigned$Cn[i] <- user.AlkaneRT[i]
        }
    }

    # cat("Excluding mis-identified Cns:",length(which(peak_alkane_std$Cn == -99)),"\n")    
    if( length(which(peak.list.assigned$Cn == -99)) > 0 ) {
        peak.list.assigned <- peak.list.assigned[- which(peak.list.assigned$Cn == -99), ]
    }
    if (DEBUG) { cat("## alkane peaks (after adjustment with user define values):\n"); print(round(peak.list.assigned,2)) }
    
    if( length(which(table(peak.list.assigned$Cn) > 1)) > 0 ) {
        showErrMessage(" Error in user defined alkane:\n\t there are same carbon numbers. It should be unique values")
        if (DEBUG) { cat(" >> Cn:",peak.list.assigned$Cn,"\n\n\n") }
        stop()
    }
    
    return(peak.list.assigned)
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


# xset <- xset.alkane
# extract_peak_list_alkane2 <- function(xset, numOfAlkane, ctype="EIC", offset=7, plotFile=TRUE)  {
## this will generate PLOT image and RT Peaks 
extract_peak_list_alkane2 <- function(xset, numOfAlkane, ctype="EIC", offset=7)  {
  
  ## This is the important part to detect peak correctly; it depends on mzrange
  # mzrange <- t(range(xset@peaks[,"mz"]))            
  mzrange <- t(range(xset@mzrange))
  rtrange <- t(range(xset@scantime))
  # eic_obj <- getEIC(xset, mzrange=mzrange, rtrange=rtrange, rt="corrected")
  # eic_obj <- getEIC(xset, mzrange=mzrange, rtrange=rtrange, rt="raw")  
  eic_obj <- getEIC(xset, mzrange=mzrange, rtrange=rtrange)   ## when xset is xcmsRaw class
  
  # EIC plot generate & save  
  # if ( plotFile ) {
    sampleFile <- sub(".mzXML|.CDF", "", basename(xset@filepath), ignore.case=TRUE)  
    # png(filename = paste("Plot_", ctype,"_", sampleFile,".png", sep=''), width = 1000, height = 800, units = "px", pointsize = 10)
    png(filename = paste("Plot_EIC_", sampleFile,".png", sep=''), width = PNG_WIDTH, height = PNG_HEIGHT, units = "px", pointsize = 12)
    plotEIC(xset, mzrange=mzrange, rtrange=rtrange) ## same as chemstation
    dev.off()
  # }
  
  ## get the RT and Intensity from EIC object
  rt_int_matrix <- eic_obj@eic[1][[1]][[1]]         
  if (is.null(rt_int_matrix[, "intensity"])) stop(paste("rt_int_matrix is null in ", xset@filepaths[1]))
  
  ## select significant peaks & screening apexes only to mapping compounds
  ## use TIC instead of EIC
  if(ctype=="TIC") {
    rt_peak <- extract_RT_forEachPeak(rt_int_matrix, offset, ctype="TIC", xset); 
    # cat("rt_peak:\n"); print(rt_peak)
    
    ## exclude noise peaks
    idx.max <- which(rt_peak$intensity == max(rt_peak$intensity)) ## C20
    
    # calculate Thresholds with the smallest significant peak' intensity 
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

# peak.list <- peaks.alkane    
cleanning_alkaneFaslePeaks <- function(peak.list, RT.offset=50)
{ 
  newPeaklist <- NULL
  if(DEBUG) {  cat("## cleanning_alkaneFaslePeaks (apgcms_funclib):\n")}
  for (i in 1:length(peak.list$rt) ) {
    tmp <- peak.list[which( (peak.list$rt > peak.list$rt[i] - RT.offset) & (peak.list$rt < peak.list$rt[i] + RT.offset) ), ]
    # print(tmp)
    
    if (nrow(tmp) > 1) {
      if(DEBUG) { 
        cat("|--> Neighbor peak was found at RT:", peak.list$rt[i], "\n")      
        print(tmp)
      }
      tmpTop <- tmp[order(tmp$intensity, decreasing = TRUE), ][1,]
      newPeaklist <- rbind(newPeaklist, tmpTop)
    } else {
      newPeaklist <- rbind(newPeaklist, tmp)
    }
  }
  newPeaklist <- unique(newPeaklist)
  rownames(newPeaklist) <- 1:nrow(newPeaklist)
  if(DEBUG) { cat("# final screened data:\n"); print(newPeaklist) }  
  
  return(newPeaklist)
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
# print_mzInt=FALSE; to make library update 
peakIdentify.alkane2 <- function(alkane.peakInfo, xset.one, lib.peak.alkane, print_mzInt=FALSE)
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
  if (FALSE & DEBUG) { 
    cat("peak_mzInt_list:\n"); print(peak_mzInt_list) 
  }
  ## @@@@@@
  ## used for mz/int DB update
  if( print_mzInt ) {
    ofile <- paste(basename(xset.one@filepath[1]),"-mzint4DB.csv", sep='')
    cat("PeakNO,rt,rt_min,mz,intensity\n", file=ofile, append=FALSE)
    for (i in 1:length(peak_mzInt_list))  {
      peakrt <- as.character(peak_mzInt_list[[i]]$rt)
      # peakri <- as.character(asample.peakInfo[i,"RI"])
      peakmz <- paste(noquote(round(peak_mzInt_list[[i]]$mzInt[,"mz"],2)), collapse=" ")
      peakmzint <- paste(peak_mzInt_list[[i]]$mzInt[,"intensity"], collapse=" ")
      
      oinfo <- paste(c(i, peakrt, round(as.numeric(peakrt)/60,2), peakmz, peakmzint), collapse=",")
      cat(oinfo, "\n", file=ofile, append=TRUE)
    }    
  }
  
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

##
## estimating missing alkane RTs with nls algorithm 
##
estimateMissingAlkaneRT <- function(peak.list)
{
  # make estimated RT of Alkane
   predAlkaneRT <- function(d, maxCn, lastCn)
   {
      if( FALSE ) {
        ALKRT <- c(13.07, 16.27, 19.28, 22.0, 24.53, 26.88, 29.05, 31.1, 33.09, 34.89, 36.64)
        Cn <- c(10,11,12,13,14,15,16,17,18,19,20)
        # fit <- nls(ALKRT ~ a*exp(b*Cn), start=c(a=1, b=0.001))  
        # fit <- nls(ALKRT ~ a+b*log(Cn), start=c(a=1, b=0.001))  
      }
      fit <- nls(ALKRT ~ a+b*log(Cn), data=d, start=c(a=1, b=0.01)) 
      
      Cn.newlist <- c( (maxCn+1):lastCn)      
      predALK <- round(predict(fit, newdata=list(Cn=Cn.newlist)),2)
      # print(predALK)
      zeroIntensity <- rep(0, length(Cn.newlist))      
      return(as.data.frame(cbind(ALKRT=predALK, Cn=Cn.newlist, Intensity=zeroIntensity)) )
  }

  if(max(peak.list[,'Cn']) < 36) {
    if(DEBUG) {cat("## Estimated Higher Alkane Cn's RT (>= 36) \n")}
    alkane.ext <- predAlkaneRT(as.data.frame(peak.list), max(peak.list[,"Cn"]), 36)
    peak.list.ext <- rbind(peak.list[,c("ALKRT", "Cn", "Intensity")], alkane.ext)      
    if(DEBUG) { cat("alkane.extended\n"); print(peak.list.ext) }
    
    peak.list <- peak.list.ext
  }
  
  if (min(peak.list[,'Cn']) > 10) {
    if(DEBUG) {cat("## Estimated Lower Alkane Cn's RT (<= 10) \n")}

    alkane.ext <- predAlkaneRT(as.data.frame(peak.list), 8, min(peak.list[,'Cn'])-1)
    peak.list.ext <- rbind(alkane.ext, peak.list[,c("ALKRT", "Cn", "Intensity")])      
    if( DEBUG ) {  cat("alkane.extended\n"); print(peak.list.ext) }
    peak.list <- peak.list.ext
  }
  
  return(peak.list)
}


#================================================================================
# Alkane peak profiling MAIN ()
#================================================================================
# do_AlkanePeakProfile <- function(lib.fname.alkane, sample.fname.alkane, setAdjustAlkanePeakCn=FALSE, userDefined.Cn=FALSE, userEstAlkaneRT=FALSE) 
do_AlkanePeakProfile <- function(lib.fname.alkane, sample.fname.alkane, setAdjustAlkanePeakCn=FALSE, userDefined.Cn=FALSE) 
{
  
  ## load Compound Name RI, MZ and Intensity library 
  # lib.peak.alkane <- read.csv(file=file.path(lib_dir, LibFile.Alkane), head=TRUE, sep=",", quote = "\"");
  lib.peak.alkane <- read.csv(file=lib.fname.alkane, head=TRUE, sep=",", quote = "\"");
  # names(lib.peak.alkane)
  if(nrow(lib.peak.alkane) == 0) stop("Cannot load the m/z & intensity library for Alkane")
  
  cat("## Extracting peak list for alkane standard ...\n")    
  xset.alkane <- extractAlkaneStdInfo(sample.fname.alkane)
  
  # peaks.alkane <- extract_peak_list_alkane2(xset.alkane, numOfAlkane, show.plot, ctype="TIC", offset=7)
  # if (SampleType == ORGANIC_ACID) {   
  #    offset <- 7
  #} else {   
  #    offset <- 5 
  #}
  
  offset <- 2.01
  # peaks.alkane <- extract_peak_list_alkane2(xset.alkane, numOfAlkane, ctype="TIC", offset, RunPlotOnly)
  peaks.alkane <- extract_peak_list_alkane2(xset.alkane, numOfAlkane, ctype="TIC", offset)
  if (DEBUG) { cat("## peaks.alkane:\n"); print(peaks.alkane) }
  
  ## cleanning false peaks before profiling using RT
  peaks.alkane <- cleanning_alkaneFaslePeaks(peaks.alkane, RT.offset=50)  # offset: 20~60
  if (DEBUG) { cat("## screened peaks.alkane:\n"); print(peaks.alkane) }
  
  # profiling with m/z & intensity
  # option: print_mzInt (TRUE/FALSE) for library/DB update
  profiled_peaks_alkane <- peakIdentify.alkane2(peaks.alkane, xset.alkane, lib.peak.alkane, print_mzInt=FALSE)
  if (DEBUG) { cat("## profiled_peaks_alkane:\n"); print(profiled_peaks_alkane) }
  
  final_PeakProfile_alkane <- arrangeProfiledPeaks2.alkane(profiled_peaks_alkane, SampleType)
  if (DEBUG) { cat("## final_peakProfile_alkane:\n"); print(final_PeakProfile_alkane) }
  
  # this is for the serum on Old GC
  # if( FALSE ) {
  #  intensityThreshold <- max(as.numeric(as.character(final_PeakProfile_alkane$Intensity))) / 10
  #  final_PeakProfile_alkane <- final_PeakProfile_alkane [which(as.numeric(as.character(final_PeakProfile_alkane$Intensity)) > intensityThreshold), ]
  #  if (DEBUG) { cat("## final_peakProfile_alkane after filtering with >", intensityThreshold, ":\n"); print(final_PeakProfile_alkane) }
  #}
  
  alkane.peaks.unique <- getUniqueAlkanePeakList(final_PeakProfile_alkane, offset.RT=50)
  alkane.peaks.cleaned <- cleaningFalseAlkanePeaks(alkane.peaks.unique, offset.RT=50) 
  
  alkane.peaks.profiled <- cbind(ALKRT=as.double(as.character(alkane.peaks.cleaned$RT))
                                 , Cn=as.integer(as.character(alkane.peaks.cleaned$Cn))
                                 , Intensity=as.integer(as.character(alkane.peaks.cleaned$Intensity)))
  
  if (DEBUG) { cat("## alkane peaks:\n"); print(round(alkane.peaks.profiled,2)) }
  
  if (setAdjustAlkanePeakCn) {
    alkane.peaks.profiled <- adjustAlkanePeakCn(alkane.peaks.profiled, Cn.topIntensity=20)
  }

  if( ! is.null(user.AlkaneRT) ) {
    alkane.peaks.profiled <- userAssignedAlkanePeakCn(alkane.peaks.profiled, user.AlkaneRT)
  }
  
  alkaneInfo <- check_alkane_std(alkane.peaks.profiled)
  
  if (DEBUG) { cat("## alkane peaks (for the JSON file create):\n"); print(round(alkane.peaks.profiled,2)) }
  
  if (CREATE_JSON_FILE) {       
    ofilename <- paste(sub(".mzXML|.CDF","", basename(sample.fname.alkane), ignore.case = TRUE),"_spectrum.json", sep='')
    # create_json_file.alkane(ofilename, xset.alkane@scantime, xset.alkane@tic,
    #                  alkane.peaks.profiled[,"ALKRT"], alkane.peaks.profiled[,"Intensity"], paste('C',alkane.peaks.profiled[,"Cn"],sep='') )
    create_json_file.alkane(ofilename, xset.alkane@scantime, xset.alkane@tic, alkane.peaks.profiled )
  }
  
  ## Estimated missing alkane
  userEstAlkaneRT<-TRUE
  if( userEstAlkaneRT == TRUE ) {
    if(DEBUG) {cat("\n## Use Estimated missing Alkane\n") }
    alkane.peaks.profiled <- estimateMissingAlkaneRT(alkane.peaks.profiled)
  }
  
  if (DEBUG) { cat("alkane:\n"); print(cbind(alkane.peaks.profiled[,c(2,1)], RTmin=round(alkane.peaks.profiled[,1]/60, 2))) }
  
  ofilename <- paste(sub(".mzXML|.CDF","", basename(sample.fname.alkane), ignore.case = TRUE),"_alkanePeakList.csv", sep='')
  write.csv(alkane.peaks.profiled, file=ofilename, quote=TRUE, row.names=FALSE)
  
  return(alkane.peaks.profiled)    
}
# end of the do_AlkanePeakProfile() main

