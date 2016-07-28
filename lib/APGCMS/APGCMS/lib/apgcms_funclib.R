##########################################################################################
## Library for automatic GC-MS profiling
## update: August 8, 2014
############################################################################################

## Loading Packages
## Install packages:
##    source("http://bioconductor.org/biocLite.R")
##    biocLite(module)
##    biocLite() # update related packages
##    biocLite("xcms")
##    sessionInfo() 

## library(xcms)
# suppressMessages(require(xcms));
# options(warn=-1)

# Generate Spectrum Plots
# fname.list <- fileList$sampleFiles
# isPlotOnly=TRUE
# generateSpectrumPlot <- function(fname.list, isPlotOnly=TRUE) {      
generateSpectrumPlot <- function(fname.list) {      
  if(DEBUG) cat("# Generate Spectrum Plots\n")
  
  for (i in 1:length(fname.list)) {   ## running per each sample         
    # i<-1
    f.sample <- fname.list[i]
    f.sample.basename <- basename(f.sample)
    cat(paste("\n", i, ") Profiling Processing file :", sep=''), f.sample.basename, "\n")
    
    if(DEBUG) cat("## Extracting Spectrum Information \n")
    xset.asample <- extractSampleInfo2(f.sample)
    
    ## peak picking for samples using TIC or EIC (peak's RT & Intensity)
    if(DEBUG) cat("## Extracting peak list \n")
    # peaks.sample <- extract_peak_list_samples2(xset.asample, ctype="TIC", offset=1.5, isPlotOnly)
    peaks.sample <- extract_peak_list_samples2(xset.asample, ctype="TIC", offset=1.5, plotFile=TRUE)
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
   # peaks.sample <- extract_peak_list_samples2(xset.asample, ctype="TIC", offset=1.5, opt.plotonly)
   peaks.sample <- extract_peak_list_samples2(xset.asample, ctype="TIC", offset=1.5, plotFile=opt.plotonly)
}
 
### 
###  Updated May20, 2015
###
# generateSpectrumPlot.multicore <- function(nCluster, fnames, isPlotOnly=TRUE) {      
generateSpectrumPlot.multicore <- function(nCluster, fnames) {
   if(DEBUG) cat("# Generate Spectrum Plots with Multiple Core\n")
   #print(fname.list)
   
   # Calculate the number of cores
   ncl <- makeCluster(nCluster)  
   
   clusterExport(ncl, list("extractFunc", "extractSampleInfo2", "xcmsRaw", "extract_peak_list_samples2",
                           "getEIC", "plotEIC","extract_RT_forEachPeak", "DEBUG","get_RTofeachPeak"))
   
   clusterApply(ncl, fnames, extractFunc, opt.plotonly=TRUE)
   # parLapply(ncl, fname.list, extractFunc, opt.plotonly=isPlotOnly)
   stopCluster(ncl)
   
   # To check the process is done or not
   cat("Done -", length(fnames),"Samples's Image Files were Generation\n", file="Done_ImageFiles.txt", append=FALSE)      
}

# to give more kind/understandable string in concentration
replaceShort2LongString <- function(tbl, isJson=FALSE)
{
  
    tbl$Concentration2 <- as.character(unlist(tbl$Concentration2)) 
    
    if (nrow(tbl[which(tbl$Concentration2 == "<LOD"),]) > 0) {
        if(isJson==TRUE) {
            tbl[which(tbl$Concentration2 == "<LOD"), "Concentration2"] <- "&ltLOD (Limit Of Detection)"
        } else {
            tbl[which(tbl$Concentration2 == "<LOD"), "Concentration2"] <- "<LOD (Limit Of Detection)"
        }
    }
    
    if (nrow(tbl[which(tbl$Concentration2 == "MP"),]) > 0) {
        tbl[which(tbl$Concentration2 == "MP"), "Concentration2"] <- "Multiple peaks (combined with the biggest one)"
    }
    if (nrow(tbl[which(tbl$Concentration2 == "ISTD"),]) > 0) {
        tbl[which(tbl$Concentration2 == "ISTD"), "Concentration2"] <- "Internal Standard"
    }

    return (tbl)
}

### 
###  Updated May21, 2015
###
## for uning multiple cores 
## print.on : multicore does not support this cat/print because it doesn't make sense 
quantificationFunc <- function(f.sample, print.on=FALSE, use.blank, threshold.matchFactor, internalStd, lib.calicurv, cmpdlist, final_PeakProfile_blank, PRINT_MZINT4DB=FALSE) 
{
      # RunPlotOnly <- FALSE
      # PRINT_MZINT4DB <- FALSE 
      f.sample.basename <- basename(f.sample)
      if(print.on) {
          # cat(paste("\n", i, ") Compound Profiling and Quantifying:", sep=''), f.sample.basename, "\n")
          cat("\n Compound Profiling and Quantifying:", f.sample.basename, "\n")
      }
      
      ## should be updated (remove peakFind function; data structure because it doesnot use anymore)  
      if( print.on ) { cat("\t >> Extracting Spectrum Information \n") }
      xset.asample <- extractSampleInfo2(f.sample)
      if(DEBUG) { cat("\n## xset.asample:\n"); print(xset.asample) }

      ## peak picking for samples using TIC or EIC (peak's RT & Intensity)
      if( print.on & DEBUG ) {  cat("\t >> Extracting peak list \n")  }
      # peaks.sample <- extract_peak_list_samples2(xset.asample, ctype="TIC", offset=1.5, RunPlotOnly)
      peaks.sample <- extract_peak_list_samples2(xset.asample, ctype="TIC", offset=1.5, plotFile=FALSE)

      ## to check missing peak
      if (FALSE & DEBUG) {
          ofilename <- paste(sub(".mzXML|.CDF","", f.sample.basename, ignore.case = TRUE),"_extractedPeaks_temp.csv", sep='')
          write.csv(peaks.sample/60, file=ofilename, quote=FALSE)
      }
      
      ## RI calculation using Alkane Std
      if ( print.on ) { cat("\t >> Get RI for each peak \n") }
      peak_samples_ri <- get_RI_for_samples2(peaks.sample, peak_alkane_std)
      if (FALSE & DEBUG) {
          ofilename <- paste(sub(".mzXML|.CDF","", f.sample.basename, ignore.case = TRUE),"_peak_sample_RI.csv", sep='')
          write.csv(peak_samples_ri, file=ofilename, quote=FALSE)
      }
      
      ##########################################################################    
      ## NOT USED for ACTUAL PROCESS
      ## For internal Works including library and calibration curves    
      ##########################################################################    
     {
          if( FALSE & DEBUG ) {
            ## for using to easy to find a peak
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
    
    # If a spectrum has problem, return null
    if ( length(unique(xset.asample$xraw@scanindex)) < length(xset.asample$xraw@scanindex) * 0.9  ) { 
        # cat("length(xset.alkane@scanindex):\n"); print(length(xset.asample$xraw@scanindex));
        # cat("length(unique(xset.alkane@scanindex)):\n"); print(length(unique(xset.asample$xraw@scanindex)));
        amsg <- paste("WARNING: Unable to detect peaks in the spectrum.\n\t Please verify the Spectrum of", 
                      f.sample.basename)
        showErrMessage(amsg)
        cat(file=File.ErrorLog, amsg, append=TRUE)
      
        return (NULL)
    }
    
    ## Compound Identification using RI and Mass Spectrum 
    # profiled_peaks <- compoundIdentify3(peak_samples_ri, xset.asample, lib.peak, alkaneInfo, RI.Variation, isBLANK=FALSE, print_mzInt=PRINT_MZINT4DB)
    profiled_peaks <- compoundIdentify4(peak_samples_ri, xset.asample, lib.peak, alkaneInfo, RI.Variation, isBLANK=FALSE, print_mzInt=PRINT_MZINT4DB)
    if (DEBUG) cat("\n\n## DONE compound Identify 4 for sample:", f.sample.basename,"\n\n")

    if(nrow(profiled_peaks) == 0) {
          profiled_peaks <- "\n\n## Error: There is no matched peak with library\n Please check spectrum data file\n"
          cat(file=File.ErrorLog, profiled_peaks, append=TRUE)
          ofilename <- paste(sub(".mzXML|.CDF","", basename(f.sample), ignore.case = TRUE),"_profiledPeaksTMP.csv", sep='')
          write.csv(profiled_peaks, file=ofilename, quote=TRUE)
          
          # tmp.Concentration <- data.frame(HMDB_ID=NULL,Compound=NULL,Concentration2=NULL)]
          tmp.Concentration <- NULL
          return (tmp.Concentration)
    } 
    
    if (FALSE & DEBUG) {
        ofilename <- paste(sub(".mzXML|.CDF","", basename(f.sample), ignore.case = TRUE),"_profiledPeaksTMP.csv", sep='')
        write.csv(profiled_peaks, file=ofilename, quote=TRUE)
    }
    
    ## -- select only the highest (T)score peaks
    if (print.on & DEBUG) { cat("## arranging profiled peaks \n")  }
    # final_PeakProfile <- arrangeProfiledPeaks2(profiled_peaks, SampleType)
    final_PeakProfile <- arrangeProfiledPeaks2(profiled_peaks)
    if (print.on & DEBUG) { cat("final_PeakProfile:\n"); print(final_PeakProfile[,c(1:21)]) }
    
    # Subtract BLANK Peak Areas
    # if there is a blank sample, then substract the area from sample's area
    if( use.blank &  ! is.null(final_PeakProfile_blank) ) { 
      if( print.on & DEBUG) {  cat("\n\n## subtract blank peaks' area \n") }
      # final_PeakProfile <- cbind(final_PeakProfile, Area.Blank=0)
      # exclude Standard Peak

      final_PeakProfile <- as.data.frame(final_PeakProfile)
      final_PeakProfile_blank <- as.data.frame(final_PeakProfile_blank)
      final_PeakProfile$Area.EICTarget <- as.double(as.character(final_PeakProfile$Area.EICTarget))

      # for trace Blank substraction
      Area.EICTarget.origin <- NA
      Area.EICTarget.blank <- NA
      final_PeakProfile <- cbind(final_PeakProfile, Area.EICTarget.origin, Area.EICTarget.blank)

      ## excluding Internal Standard in Subtract target ION's area
      final_PeakProfile_blank <- final_PeakProfile_blank[- grep("ISTD", final_PeakProfile_blank$CompoundWithTMS), ]
      final_PeakProfile_blank$Area.EICTarget <- as.double(as.character(final_PeakProfile_blank$Area.EICTarget))

      for(p.idx in 1:nrow(final_PeakProfile_blank)) {
          cmpdname.tmp <- final_PeakProfile_blank$CompoundWithTMS[p.idx]
          sp.idx <- which(as.character(final_PeakProfile$CompoundWithTMS) == cmpdname.tmp)
          
          if ( length(sp.idx) > 0 ) {
            if(print.on & DEBUG) {
              cat("# Subtract Blank Sample's Peak Area - sp.idx:", sp.idx, "\n")
              cat("\t cmpdname:", cmpdname.tmp, "\t sample:", final_PeakProfile[sp.idx, 'Area.EICTarget'], "-", "blank:", final_PeakProfile_blank[p.idx, "Area.EICTarget"], "\n")
            } 
            
            # final_PeakProfile[sp.idx, 'Area.Blank'] <- final_PeakProfile_blank[p.idx, 'Area']
            final_PeakProfile[sp.idx, 'Area.EICTarget.origin'] <- final_PeakProfile[sp.idx, 'Area.EICTarget']
            final_PeakProfile[sp.idx, 'Area.EICTarget.blank'] <- final_PeakProfile_blank[p.idx, 'Area.EICTarget']
            final_PeakProfile[sp.idx, 'Area.EICTarget'] <- final_PeakProfile[sp.idx, 'Area.EICTarget'] - final_PeakProfile_blank[p.idx, 'Area.EICTarget']
            if ( final_PeakProfile[sp.idx, 'Area.EICTarget'] < 0 ) {
                final_PeakProfile[sp.idx, 'Area.EICTarget']  <- 0
            }
            if(print.on & DEBUG) {
                cat("\t subtracted sample area:", final_PeakProfile[sp.idx, 'Area.EICTarget'],"\n")
            } 
        } 
      }
    }
    
    if (print.on & DEBUG) { 
      cat("\n\n## final_peakProfile after blank subtraction:\n"); 
      print(final_PeakProfile)
      print(names(final_PeakProfile))
    }
    
    if (DEBUG) { cat("\n\n## screeningWithMatchFactor with Match Factor Threshold (", threshold.matchFactor, ")\n") }
    final_PeakProfile.screened <- screeningWithMatchFactor(final_PeakProfile, threshold.matchFactor)
    if (DEBUG) {
        cat("# final_PeakProfile.screened\n"); print(final_PeakProfile.screened)
    } 
    
    ## Quantification        
    checkInternalStd <- existInternalStd(internalStd, final_PeakProfile, lib.peak)
    
    if ( internalStd != 'NONE' & checkInternalStd == TRUE ) {
        
        if (print.on) { cat("\n\n###  Quantifying identified peaks\n\n") }
        quantifiedResult <- quantification(final_PeakProfile.screened, lib.peak, lib.calicurv, internalStd, f.sample, stype=SampleType) 
        
        if (print.on & DEBUG) {
            cat("\n\n## quantified Result:\n")
            print(quantifiedResult)
            # hmdbID, Area, Concentration 
        }
        
        if (print.on) { cat("\n\n## Generating FinalReport (with concentration)\n\n") }
        finalReport <- genFinalReport(final_PeakProfile.screened, quantifiedResult) 
        colnames(finalReport)[1] <- "HMDB_ID"
        
        # (matchMZrate > 60.0)
        if(DEBUG) { ## save quantified results
            ofilename <- paste(sub(".mzXML|.CDF","", f.sample.basename, ignore.case = TRUE),"_quantifiedResult.csv", sep='')
            write.table(finalReport, file=ofilename, quote=TRUE, row.names=FALSE, sep=",")
        }
        
        # Concentration2 <- check.Concentration(finalReport$Concentration)  ## changed July2516
        Concentration2 <- finalReport$Concentration ## temporary
        finalReport <- cbind(finalReport, Concentration2) 
        if (print.on & DEBUG) { cat("# finalReport:\n"); print(finalReport); }
        
        finalReport.All <- merge(cmpdlist, finalReport, by=c('HMDB_ID','CompoundWithTMS'), all.x=TRUE)
        finalReport.All <- finalReport.All[order(finalReport.All$SeqIndex), ]
        rownames(finalReport.All) <- c(1:nrow(finalReport.All))
        
        finalReport.All$Concentration2 <- as.character(finalReport.All$Concentration2)
        finalReport.All$Concentration2[is.na(finalReport.All$Concentration2)] <- "<LOD" # not detected
        
        finalReport.json <- finalReport.All # for the JSON file generation

        ## saving final profile/quantification data into a file
        finalReport.All <- finalReport.All[,c("SeqIndex","HMDB_ID","Compound","CompoundWithTMS","RT_min","RT","RI","Intensity",
                                              "TargetIon","QIon","MatchFactor","RI.Similarity","Corr.Spearman","matchMZrate",
                                              "Area.EICTarget","Area.EICQualification","AreaRatio", "Concentration2")]

        # change short string to long string: eg. <LOD -> <LOD (Limit Of Detection)
        finalReport.All <- replaceShort2LongString(finalReport.All) 
        
        outColnames <- c("HMDB_ID", "Compound", "CompoundWithTMS", "RT_min","RT","RI","Intensity",
                         "TargetIon","QIon","MatchFactor","RI.Similarity","Corr.Spearman","matchMZrate",
                         "Area.EICTarget","Area.EICQualification","AreaRatio", "Concentration")
        ofilename <- paste(sub(".mzXML|.CDF","", f.sample.basename, ignore.case = TRUE),"_profiled.csv", sep='')
        write.table(finalReport.All[which(substr(finalReport.All$Concentration,1,4) !="<LOD"),-1], file=ofilename, 
                    quote=TRUE, row.names=FALSE, col.names=outColnames, sep=",")
        
        # check blank substraction
        if(FALSE & DEBUG) {
            ofname.tmp <- paste(sub(".mzXML|.CDF","", f.sample.basename, ignore.case = TRUE),"_profiled_allFields.csv", sep='')
            write.table(finalReport.All[,-1], file=ofname.tmp, quote=TRUE, row.names=FALSE, sep=",")
        }
        
        ###################################################################
        ## for making calibration curve, generting temporary summary files
        ###################################################################
        if (DEBUG) {
            sampleID <- sub(".mzXML|.CDF","", f.sample.basename, ignore.case = TRUE)
            # ofilename <- paste("profileddata_for_calibration_", substr(sampleID,1,4),".csv", sep='')
            ofilename <- "profileddata_for_calibration.csv"
            # dt4calibcurve <- finalReport.All[!which(is.na(finalReport.All$Area.EICTarget)), c("HMDB_ID","CompoundWithTMS","RT_min","RT","RI",
            dt4calibcurve <- finalReport[, c("HMDB_ID","CompoundWithTMS","RT_min","RT","RI","Intensity",
                                             "TargetIon","TargetIon.intensity","QIon","MatchFactor","RI.Similarity","Corr.Spearman","matchMZrate",
                                                  "Area.EICTarget","Area.EICQualification","AreaRatio")]
            
            # HMDB ID
            istd.cmpds <- dt4calibcurve[grep("ISTD", dt4calibcurve$CompoundWithTMS), "HMDB_ID"]
            
            if(length(istd.cmpds) == 2) {
                if (DEBUG) cat("\n## Two Internal Standards were found\n")
                areaCholesterol <- dt4calibcurve[which(dt4calibcurve$HMDB_ID == "HMDB00067"), "Area.EICTarget"]
                areaSuccinicD4 <- dt4calibcurve[which(dt4calibcurve$HMDB_ID == "HMDB_ISTD1"), "Area.EICTarget"]
                ratioWithCholesterol <- dt4calibcurve$Area.EICTarget / areaCholesterol
                ratioWithSuccinicD4 <- dt4calibcurve$Area.EICTarget / areaSuccinicD4
                
                dt4calibcurve <- cbind(sampleID, dt4calibcurve, ratioWithCholesterol, ratioWithSuccinicD4)
                
                if(DEBUG) {  
                    cat("# sampleID\n"); print(sampleID)
                    cat("# dt4calibcurve\n"); print(dt4calibcurve[,c(1:2)])
                    cat("# ratioWithCholesterol\n"); print(ratioWithCholesterol)
                    cat("# ratioWithSuccinicD4\n"); print(ratioWithSuccinicD4)
                    
                    cat("## sample:", f.sample.basename, "\n")
                    cat("\n\n## areaCholesterol:", areaCholesterol, "\t areaSuccinicD4:", areaSuccinicD4, "\n")
                }
            } else {
                # one ISTD - Succinic Acid -D4
                if (DEBUG) { 
                    cat("\n## Single Internal Standard was found \n")
                    print(istd.cmpds)
                }
                if(!TRUE)  {
                    # succinic-D4
                    areaCholesterol <- 0
                    areaSuccinicD4 <- dt4calibcurve[which(dt4calibcurve$HMDB_ID == "HMDB_ISTD1"), "Area.EICTarget"]
                    ratioWithCholesterol <- 0
                    ratioWithSuccinicD4 <- dt4calibcurve$Area.EICTarget / areaSuccinicD4
                    
                    dt4calibcurve <- cbind(sampleID, dt4calibcurve, ratioWithCholesterol, ratioWithSuccinicD4)
                    if(DEBUG) {
                        cat("## sample:", f.sample.basename, "\n")
                        cat("\n\n## areaCholesterol:", areaCholesterol, "\t areaSuccinicD4:", areaSuccinicD4, "\n")
                    }
                }
              
                if(TRUE)  {
                  # others - Cholestreol; Ribitol; etc alone
                  # print(dt4calibcurve)
                  areaISTDCompound <- dt4calibcurve[which(dt4calibcurve$HMDB_ID == istd.cmpds), "Area.EICTarget"]
                  ratioWithISTD <- dt4calibcurve$Area.EICTarget / areaISTDCompound
                  
                  dt4calibcurve <- cbind(sampleID, dt4calibcurve, ratioWithISTD)
                  if(DEBUG) {
                      cat("## sample:", f.sample.basename, "\n")
                      cat("\n\n## areaISTD:", areaISTDCompound, "\n")
                  }
                }
            }                
            write.table(dt4calibcurve, file=ofilename, quote=TRUE, row.names=FALSE, sep=",", append=TRUE)
        }
        ###########################################################################
        ## End: for making calibration curve, generting temporary summary files
        ###########################################################################
        
        
        if (print.on & DEBUG) { cat("# finalReport All:\n"); print(finalReport.All); }
        
        ## making JSON file for Profiled Peak View
        finalReport.json <- finalReport.json[-which(is.na(finalReport.json$Concentration)), ]  ## excluding all NA (<LOD); non detected records
        finalReport.json <- replaceShort2LongString(finalReport.json, isJson=TRUE) 
        if (DEBUG) { 
            cat("## finalReport.json 2\n"); print(names(finalReport.json));
        }

        finalReport.json <- finalReport.json[which( (!is.na(finalReport.json$Concentration2)) ), 
                                             c("HMDB_ID","CompoundWithTMS","RT_min","RI","Intensity","MatchFactor",
                                               "TScore","RI.Similarity","Corr.Spearman","TargetIon","QIon",
                                               "Area.EICTarget","Area.EICQualification","AreaRatio","mz","mzInt","Concentration2")]
        
        
        ofilename <- paste(sub(".mzXML|.CDF","", basename(f.sample), ignore.case = TRUE),"_spectrum.json", sep='')
        create_json_file(ofilename, round(xset.asample$xraw@scantime/60, 3), xset.asample$xraw@tic, finalReport.json)
        
        ## concentration summary table
        tmp.Concentration <- finalReport.All[, c("HMDB_ID","Compound","Concentration2")]
        na.length <- length(which(is.na(tmp.Concentration$Concentration2)))
        if( na.length > 0 ) {
            tmp.Concentration <- tmp.Concentration[- which(is.na(tmp.Concentration$Concentration2)), ]
        }
        if( length(which(tmp.Concentration$Concentration2 == "Multiple peaks (merged with the first peak)")) > 0 ) {
            tmp.Concentration <- tmp.Concentration[- which(tmp.Concentration$Concentration2 == "Multiple peaks (merged with the first peak)"), ]
        }
        
        colnames(tmp.Concentration) <- c('HMDB_ID', 'Compound', basename(sub(".mzXML|.CDF", "", f.sample, ignore.case = TRUE)) )
  
    } else {
        ## No internal STD - Only Profile the Compound Names      
        # cat("\t >> Generating FinalReport for", basename(f.sample), "without Quantification \n\t\t because of no internal standard\n")
        cat("#########################################################################\n")
        cat("### Generating FinalReport: without Quantification (NO internal standard)\n\n")
        
        colnames(final_PeakProfile.screened)[1] <- "HMDB_ID"
        
        finalReport.All <- merge(cmpdlist, final_PeakProfile.screened, by = c('HMDB_ID','CompoundWithTMS'), sort=FALSE) 
        finalReport.All <- finalReport.All[order(as.numeric(as.character(finalReport.All$SeqIndex))), ]
        rownames(finalReport.All) <- c(1:nrow(finalReport.All))

        Concentration2 <- NA
        finalReport.All <- cbind(finalReport.All, Concentration2)
        finalReport.json <- finalReport.All # for the JSON file generation
        
        ofilename <- paste(sub(".mzXML|.CDF","", f.sample.basename, ignore.case = TRUE),"_profiled.csv", sep='')
        finalReport.All <- finalReport.All[,c("SeqIndex","HMDB_ID","Compound","CompoundWithTMS","RT_min","RT","RI","Intensity",
                                              "TargetIon","QIon","MatchFactor","RI.Similarity","Corr.Spearman","matchMZrate",
                                              "Area.EICTarget","Area.EICQualification","AreaRatio", "Concentration2")]
        
        outColnames <- c("HMDB_ID", "Compound", "CompoundWithTMS", "RT_min","RT","RI","Intensity",
                         "TargetIon","QIon","MatchFactor","RI.Similarity","Corr.Spearman","matchMZrate",
                         "Area.EICTarget","Area.EICQualification","AreaRatio", "Concentration")
        write.table(finalReport.All[, -1], file=ofilename, quote=TRUE, row.names=FALSE, col.names=outColnames, sep=",")
        
        # making JSON file for Profiled Peak View
        finalReport.json <- finalReport.json[, c("HMDB_ID","CompoundWithTMS","RT_min","RI","Intensity","MatchFactor",
                                               "TScore","RI.Similarity","Corr.Spearman","TargetIon","QIon",
                                               "Area.EICTarget","Area.EICQualification","AreaRatio","mz","mzInt","Concentration2")]
        cat("### finalReport.json: ###\n"); print(head(finalReport.json))
                
        ## No internal STD - Only Profile the Compound Names      
        ofilename <- paste(sub(".mzXML|.CDF","", basename(f.sample), ignore.case = TRUE),"_spectrum.json", sep='')
        create_json_file(ofilename, round(xset.asample$xraw@scantime/60, 3), xset.asample$xraw@tic, finalReport.json )

        # if some of sample does not have Internal Standard, all the concentration will be null
        # this is only for combined results
        ## concentration summary table
        tmp.Concentration <- finalReport.All[, c("HMDB_ID","Compound","Concentration2")]
        # na.length <- length(which(is.na(tmp.Concentration$Concentration2)))
        if(FALSE) {
            if ( na.length > 0 ) {
              # cat("length(NA):", na.length, "\n")
              tmp.Concentration <- tmp.Concentration[- which(is.na(tmp.Concentration$Concentration2)), ]
            }
            if ( length(which(tmp.Concentration$Concentration2 == "Multiple peaks (merged with the first peak)")) > 0 ) {
              tmp.Concentration <- tmp.Concentration[- which(tmp.Concentration$Concentration2 == "Multiple peaks (merged with the first peak)"), ]
            }
        }
        colnames(tmp.Concentration) <- c('HMDB_ID', 'Compound', basename(sub(".mzXML|.CDF", "", f.sample, ignore.case = TRUE)) )        
        
    } # end of else 

    return (tmp.Concentration)
}

mergeConcTable <- function( conc.list )
{
      # excluding NA cases including Multiple Peaks
      for (i in 1:length(conc.list)) {
          dt <- conc.list[[i]]
          # cat(i,":", which(dt[,3] == "Internal Standard" | !is.na(as.numeric(dt[,3]))), "\n")
          conc.list[[i]] <- dt[which(dt[,3] == "Internal Standard" | !is.na(as.numeric(dt[,3]))), ]
          # cat(i,":", nrow(conc.list[[i]]), "\n")
          # print(conc.list[[i]])
      }
  
      # column names: HMDB_ID, Compound, sample_ID
      if( is.null(conc.list[[1]])) {
          amsg <- "WARNING: No identification and quantification because of problem in the spectrum.\n\t Please verify the Spectrum !!!"
          showErrMessage(amsg)
          cat(file=File.ErrorLog, amsg, append=TRUE)
      } 
      conc.all <- conc.list[[1]]
        
      for (i in 2:length(conc.list))  {
          if( !is.null(conc.all) & !is.null(conc.list[[i]]) ) {
              conc.all <- merge(conc.all, conc.list[[i]], by=c("HMDB_ID","Compound"), sort=FALSE, all=TRUE)
          } else if ( is.null(conc.all) & !is.null(conc.list[[i]]) ) {
              conc.all <- conc.list[[i]]
          } else {
              amsg <- "WARNING: No identification and quantification because of problem in the spectrum.\n\t Please verify the Spectrum !!!"
              showErrMessage(amsg)
              cat(file=File.ErrorLog, amsg, append=TRUE)              
          }
      }
      return (conc.all)
}

quantification.multicore <- function(nCluster, fnames, use.blank, threshold.matchFactor, internalStd, lib.calicurv, cmpdlist, final_PeakProfile_blank) {      
  if(DEBUG) {
      cat("\n# Quantifying spectrum peaks with Multiple Core (nCores:", nCluster, ")\n")
      #print(fname.list)
  }
  
  # Calculate the number of cores
  ncl <- makeCluster(nCluster)  
  
  clusterExport(ncl, list("DEBUG", "extractSampleInfo2", "xcmsRaw","extract_peak_list_samples2", "getEIC","extract_RT_forEachPeak", "get_RTofeachPeak",
                          "get_RI_for_samples2","calc_RI","peak_alkane_std", "compoundIdentify3", "getMZIntensityofEachPeak2", "getPeakRange2","getPeakArea3",
                          "getScan","getPeakArea2","RI.Variation","lib.peak","find_similar_peaks","calcMFscore","arrangeProfiledPeaks2",
                          "create_json_file","spectrumToJSON.fullSpectrum","spectrumToJSON.profile","existInternalStd","screeningWithMatchFactor",
                          "quantification","getSamePeakArea", "calibration","calcConcentration","genFinalReport","check.Concentration","alkaneInfo", "plotEIC"))
  # "xset.blank"
  
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
extract_RT_forEachPeak <- function(rt_int_matrix, offset=7, ctype="TIC", xset=NULL) {

      if( ctype == "TIC") {
          if(is.null(xset)) {
              stopMessage("TIC type analysis is required raw dataset (xset)")
          }
          # rtint.o <- rt_int_matrix
          rt_int_matrix <- data.frame(rt=xset@scantime, intensity=xset@tic)
          # cat("## rt_int_matrix:\n"); print(head(rt_int_matrix))
      } else {
          stopMessage("## ctype is EIC - process will stop\n"); 
      }

      ## new algorithm to find significant peak's RT & Intensity (Jan 21, 2016)
      ## using 2nd derivative
      if (TRUE) {
          spd <- rt_int_matrix # rt & intensity
          spd.gd2 <- diff(spd$intensity, differences=2, lag=3);
          spd.gd5 <- diff(spd$intensity, differences=2, lag=5);
  
          rt.len <- length(xset@scantime) 
          # cat("## rt.len:", rt.len,"\n");
          # cat("## len(spd.gd5):", length(spd.gd5), "\n");
          # cat("## length:", length( xset@scantime[-c(1:5,(rt.len-4):rt.len)]), "\n");

          rt_int_matrix <- data.frame(rt=xset@scantime[-c(1:5,(rt.len-4):rt.len)], intensity.gd5= -spd.gd5, intensity=xset@tic[-c(1:5,(rt.len-4):rt.len)])
          # ofilename <- paste("/Users/beomsoo/gcmsProfiling/gc-autofit/lib/APGCMS/test/rt_int_matrix.csv", sep='')
          # write.csv(rt_int_matrix, file=ofilename, quote=FALSE)
          
          # xrange <- c(1950:2100)
          # xrange <- c(0:4000)
          if (FALSE & DEBUG) {
              sampleFile <- sub(".mzXML|.CDF", "", basename(xset@filepath), ignore.case=TRUE)  
              png(filename = paste("Plot_ScanTimeIntensity_", sampleFile,".png", sep=''), width = PNG_WIDTH, height = PNG_HEIGHT*2, units = "px", pointsize = 10)
                  par(mfrow=c(2,1))
                  plot(rt_int_matrix$rt, rt_int_matrix$intensity.gd5, type="l", col='red', main="RT vs Intensity (2nd Derivative)")
                  plot(rt_int_matrix$rt, rt_int_matrix$intensity, type="l", col='navy', main="RT vs Intensity")
              dev.off()
          }
      }

      ## to reduce size, using cut off values is very important to remain significant peak      
      # dstat <- summary(rt_int_matrix[,"intensity"])      
      # using 2nd gap derivative (k=5) 
      dstat <- summary(rt_int_matrix[which(rt_int_matrix[,"intensity.gd5"] > 0), 'intensity.gd5'])
      
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
          # cutoff1 <- dstat[3] + offset * (dstat[5] - dstat[2]) # median        
          cutoff <- dstat[4]  # mean
      } else {
          # for sample and blank spectrum
          # cutoff <- dstat[3] # median
          cutoff <- dstat[3] + offset * (dstat[5] - dstat[2]) # median
      }
      
      idx <- which(rt_int_matrix[,"intensity.gd5"] > cutoff)
      if (! DEBUG) { 
          cat("\n\n# Intensity cutoff:", cutoff, "offset:", offset, "\n")
          cat("## rt.len:", rt.len, "after cutoff:", length(idx),"\n");
      }
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
      # ofilename <- paste("/Users/beomsoo/gcmsProfiling/gc-autofit/lib/APGCMS/test/peak_matrix.csv", sep='')
      # write.csv(peak_matrix, file=ofilename, quote=FALSE)
      
      if (FALSE & DEBUG) {
          png(filename = paste("Plot_Selected_gd5_", sampleFile,".png", sep=''), width = PNG_WIDTH, height = PNG_HEIGHT*2, units = "px", pointsize = 10)
          plot(peak_matrix[,c(1,2)], type="h") # rt with intensity.gd5
          dev.off()
      }

      # if (DEBUG) { cat("## peak_matrix:\n"); print(peak_matrix)  }
      
      # detect the RT of each peak
      # peak_true_false_vec <- get_peak_true_false_vec2(peak_matrix)  
      peak_matrix <- get_RTofeachPeak(peak_matrix, wsize=3) 

      if (DEBUG) { 
        # cat("## peak_matrix using gd5:\n"); print(peak_matrix)
        cat("## rt.len:", rt.len, "--> cutoff:", length(idx), "--> apex of peak:", nrow(peak_matrix),"\n");
      }

      if (FALSE & DEBUG) {
          png(filename = paste("Plot_RTofeachPeak_", sampleFile,".png", sep=''), width = PNG_WIDTH, height = PNG_HEIGHT*2, units = "px", pointsize = 10)
          plot(peak_matrix[,c("rt","intensity")], type="h")
          dev.off()
      }
      
      return (peak_matrix)
}


# get_peak_true_false_vec2 <- get_RTofeachPeak
get_RTofeachPeak <- function(peak_matrix, wsize=5) {
  
    ## step 1: detect peak's apex
    rt <- peak_matrix[,"rt"]
    intensity.gd5 <- peak_matrix[,"intensity.gd5"]
    intensity <- peak_matrix[,"intensity"]
      
    nRT <- length(rt)
    rtPeak <- NULL 
    intGD5Peak <- NULL
    intPeak <- NULL 
    intPrv <- intensity[1] 
    for (i in 1:nRT) { 
      intNext<- ifelse(i==nRT,intensity.gd5[nRT],intensity.gd5[i+1]) 
      if(intPrv < intensity.gd5[i] & intensity.gd5[i] > intNext) { #found local peak 
          rtPeak<- c(rtPeak,rt[i]) 
          intGD5Peak <- c(intGD5Peak,intensity.gd5[i]) 
          intPeak <- c(intPeak,intensity[i]) 
      } 
      intPrv<- intensity.gd5[i] 
    } 
    
    apexes <- data.frame(rt=rtPeak, intensity.gd5=intGD5Peak, intensity=intPeak)
    if(FALSE & DEBUG) { 
        cat("apexes:\n"); print(cbind(rt_min=round(apexes$rt/60,2), apexes) );  
    }
    # plot(apexes) 
    
    ##  step 2: select only apex within a peak using window size
    num <- length(rtPeak);
    rt.new <- NULL;
    intGD5.new <- NULL;
    int.new <- NULL;
    for(i in 1:num) {
      tmp <- apexes[which(apexes$rt > (apexes$rt[i] - wsize) & apexes$rt < (apexes$rt[i] + wsize)), ];
      if(nrow(tmp) > 1) {
        tmp.apex <- tmp[order(tmp$intensity.gd5, decreasing=TRUE),][1,]      
        if( !(tmp.apex$rt %in% rt.new) ) { 
          rt.new <- c(rt.new, tmp.apex$rt);
          intGD5.new <- c(intGD5.new, tmp.apex$intensity.gd5);
          int.new <- c(int.new, tmp.apex$intensity);
        }
      } else {
        rt.new <- c(rt.new, apexes$rt[i]);
        intGD5.new <- c(intGD5.new, apexes$intensity.gd5[i]);
        int.new <- c(int.new, apexes$intensity[i]);
      }
    }  
    
    return( data.frame(rt=rt.new, intensity.gd5=intGD5.new, intensity=int.new) );  
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
  xset <- list( xraw=xraw, peaks=peaks )
  
  # unused in new updates
  if( ! TRUE ) {
      xset.samples <- xcmsSet(aSampleFile, method='centWave',ppm=1,snthresh=1,peakwidth=c(1,10),scanrange=c(1,3800))
      peak.ranges <- findPeaks.centWave(xset.samples, ppm=25, peakwidth=c(1,10), snthresh=10, prefilter=c(3,100), fitgauss=FALSE)
      cat("\n\n## findPeaks:\n"); print(peak.ranges)
  }  
  
  return(xset)  
}


# xset <- xset.asample
# extract_peak_list_blank <- function(xset, ctype="EIC", offset=1.5, plotFile=TRUE)  {
extract_peak_list_blank <- function(xset, ctype="TIC", offset=1.5)  {
    # mzrange <- t(xset@mzrange)
    # rtrange <- t(range(xset@scantime))
    ## Generate multiple extracted ion chromatograms for m/z values of interest. 
    ## For xcmsSet objects, reread original raw data and apply precomputed 
    ## retention time correction, if applicable.
    # eic_obj <- getEIC(xset, mzrange=mzrange, rtrange=rtrange)
    # eic_obj <- getEIC(xset, mzrange=mzrange, rtrange=rtrange, sampleidx=i, rt="corrected")
    
    # plot EIC generation
    # if (plotFile) {
      sampleFile <- sub(".mzXML|.CDF", "", basename(xset@filepath[1]), ignore.case=TRUE)
      ## plot TIC instead of EIC (Jan 19, 2016) 
      # png(filename = paste("Plot_EIC_", sampleFile,".png", sep=''), width = PNG_WIDTH, height = PNG_HEIGHT, units = "px", pointsize = 10)
      #    plotEIC(xset, mzrange=mzrange, rtrange=rtrange); # same as plotTIC when it uses all m/zs
      # dev.off()
      png(filename = paste("Plot_TIC_", sampleFile,".png", sep=''), width = PNG_WIDTH, height = PNG_HEIGHT, units = "px", pointsize = 10)
          plotTIC(xset); # same as plotTIC when it uses all m/zs
      dev.off()
    # }
    
    ## get the RT and Intensity from EIC object
    # rt_int_matrix <- eic_obj@eic[1][[1]][[1]]         
    rt_int_matrix <- NULL
    # if (is.null(rt_int_matrix[, "intensity"])) stop(paste("rt_int_matrix is null in ", xset@filepath[1])) 
    ## sample case
    
    ## select significant/compound peaks & screening higher peak only to mapping compounds
    ## Peak detection depends on the 'extract_rt_w_highest_peaks_on_outliers' !!!   
    if(ctype=="TIC") {
        rt_peak <- extract_RT_forEachPeak(rt_int_matrix, offset, ctype, xset) # at line 405
    } else {
        rt_peak <- extract_RT_forEachPeak(rt_int_matrix, offset, ctype)  
    }
    
    if( FALSE )  {
        sampleName <- basename(xset@filepath[1]);
        plot(round(rt_peak[,1]/60,1), rt_peak[,2], type="h", xlim=c(08,60), ylim=c(0,max(rt_peak[,2])), main=sampleName)
      # round(rt_peak[,1]/60,3)
    }
    return(rt_peak)
}


# xset <- xset.asample
extract_peak_list_samples2 <- function(xset, ctype="TIC", offset=1.5, plotFile=TRUE)  {
      # not used because using TIC (Jan 21, 2016)
      # mzrange <- t(xset$xraw@mzrange)
      # rtrange <- t(range(xset$xraw@scantime))

      ## Generate multiple extracted ion chromatograms for m/z values of interest. 
      ## For xcmsSet objects, reread original raw data and apply precomputed 
      ## retention time correction, if applicable.
      # eic_obj <- getEIC(xset$xraw, mzrange=mzrange, rtrange=rtrange)
      # eic_obj <- getEIC(xset, mzrange=mzrange, rtrange=rtrange, sampleidx=i, rt="corrected")

      # TIC plot generation  
      if (plotFile) {
            sampleFile <- sub(".mzXML|.CDF", "", basename(xset$xraw@filepath[1]), ignore.case=TRUE)
            ## plot TIC instead of EIC (Jan 19, 2016) 
            # png(filename = paste("Plot_EIC_", sampleFile,".png", sep=''), width = PNG_WIDTH, height = PNG_HEIGHT, units = "px", pointsize = 10)
            #    plotEIC(xset$xraw, mzrange=mzrange, rtrange=rtrange); # same as plotTIC when it uses all m/zs
            # dev.off()
            png(filename = paste("Plot_TIC_", sampleFile,".png", sep=''), width = PNG_WIDTH, height = PNG_HEIGHT, units = "px", pointsize = 10)
                plotTIC(xset$xraw); # same as plotTIC when it uses all m/zs
            dev.off()
      }
      
      ## get the RT and Intensity from EIC object
      # rt_int_matrix <- eic_obj@eic[1][[1]][[1]]         
      rt_int_matrix <- NULL 
      # if (is.null(rt_int_matrix[, "intensity"])) stop(paste("rt_int_matrix is null in ", xset$xraw@filepath[1])) 
      ## sample case
      
      ## select significant/compound peaks & screening higher peak only to mapping compounds
      ## Peak detection depends on the 'extract_rt_w_highest_peaks_on_outliers' !!!   
            
      if(ctype=="TIC") {
          rt_peak <- extract_RT_forEachPeak(rt_int_matrix, offset, ctype, xset$xraw)
      } else {
          rt_peak <- extract_RT_forEachPeak(rt_int_matrix, offset, ctype)  
      }
      
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
  peak_list_sample <- peak_list_sample[which(peak_list_sample$RI > 0), ]
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
          if(FALSE & DEBUG) cat("## RT end:", peak.rtmax, " RT end (min):", peak.rtmax/60, " diff.intensity:", diff.intensity, "\n")
          idx <- idx + 1
        }        

        if (FALSE & DEBUG) cat("## Peak RT Range (RT:", peak.rt/60,") :\n\t Step1 - RTmin:", peak.rtmin/60," RTmax:", peak.rtmax/60, "\n\n")
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
        
        if (FALSE & DEBUG) { 
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


## get range of ION peak with 2nd Derivative
getIonPeakRange <- function(df.EIC, in.RT=NULL, method.interpolation="linear", lag.2nd=7, fname=NULL, IonType=NULL, cmpdname=NULL) {
  
      if(DEBUG) cat("## [getIonPeakRange] linear interpolation with approx() to increase more points:\n")
      # df.EIC <- as.data.frame(approx(df.EIC, method = method.interpolation))
      
      # sm <- smooth.spline(df.EIC)
      # print(str(sm))
      # df.EIC <- data.frame(rt=sm$x, intensity=sm$y)
  
      #df.EIC <- as.data.frame(spline(df.EIC))
      df.EIC.sp <- as.data.frame(spline(df.EIC, n=3*nrow(df.EIC)))
      # df.EIC.ap <- as.data.frame(approx(df.EIC.sp, method = "linear"))
      # names(df.EIC.ap) <- c("rt","intensity")
      names(df.EIC.sp) <- c("rt","intensity")
      # print(df.EIC)
  
      sm <- smooth.spline(df.EIC.sp$rt, df.EIC.sp$intensity, spar=0.50)
      sm <- data.frame(rt=sm$x, intensity=sm$y)
      
      # find a new apex of EIC peak
      apex.intensity <- max(sm[which( (sm$rt > (in.RT-0.5)) & (sm$rt < (in.RT+0.5)) ), "intensity"])
      rt.EIC.apex <- sm[which(sm$intensity == apex.intensity), "rt"]
      
      ## second derivative
      intensity.derivative <- diff(sm$intensity, differences=2, lag=lag.2nd);
      rt.len <- length(sm$rt)
      rt.vec <- sm$rt[-c(1:lag.2nd,(rt.len-lag.2nd+1):rt.len)]
      df.EIC.sm.derivative <- data.frame(rt=rt.vec, intensity= - intensity.derivative)
      
      ## Check the EIC peak shape - if problem in peak shape, return with NULL
      bp <- boxplot(sm$intensity, plot=FALSE)
      # rt.eic.intensity <- round(max(sm[which( (sm$rt > (in.RT-0.05)) & (sm$rt < (in.RT+0.05)) ), "intensity"]), 1)
      rt.eic.intensity <- round(max(sm[which( (sm$rt > (rt.EIC.apex-0.05)) & (sm$rt < (rt.EIC.apex+0.05)) ), "intensity"]), 1)
      
      # peakheight.threshold <- bp$stat[3] + 2*(bp$stat[4]-bp$stat[2])
      peakheight.threshold <- bp$stat[4]
      if ( rt.eic.intensity < peakheight.threshold) {
            if(DEBUG & !is.null (fname)) {
              cat("## [getIonPeakRange] Skip peak - because of lower peak intensity than threshold:\n")
              cat("\t rt.eic.intensity:", rt.eic.intensity, " peakheight.threshold:", peakheight.threshold,"\n")
        
              png(filename = paste("Plot_EIC_Skipped_", fname,"_RT", round(in.RT/60,2),"_",cmpdname,"_",IonType,".png", sep=''), width = PNG_WIDTH, height = PNG_HEIGHT*2, units = "px", pointsize = 10)
              par(mfrow=c(2,1))
                  plot(sm, type="b", main=paste("# Skipped Peak #  EIC - RT:", in.RT), xlim=range(rt.vec), ylim=c(0,max(df.EIC.sp$intensity)) )
                  abline(h=peakheight.threshold, col="blue", lwd=1)
                  plot(df.EIC.sm.derivative, type="b", main="2nd Derivative")
              dev.off()
            }

            return (list(peakMatrix=NULL, start=NULL, end=NULL, eic.intensity=rt.eic.intensity))
      }
      
      ## find the "inflection points" with 2nd derivative 
      ##
      df.derivative.left <- df.EIC.sm.derivative[which(df.EIC.sm.derivative$rt < in.RT), ]
      df.derivative.right <- df.EIC.sm.derivative[which(df.EIC.sm.derivative$rt > in.RT), ]
      # df.derivative.left <- df.EIC.sm.derivative[which(df.EIC.sm.derivative$rt < rt.EIC.apex), ]
      # df.derivative.right <- df.EIC.sm.derivative[which(df.EIC.sm.derivative$rt > rt.EIC.apex), ]

      i <- length(df.derivative.left$rt)
      while ( df.derivative.left$intensity[i] >= 0 ) {
         i <- i-1
      }
      rt.inflection.left <- round(df.derivative.left$rt[i], 3)
      
      i <- 1
      while ( df.derivative.right$intensity[i] >= 0 ) {
        i <- i+1
      }
      rt.inflection.right <- round(df.derivative.right$rt[i], 3)
      
      # check for inflection points 
      if( !is.null (fname) ) {
        png(filename = paste("Plot_TEST_Inflection_", fname,"_RT", round(in.RT/60,2),"_",cmpdname,"_",IonType,".png", sep=''), width = PNG_WIDTH, height = PNG_HEIGHT*2, units = "px", pointsize = 10)
        par(mfrow=c(2,1))
        plot(sm, type="b", main=paste("Inflection EIC - RT:", in.RT), xlim=range(rt.vec), ylim=c(0,max(df.EIC.sp$intensity)) )
        abline(v=in.RT, col="blue", lwd=1)
        abline(v=rt.EIC.apex, col="red", lwd=1)

        abline(v=rt.inflection.left, col="green", lwd=1)
        abline(v=rt.inflection.right, col="green", lwd=1)

        plot(df.EIC.sm.derivative, type="b", main="2nd Derivative")
        abline(v=in.RT, col="blue", lwd=2)
        abline(v=rt.EIC.apex, col="red", lwd=1)
        dev.off()
      }   
      
      ## find peak start & end with sm (smooth.spline)
      ## 
      
      if( (rt.inflection.left < rt.EIC.apex) & (rt.inflection.right > rt.EIC.apex) )  {
          ## data from liftoff to inflection, and inflection to touchdown
          df.sm.left <- sm[which(sm$rt < rt.inflection.left), ]
          df.sm.right <- sm[which(sm$rt > rt.inflection.right), ]
    
          if( DEBUG ) {
              cat("\n\n### getIonPeakRange - fname:", fname, " IonType:", IonType,"\n");
              cat("in.RT:", in.RT, " rt.inflection.left:", rt.inflection.left," rt.inflection.right:",rt.inflection.right,"\n")
              cat("nrow(df.sm.left):", nrow(df.sm.left), "\n")
              cat("nrow(df.sm.right):", nrow(df.sm.right), "\n")
          }
          
          # left side
          # ---------------------------------------
          i <- length(df.sm.left$rt)
          while( (i > 2) & ((df.sm.left$intensity[i] - df.sm.left$intensity[i-1]) > 0) ) {
              i <- i - 1
          }
          rt.start <- round(df.sm.left$rt[i], 3)
          rt.st1 <- rt.start
          
          # 2nd check with df.EIC.sm.derivative
          if (TRUE) {
              slope.avg <- 0
              inflection.intensity <- max(df.sm.left$intensity)
              if(i==1) { i <- i+1 }
              while( (i < length(df.sm.left$rt)) & (slope.avg < 0.02) ) {
                slope1 <- (df.sm.left$intensity[i] - df.sm.left$intensity[i-1])/(df.sm.left$rt[i]-df.sm.left$rt[i-1]); 
                slope2 <- (df.sm.left$intensity[i+1] - df.sm.left$intensity[i])/(df.sm.left$rt[i+1]-df.sm.left$rt[i]); 
                slope.avg <- ((slope1 + slope2)/2) / inflection.intensity
                if(FALSE) cat("\t left - i:", i, " slope.avg:", slope.avg, "\n")

                i <- i + 1
              }
              rt.start <- round(df.sm.left$rt[i], 3)
              slope.avg.left <- round(slope.avg,4)
          }
          
          # right side
          # ---------------------------------------
          i <- 2
          while( (i < length(df.sm.right$rt)) & ((df.sm.right$intensity[i] - df.sm.right$intensity[i+1]) > 0) ) {
              i <- i + 1
          }
          rt.end <- round(df.sm.right$rt[i], 3)
          rt.end2 <- rt.end
          
          # cat("\n\n## right side - rt.end:", rt.end, " rt[2]:", df.sm.right$rt[2], "\n\n")

          # 2nd check with df.EIC.sm.derivative
          if (TRUE) {
              slope.avg <- 0
              inflection.intensity <- max(df.sm.right$intensity)
              if(i==nrow(df.sm.right)) { i <- i-1 }
              while( (i > 2) & (slope.avg < 0.02) ) {
                slope1 <- (df.sm.right$intensity[i] - df.sm.right$intensity[i+1])/(df.sm.right$rt[i+1]-df.sm.right$rt[i]); 
                slope2 <- (df.sm.right$intensity[i-1] - df.sm.right$intensity[i])/(df.sm.right$rt[i]-df.sm.right$rt[i-1]); 
                slope.avg <- ((slope1 + slope2)/2) / inflection.intensity
                if(FALSE) cat("\t right - i:", i, " slope.avg:", slope.avg, " s1:", slope1, "  s2:", slope2,"\n")
                
                i <- i - 1
              }
              rt.end <- round(df.sm.right$rt[i], 3)
              slope.avg.right <- round(slope.avg,4)
          }
          
          
          # return Smooth.Splined Peak instead of Original Peaks which has lots of noise
          peakMatrix <- sm[which((sm$rt > rt.start) & (sm$rt < rt.end)), ]
          if(DEBUG) {
              # cat("## peakMatrix:\n"); print(peakMatrix);
              cat("## range:", rt.start,"~", rt.end,"\n")
          }
      } else {
          # no significant peak (EIC)
          peakMatrix <- NULL
          rt.start <- NULL
          rt.end <- NULL
          rt.st1 <- rt.start
          slope.avg.left <- " SKIPPED"
          slope.avg.right <- " SKIPPED"
      }    
      
      # peak is OK then next
      if( DEBUG & !is.null (fname) ) {
          png(filename = paste("Plot_EIC_", fname,"_RT", round(in.RT/60,2),"_",cmpdname,"_",IonType,".png", sep=''), width = PNG_WIDTH, height = PNG_HEIGHT*2, units = "px", pointsize = 10)
          par(mfrow=c(2,1))
          plot(df.EIC.sp, type="l", main=paste("Interpolated EIC - RT:",in.RT," slope.L:", slope.avg.left," slope.R:", slope.avg.right)
                                  , xlim=range(rt.vec), ylim=c(0,max(df.EIC.sp$intensity)))
          lines(sm, col="red", lwd=1)
          abline(v=in.RT, col="blue", lwd=2)
          abline(v=rt.EIC.apex, col="red", lwd=1)

          if(!is.null(rt.start) & !is.null(rt.end)) {
              abline(v=rt.start, col="green", lwd=2)
              abline(v=rt.end, col="green", lwd=2)
          }
          abline(h=peakheight.threshold, col="green", lwd=1) # if the peak is less than upper whisker, then that is not correct matched one
          
          plot(df.EIC.sm.derivative, type="b", main="2nd Derivative")
          abline(v=in.RT, col="blue", lwd=2)
          abline(v=rt.EIC.apex, col="red", lwd=1)
          dev.off()
      }   
      
      return (list(peakMatrix=peakMatrix, start=rt.start, end=rt.end, eic.intensity=rt.eic.intensity))
}


# mz & intensity for each peak
getMZIntensityofEachPeak2 <- function(xset.one, peak_rt_vec) {
  
      mzIntList <- list()
      nPeakRT <- length(peak_rt_vec)
      nScanIndex <- max(xset.one@scanindex)
      
      # repeat each RT 
      for ( i in 1:nPeakRT) {    
            # i <- 322
            peak.scanidx <- which(xset.one@scantime == peak_rt_vec[i]) 
            if(FALSE & DEBUG) cat("i:", i, "peak.scanidx:", peak.scanidx, "\n")
            
            if ( length(peak.scanidx) == 0) {
                  if (DEBUG) { cat("There is no matched RT - rt:", peak_rt_vec[i], " --- skipped\n") }
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

            mzIntensity <- as.data.frame(getScan(xset.one, peak.scanidx)) # *** give all m/z & intensity spectrum         
            # convert m/z into integer
            mzIntensity$mz <- toIntMZ.sample(mzIntensity$mz)

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


screeningPeakinLib <- function(peakInfo, lib.peak, RI.Variation=0.03)
{

    peakInfo <- peakInfo[which(peakInfo$RI > 0), ]
  
    matched.RI <- NULL
    for (i in 1:nrow(peakInfo)) {
        peak.RI <- peakInfo[i,"RI"]

        ## get candidate screening with RI
        RI.Offset <- peak.RI * RI.Variation ## +/- 3% variation
        lib.matched <- lib.peak[which(lib.peak$RI > (peak.RI - RI.Offset) & lib.peak$RI < (peak.RI + RI.Offset)), ]
        if (FALSE & DEBUG) { 
          if(length(lib.matched$RI) > 0) { cat("\n# lib.matched at RI:", peak.RI,"\n"); print(lib.matched[,  c('Compound','RI','RT')]) }
        }

        ## if peak's RI is matched with Library, keep it.
        if (length(lib.matched$RI) > 0) {          
            matched.RI <- c(matched.RI, i)
        }
    }

    peakInfo.new <- peakInfo[matched.RI, ]

    return (peakInfo.new)
}



## Using Target/Qualification Ions (Feb 17, 2016)
## using the area of EIC
## 
# asample.peakInfo <- peak_samples_ri
# xset.one <- xset.asample
# print_mzInt=FALSE; to make library update 
compoundIdentify4 <- function(asample.peakInfo, xset.one, lib.peak, alkaneInfo, RI.Variation=0.03, isBLANK=FALSE, print_mzInt=FALSE)
{
      if (DEBUG.TIME) { t2.beg = Sys.time() }
  
      if(isBLANK) {
          if(DEBUG) { cat("\n## Compound Identify: Blank Sample\n\n")}
          xset.one <- xset.one
      } else {
          xset.one <- xset.one$xraw
      }
        
      # get m/z and intensity using xcmsRaw() for a sample spectra
      
      ## varifying peak compound check
      ## get peaks' RIs and RTs 
      ## get m/z and intensity  
      ## for RT, it should be used raw RT instead of corrected
      if (DEBUG)  { cat("asample.peakInfo:\n"); print(asample.peakInfo) }
  
      if (DEBUG) { nRI.before <- nrow(asample.peakInfo) }
      
      ## screening RT/RI with library before calculate mz/intensity/other measures
      # asample.peakInfo <- screeningPeakinLib(asample.peakInfo, lib.peak, RI.Offset=3)
      asample.peakInfo <- screeningPeakinLib(asample.peakInfo, lib.peak, RI.Variation = 0.02)
      
      if (DEBUG) { 
          cat("asample.peakInfo - screened:\n")
          print(asample.peakInfo) 
          nRI.after <- nrow(asample.peakInfo)
          cat("\n\n## After screening RI with Library:", nRI.after,"/", nRI.before," was remained\n")
      }
      
      # peak_rt_vec <- asample.peakInfo[, "rt"] 

      ## get rt/mz/intensity/area/... for each peak
      # peak_mzInt_list <- getMZIntensityofEachPeak2(xset.one, peak_rt_vec)
      peak_mzInt_list <- getMZIntensityofEachPeak2(xset.one, asample.peakInfo[, "rt"])
      if(FALSE & DEBUG) { cat("peak_mzInt_list\n"); print(head(peak_mzInt_list)) }
      
      if (DEBUG.TIME) { spentTime(t2.beg, "T2: after getMZIntensityofEachPeak2") }

      ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      ## used for mz/int DB update in Internal Library
      ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      if(Flag.mzInt4DB) {
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

      # get mass spectrum data for plotting mass spectrum (JSON format)
      getMassSpec <- function(mzs, mzInts, rt)
      {    
          mzlist <- paste(noquote(mzs), collapse=",")
          intList <- paste(mzInts, collapse=",")
          
          # max.mzInts <- sort(mzInts, decreasing=TRUE)[c(1:3)]
          # mzMaxInts <- paste(c(mzs[which(max.mzInts[1] == mzInts)], mzs[which(max.mzInts[2] == mzInts)],
          #                mzs[which(max.mzInts[3] == mzInts)]), collapse=", ")

          # rt, m/z, intensity      
          # return (list(rt=rt, mzlist=mzlist, intList=intList, mzMaxInts=mzMaxInts))
          return (list(rt=rt, mzlist=mzlist, intList=intList))
      }
      
      getEICPeakRTOffset <- function(mz, intensity, aIon)  {
          aIonIntensity <- intensity[which(mz == aIon)]
          if( FALSE ) {
              cat("# getEICPeakRTOffset:", aIon,"\n")
              cat("mz:\n"); print(mz)
              cat("aIonIntensity:", aIonIntensity,"\n")
          }
          
          if (aIonIntensity > 2000000) {
              rt.offset <- 20.0
          } else if ( (aIonIntensity < 2000000) & (aIonIntensity > 100000)) {
              rt.offset <- 15.0
          } else if ( (aIonIntensity < 100000) & (aIonIntensity > 50000)) {
              rt.offset <- 15.0
          } else if ( (aIonIntensity < 50000) & (aIonIntensity > 5000)) {
              rt.offset <- 15.0
          } else {
              rt.offset <- 10.0
          }
          # rt.offset <- 30.0
          return (rt.offset)
      }
      
      ## note: the length of each vector should be same. if not, the function will be stop and give message
      maxIntensity <- max(asample.peakInfo[,'intensity'])  
      rtRange <- range(xset.one@scantime)

      if( DEBUG ) {
          cat("\n################################################\n")
          cat("## CompoundIdentify4\n")
          cat("################################################\n")
      }
      
      ## for each peak
      ## identifying compounds using each peak's RI and RT/m.z/intensity with library
      identifiedList <- NULL # collecting identified information for all peaks in a sample spectrum
      fname.identifeid.trace <- paste(sub(".mzXML|.CDF", "", basename(xset.one@filepath), ignore.case=TRUE),"_identified.trace.tsv", sep='')  
      flag.firstItem <- TRUE
      
      # for (j in 1:length(peak_rt_vec)) {
      for (j in 1:nrow(asample.peakInfo) ) {
          identified <- NULL
          
          # use peak location/index for no alkane covering
          peakRT <- as.numeric(asample.peakInfo[j,"rt"])
          relativePeakRT <- (peakRT - rtRange[1]) / (rtRange[2] - rtRange[1])

          ## step 1) using RI, screening candidate 
          RI.sample <- as.numeric(asample.peakInfo[j,"RI"])
          RT.sample <- as.numeric(asample.peakInfo[j,"rt"])  
          
          if( DEBUG ) {
              cat("\n\n################################################\n")
              cat("##", j, "th   RI:",RI.sample, "RT:", round(RT.sample/60,2), "\n" )
          }
          
          if (RI.sample > 0) {  ## not -1  # if -1 then exclude in the previous modules!!!
              ## get candidate screening with RI
              RI.offset <- RI.sample * RI.Variation ## +/- 3% variation
              lib.matched <- lib.peak[which(lib.peak$RI > (RI.sample - RI.offset) & lib.peak$RI < (RI.sample + RI.offset)), ]
              # temporary
              # for using Target Ions
              lib.matched <- lib.matched[which(!is.null(lib.matched$TargetIon) & !is.na(lib.matched$TargetIon) ), ]
              if (FALSE & DEBUG) {
                  cat("RI.offset:",RI.offset ," RI Range:",(RI.sample - RI.offset), " ~ ", (RI.sample + RI.offset),"\n" )
                  cat("lib.matched: \n"); print(lib.matched[,  c('Compound','RI','RT','TargetIon','QIon')])
              }
              
              ## step 2) for selected candidate compounds, calculate scores like Match Factor and others?
              if (length(lib.matched$RI) > 0) {          
                  # sample_mzs_vec <- peak_mzInt_list[[j]]$mzInt[,"mz"] ## from xcmsRaw
                  sample_mzs_vec <- toIntMZ.sample(peak_mzInt_list[[j]]$mzInt[,"mz"]) ## from xcmsRaw
                  sample_mz_int_vec <- peak_mzInt_list[[j]]$mzInt[,"intensity"] ## from xcmsRaw
                  peakIntensity <- peak_mzInt_list[[j]]$peakIntensity # just for adding addition information
                  peakArea <- peak_mzInt_list[[j]]$peakArea
                  peakRTStart <- peak_mzInt_list[[j]]$peakRTStart
                  peakRTEnd <-  peak_mzInt_list[[j]]$peakRTEnd
                  peakType <- peak_mzInt_list[[j]]$peakType
                  
                  # if the number of m/z of a sample peak < 10, then just skip this peak 
                  if (length(sample_mzs_vec) < 10) {
                      if(DEBUG) { cat("### sample_mzs_vec < 10:\n"); print(sample_mzs_vec) }
                      next
                  }
                  
                  if(DEBUG) { 
                      cat("### sample_mzs_vec - RI:",RI.sample, "RT:", round(RT.sample/60,2), "\n"); 
                      print(sample_mzs_vec) 
                  }
                  
                  ## identifying a correct and best matched compound from library
                  ## add final decision (compound name) with the similarity scores 
                  TScore <- 0;
                  RIScore <- 0;

                  identified.trace <- NULL; ## for trace
                  if (DEBUG.TIME) { spentTime(t2.beg, "T2: before for loop to find a matched case in lib\n") } # 0.25 sec
                  for(k in 1:length(lib.matched$RI)) {  ## repeat for selected candidates of Library 
                      # k<-1
                      ## calculate scores (MF_Score, Prob, ...)      
                      alib.matched <- lib.matched[k,]
                      
                      # RI closest (100%) using RI.offset, 2*RI.offset is the maximum range
                      RI.similarity <- round((2*RI.offset - abs(alib.matched$RI - RI.sample)) / (2*RI.offset) * 100, 2)
                      
                      ref_MZS_vec <- as.numeric(unlist(strsplit(as.character(alib.matched$MZ), split=" ")))
                      ref_INT_vec <- as.numeric(unlist(strsplit(as.character(alib.matched$Intensity), split=" ")))                

                      if( FALSE ) {
                          cat("\n## alib.matched:\n"); 
                          print(alib.matched[,c(1:9)]);
                          cat("\t alib.matched$TargetIon:",alib.matched$TargetIon,"\n\t QIon:", alib.matched$QIon, "\n\t IonArea(EIC) Ratio:",alib.matched$IonRatio,"\n")
                          print(c(alib.matched$TargetIon, alib.matched$QIon) %in% sample_mzs_vec)
                          print(length(which(c(alib.matched$TargetIon, alib.matched$QIon) %in% sample_mzs_vec)))
                          # stop()
                      }
                      
                      if(DEBUG & (alib.matched$Compound == "Ribitol")) {
                          print(which(c(alib.matched$TargetIon, alib.matched$QIon) %in% sample_mzs_vec))
                          # stop()
                      }
                      
                      if(length(which(c(alib.matched$TargetIon, alib.matched$QIon) %in% sample_mzs_vec)) < 2) {
                          if(DEBUG) { 
                              cat("\n## SKIP following procedure [Compound Identification]: No matched Target & Qualification Ions\n");
                              cat("## Sample's RI:",RI.sample, "RT:", round(RT.sample/60,2), "\n"); 
                              print(alib.matched[,c(1:9)]);
                              cat("\t alib.matched$TargetIon:",alib.matched$TargetIon,"\n\t QIon:", alib.matched$QIon, "\n\t IonArea(EIC) Ratio:",alib.matched$IonRatio,"\n")
                          }
                          next # go to next candidate compound in library
                        
                      } else {
                          if(DEBUG) { 
                            cat("\n## alib.matched:\n"); 
                            cat("\t alib.matched$TargetIon:",alib.matched$TargetIon,"\n\t QIon:", alib.matched$QIon, "\n\t IonArea(EIC) Ratio:",alib.matched$IonRatio,"\n")
                          }
                      }
                      
                      # finding matched m/z for a peak
                      # round.digit <- 0 # 0 or 2; not 1
                      lst <- find_similar_peaks(ref_MZS_vec, ref_INT_vec, sample_mzs_vec, sample_mz_int_vec)
                      if (length(lst$MZS_vec_tmp)==0) { 
                          if(DEBUG) {
                              amsg <- paste("## Warning: No matched mass (m/z) - compound:", as.character(alib.matched$CompoundWithTMS),"\n",sep="")
                              cat(amsg);
                              cat(file=File.ErrorLog, amsg, append=TRUE)
                          }
                          # stop("No m/z matched")
                          next # skip following because no matched mass (m/z)
                      } else {
                          if (FALSE & DEBUG) {
                              cat("## Match Factor) # of m/z matched:", length(lst$MZS_vec_tmp), " ref m/z:", length(ref_MZS_vec)
                                  , " sample m/z:", length(sample_mzs_vec), "\n")
                              cat("# RT:", round(peakRT/60,2),"\n")
                          }
                      }
                      
                      ## get EIC Areas of Target & Qualification Ions 
                      if (DEBUG) cat("\n\n## EIC Areas of Target & Qualification Ions \n\n")
                      # objEIC.Target <- getEIC(xset.one, mzrange=matrix(c(alib.matched$TargetIon-1, alib.matched$TargetIon+1), ncol=2), rtrange=matrix(c(RT.sample-5, RT.sample+5), ncol=2), step=0.1)
                      # objEIC.Qualification <- getEIC(xset.one, mzrange=matrix(c(alib.matched$QIon-1, alib.matched$QIon+1), ncol=2), rtrange=matrix(c(RT.sample-5, RT.sample+5), ncol=2), step=0.1)
                      
                      rt.offset.EIC <- getEICPeakRTOffset(sample_mzs_vec, sample_mz_int_vec, alib.matched$TargetIon)

                      ## if (DEBUG.TIME) { spentTime(t2.beg, "T2: before") } ## 0.05 sec
                      objEIC.Target <- getEIC(xset.one, mzrange=matrix(c(alib.matched$TargetIon-0.3, alib.matched$TargetIon+0.7), ncol=2), rtrange=matrix(c(RT.sample-rt.offset.EIC, RT.sample+rt.offset.EIC), ncol=2), step=0.1)
                      objEIC.Qualification <- getEIC(xset.one, mzrange=matrix(c(alib.matched$QIon-0.3, alib.matched$QIon+0.7), ncol=2), rtrange=matrix(c(RT.sample-rt.offset.EIC, RT.sample+rt.offset.EIC), ncol=2), step=0.1)

                      df.EIC.Target <- as.data.frame(objEIC.Target@eic[1][[1]][[1]])
                      df.EIC.Qualification <- as.data.frame(objEIC.Qualification@eic[1][[1]][[1]])
                      
                      # peakRange <- getPeakRange2(xset.one, nScanIndex, peak.scanidx)
                      # find a range of a peak with 2nd derivation with interpolation
                      if(DEBUG) cat("# lib.matched[k,]$CompoundWithTMS:", as.character(lib.matched[k,]$CompoundWithTMS),"\n")
                      if(FALSE & (length(grep("ISTD", lib.matched[k,]$CompoundWithTMS)) > 0) ) {
                          sampleFile <- sub(".mzXML|.CDF", "", basename(xset.one@filepath), ignore.case=TRUE) 
                      } else {
                          sampleFile <- NULL
                          # sampleFile <- sub(".mzXML|.CDF", "", basename(xset.one@filepath), ignore.case=TRUE)
                      }
                      ## to check missing peak identification
                      # if (FALSE) {
                      #     sampleFile <- sub(".mzXML|.CDF", "", basename(xset.one@filepath), ignore.case=TRUE)  
                      #}
                      
                      if (DEBUG.TIME) { spentTime(t2.beg, "T2: before - aPeakRange") }
                      aPeakRange.target <- getIonPeakRange(df.EIC.Target, in.RT=RT.sample, method.interpolation="linear", lag.2nd=2, fname=sampleFile, IonType = "Target", cmpdname=as.character(lib.matched[k,]$CompoundWithTMS))
                      aPeakRange.qualification <- getIonPeakRange(df.EIC.Qualification, in.RT=RT.sample, method.interpolation="linear", lag.2nd=2, fname=sampleFile, IonType = "Qualification",cmpdname=as.character(lib.matched[k,]$CompoundWithTMS))
                      if (DEBUG.TIME) { spentTime(t2.beg, "T2: after - aPeakRange\n\n") }
                      
                      # aPeakRange.qualification <- getIonPeakRange(df.EIC.Qualification, in.RT=RT.sample, method.interpolation="linear", lag.2nd=5, fname=sampleFile, IonType = "Qualification",cmpdname=as.character(lib.matched[k,]$CompoundWithTMS))
                      if ( is.null(aPeakRange.target$peakMatrix) | is.null(aPeakRange.qualification$peakMatrix)) {
                          if( DEBUG ) {
                              cat("## Skipped to matching compound (", as.character(lib.matched[k,]$CompoundWithTMS), ") due to no significant Ion Peak\n\n")
                          }
                          next
                      }
                      
                      ## area calculation (Intensity, Range)    
                      # peakArea <- getPeakArea3(xset.one, xset.one@tic[peak.scanidx], peakRange) 
                      if (DEBUG.TIME) { spentTime(t2.beg, "T2: before - aPeakArea") }
                      aPeakArea.target <- getIonPeakArea(peakMatrix=aPeakRange.target$peakMatrix, x.start=aPeakRange.target$start, x.end=aPeakRange.target$end)
                      aPeakArea.qualification <- getIonPeakArea(peakMatrix=aPeakRange.qualification$peakMatrix, x.start=aPeakRange.qualification$start, x.end=aPeakRange.qualification$end)
                      if (DEBUG.TIME) { spentTime(t2.beg, "T2: before - aPeakArea\n\n") }
                      
                      if (DEBUG) {
                          # cat("aPeakRange.Target:\n"); print(aPeakRange.Target)
                          # cat("aPeakRange.Qualification:\n"); print(aPeakRange.Qualification)
                          
                          cat("# Ion Area -  target:", aPeakArea.target, ", qualification:", aPeakArea.qualification, "\n")
                          cat("# Ion area ratio (Q1 * 100)/Target:",  round((aPeakArea.qualification * 100)/aPeakArea.target,1),"\n")
                          cat("\t Ion Area(EIC) Ratio (Lib):", alib.matched$IonRatio,"\n")
                          cat("\t Compound (Lib):", as.character(alib.matched$Compound), "\n")
                          cat("# RT.sample:", RT.sample, "sec (min:", round(RT.sample/60, 2), ")\n")
                      }
                      
                      # EIC peak plot for developing 
                      # check whether correctely select the range
                      if (FALSE & DEBUG) {
                          sampleFile <- sub(".mzXML|.CDF", "", basename(xset.one@filepath), ignore.case=TRUE)  
                          png(filename = paste("Plot_EIC_", sampleFile,"_RT", round(RT.sample/60,2),"_", as.character(lib.matched[k,]$CompoundWithTMS),".png", sep=''), width = PNG_WIDTH, height = PNG_HEIGHT*2, units = "px", pointsize = 10)
                          par(mfrow=c(2,1))
                          # plotEIC(xset.one, mzrange=matrix(c(alib.matched$TargetIon-1, alib.matched$TargetIon+1), ncol=2), rtrange=matrix(c(RT.sample-5, RT.sample+5), ncol=2))
                          # plotEIC(xset.one, mzrange=matrix(c(alib.matched$QIon-1, alib.matched$QIon+1), ncol=2), rtrange=matrix(c(RT.sample-5, RT.sample+5), ncol=2))
                          plot(aPeakRange.target$peakMatrix, type="b", ylim=c(0,max(aPeakRange.target$peakMatrix$intensity)),
                               main=paste("RT.sample:",round(RT.sample/60,2),"(",RT.sample,") Range:",aPeakRange.target$start,"~", aPeakRange.target$end, " Target Ion:", alib.matched$TargetIon, sep="") )
                          plot(aPeakRange.qualification$peakMatrix, type="b",
                               main=paste("RT.sample:",round(RT.sample/60,2),"(",RT.sample,") Range:",aPeakRange.qualification$start,"~", aPeakRange.qualification$end, " Q Ion:", alib.matched$QIon, sep=""))
                          dev.off()
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
                      # tmp.TScore =  0.3 * MFscore/10 + 0.2 * RI.similarity + 0.5*matchMZrate  ## ~ Apr 27, 2016
                      tmp.TScore =  0.4 * MFscore/10 + 0.6 * RI.similarity + 0.3*cor.spearman + 0.1*matchMZrate
                      
                      hmdbID <- as.character(lib.matched[k,]$HMDB_ID)
                      
                      ## to verify result
                      tmp.identified <- c(
                          hmdbID=hmdbID,
                          CompoundWithTMS=as.character(lib.matched[k,]$CompoundWithTMS),
                          RT_min=round(RT.sample/60, 3),
                          RT=RT.sample,
                          RI=RI.sample,
                          RI.lib=lib.matched[k,]$RI,
                          Intensity=peakIntensity,
                          MatchFactor=MFscore,
                          RI.Similarity=RI.similarity,
                          Corr.Spearman=round(cor.spearman * 100, 2),
                          matchMZrate=round(matchMZrate,2),   
                          matchMZnum=matchMZnum,
                          sampleMZnum=sampleMZnum,
                          nearRelPeakRTscore=NULL,
                          TScore=round(tmp.TScore, 2),
                          TargetIon=alib.matched$TargetIon,
                          TargetIon.intensity= aPeakRange.target$eic.intensity,
                          QIon=alib.matched$QIon,
                          Area.EICTarget=aPeakArea.target,
                          Area.EICQualification=aPeakArea.qualification,
                          AreaRatio=round((aPeakArea.qualification * 100)/aPeakArea.target,1),
                          RT.Target.start= aPeakRange.target$start,
                          RT.Target.end= aPeakRange.target$end,
                          RT.Qualification.start= aPeakRange.qualification$start,
                          RT.Qualification.end= aPeakRange.qualification$end,
                          peakType = peakType
                      )

                      identified.trace <- rbind(identified.trace, tmp.identified) 

                      # if ( nearRelPeakRTscore < nearRelPeakRTscore.tmp 
                      # RI.similarity: 70 --> 90 (Dec 16, 2014)
                      # & (matchMZrate > 60.0) ## excluded Jun 20, 2016
                      # if ( (RI.similarity > RI_SIMILARITY_THRESHOLD) & (RIScore < RI.similarity) & (matchMZnum > 10) & (matchMZrate > 60.0)
                      if ( (RI.similarity > RI_SIMILARITY_THRESHOLD) & (RIScore < RI.similarity) & (matchMZnum > 10) 
                              & (hmdbID == "HMDB_ISTD1" | cor.spearman > 0.65) & (!is.na(tmp.TScore) & (tmp.TScore > TScore)) ) {
                          TScore <- tmp.TScore
                          RIScore <- RI.similarity
  
                          # get mass spectrum data for plotting mass spectrum (JSON format)
                          # and three mzs with highest Intensity
                          aMzInt <- getMassSpec(sample_mzs_vec, sample_mz_int_vec, RT.sample) 

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
                              TargetIon=alib.matched$TargetIon,
                              TargetIon.intensity= aPeakRange.target$eic.intensity,
                              QIon=alib.matched$QIon,
                              Area.EICTarget=aPeakArea.target,
                              Area.EICQualification=aPeakArea.qualification,
                              AreaRatio=round((aPeakArea.qualification * 100)/aPeakArea.target,1),
                              RT.Target.start= aPeakRange.target$start,
                              RT.Target.end= aPeakRange.target$end,
                              RT.Qualification.start= aPeakRange.qualification$start,
                              RT.Qualification.end= aPeakRange.qualification$end,
                              peakType = peakType,
                              mz = aMzInt$mzlist,
                              mzInt = aMzInt$intList,
                              mzMaxInts = aMzInt$mzMaxInts
                          )
                          if(DEBUG) {
                              cat("## added identified\n"); print(identified[c(1:13)])
                          }
                      }
                  }
                  if (DEBUG.TIME) { spentTime(t2.beg, "T2: end for loop to find a matched case in lib\n") }
                  
                  if(DEBUG & (length(identified.trace) > 0)) {
                    # cat("## tmp.identified:\n")
                    # print(tmp.identified[c(1:13)])
                    # fname.identifeid.trace <- paste(sub(".mzXML|.CDF", "", basename(xset.one@filepath), ignore.case=TRUE),"_identified.trace.tsv", sep='')  
                    if(flag.firstItem) {
                        write.table(identified.trace, file=fname.identifeid.trace, quote=TRUE, col.names=TRUE, row.names=FALSE, sep="\t", append=FALSE)
                        flag.firstItem <- FALSE
                    } else {
                        write.table(identified.trace, file=fname.identifeid.trace, quote=TRUE, col.names=FALSE, row.names=FALSE, sep="\t", append=TRUE)
                    }
                  }
                  
                  ## just show the list of identified candidates
                  # rownames(identified.trace) <- c(1:nrow(identified.trace) )
                  # identified.trace <- rbind(identified.trace, identified)  
                  if( DEBUG ) {
                      # cat("##########################################################\n")
                      # cat("fileName:", basename(xset.one@filepath[1]), "  RT:", round(RT.sample/60,3),"\n")
                      # rownames(identified.trace) <- c(1:nrow(identified.trace))
                      # print(identified.trace)
                  }
              }
            
              identifiedList <- rbind(identifiedList, identified)        
            
          }
          
          if (DEBUG.TIME) { spentTime(t2.beg, "T2: end the inside of for loop - each peak\n\n") }
          
          ## ========================================================
          ## if no matched RI from library because of no alkane    
          if ( FALSE & (RI.sample < 0) ) {   
              cat("\n#\n#\n#################\n")     
              cat("SKIP: RI.sample < 0 \n")
              cat("\n#\n#\n#################\n")
              
              if (FALSE & DEBUG) {
                  cat("## peak", j ,"th RT:", asample.peakInfo[j, "rt"],"\n")
                  cat("\n### NO RI was used because of no matched alkane\n\n")
              }
              # This part covers the peak which does not have alkane
              
              ## get candidate screening with RI of max/min alkane range
              if ( RT.sample < alkaneInfo$RTmin )  {                
                  lib.matched <- lib.peak[which(lib.peak$RI < alkaneInfo$RImin), ]
              } else if ( RT.sample > alkaneInfo$RTmax ) {
                  lib.matched <- lib.peak[which(lib.peak$RI > alkaneInfo$RImax), ]
              } else {
                  amsg <- paste("!!! Found unexpected RT case:", RT.sample, "\n")
                  cat(amsg)
                  cat(file=File.ErrorLog, amsg, append=TRUE)
                  
                  amsg <- "alkane detail:\n"
                  cat(amsg); print(alkaneInfo)
                  cat(file=File.ErrorLog, amsg, append=TRUE)
                  write.table(alkaneInfo, file=File.ErrorLog, append=TRUE)
                  
                  stopMessage("Error in compound profiling: Found unexpected RT case")
                  
              } 
              
              # cat("# N matched with lib compounds:", nrow(lib.matched), " a peak's RT:", asample.peakInfo[j,"rt"]/60, "\t j:", j, "\n");
              # print(lib.matched[,c("Compound","name_short","RT","RI")])
              
              # sample_mzs_vec <- peak_mzInt_list[[j]]$mzInt[,"mz"] ## from xcmsRaw
              sample_mzs_vec <- toIntMZ.sample(peak_mzInt_list[[j]]$mzInt[,"mz"]) ## from xcmsRaw
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
                  # round.digit <- 0 # 0 or 2; not 1
                  lst <- find_similar_peaks(ref_MZS_vec, ref_INT_vec, sample_mzs_vec, sample_mz_int_vec)
                  if (length(lst$MZS_vec_tmp)==0) { 
                    amsg <- paste("# ERROR - compound:", alib.matched$CompoundWithTMS, "\n")
                    cat(amsg)
                    cat(file=File.ErrorLog, amsg, append=TRUE)
                    
                    stopMessage("No m/z matched")
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

                    aMzInt <- getMassSpec(sample_mzs_vec, sample_mz_int_vec, RT.sample) 
                    
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
                        peakType = peakType,
                        mz = aMzInt$mzlist,
                        mzInt = aMzInt$intList,
                        mzMaxInts = aMzInt$mzMaxInts
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
      if (DEBUG.TIME) { spentTime(t2.beg, "T2: end the loop for nrow(asample.peakInfo)\n\n") }

      if (DEBUG)  { 
        cat("\n\n## After screening RI with Library:", nRI.after,"/", nRI.before," was remained\n");
        cat("identifiedList:\n"); print(identifiedList[,c(1:13)])
        cat("fileName:", basename(xset.one@filepath[1]), "\n")
      }
      
      if(! is.null(identifiedList)) {
          rownames(identifiedList) <- c(1:nrow(identifiedList))
      }
      
      # no need anymore because the hmdb id is included in the set
      # identifiedList <- merge( lib.peak[, c("HMDB_ID","CompoundWithTMS")], identifiedList, by=c("CompoundWithTMS"), sort=FALSE)

      # as.data.frame(identifiedList)
      
      ## return identified compounds with additional informations 
      ## such as (RT, RI, Intensity, Area (RT start/end), Similarity Measures)    
      return( as.data.frame(identifiedList) )
}


## area calculation
# EIC Ion Peak Area - calculating with interpolized matrix
getIonPeakArea <- function(peakMatrix, x.start, x.end)  {
    area <- 0
    
    if (nrow(peakMatrix) > 1) {
        for ( i in 2:nrow(peakMatrix) ) {
            width <- peakMatrix$rt[i] - peakMatrix$rt[i-1]  
            y <- (peakMatrix$intensity[i] + peakMatrix$intensity[i-1]) / 2
            area <- area + width * y
            # cat("## Area -  RT:", peakMatrix$rt[i], ",", peakMatrix$rt[i-1], "  y:", y, "  area:", width * y, "\n")
        } 
    }
    
    if (DEBUG) {
        if (area == 0) {
            cat("## getIonPeakArea() - area is zero! \n")
        } else {
            cat("## sum of peak area:", area, "\n")
        }
    }
    
    return ( round(area,2) )
}

# xr <- xset.one$xraw;
# peak.rt <- peak_rt_vec[i]
getPeakArea4 <- function(xraw, peakIntensity, peakRange)  { 
    if (peakRange$peakType %in% c("SYMMETRIC", "SKEWED") ) {
        area <- peakIntensity * (peakRange$RTmax - peakRange$RTmin)
    } else if (peakRange$peakType == "NORMAL") {
        area <- getPeakArea2(xraw, peakRange$RT, peakRange$RTmin, peakRange$RTmax)
    } else { # Strange
        area <- 0.0  
    }
    
    return ( round( area * 9.5, 0) )
}

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
      if(DEBUG) { cat("## No matced scan time in getPeakArea() - Peak's RT:", peak.rt, "(", peak.rt/60,")\n") }
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

find_similar_peaks <- function(MZS_vec, INTS_vec, mzs_vec, mz_int_vec, mzCutoff=50) {
    MZS_vec_tmp <- c()
    INTS_vec_tmp <- c()
    mzs_vec_tmp <- c()
    mz_int_vec_tmp <- c()
    
    # recommended to use over m/z > 50 only
    if (TRUE) {
        cutOffLen.mzs <- length(mzs_vec[which(mzs_vec < mzCutoff)])
        mzs_vec <- mzs_vec[-c(1:cutOffLen.mzs)]
        mz_int_vec <- mz_int_vec[-c(1:cutOffLen.mzs)]
      
        cutOffLen.MZS <- length(MZS_vec[which(MZS_vec < mzCutoff)])
        MZS_vec <- MZS_vec[-c(1:cutOffLen.MZS)]
        INTS_vec <- INTS_vec[-c(1:cutOffLen.MZS)]
    }    
    
    if (length(mzs_vec) == 0) {
        if(DEBUG) {
            cat("## No m/z of a peak of sample spectrum\n")
        }
        return(list(MZS_vec_tmp=MZS_vec_tmp, INTS_vec_tmp=INTS_vec_tmp, mzs_vec_tmp=mzs_vec_tmp, 
                  mz_int_vec_tmp=mz_int_vec_tmp))
    }
    for(i in 1:length(mzs_vec)) {
        sample_mz <- mzs_vec[i] # this will be used for M in Match Factor (MF) calcuration    
        # cat("sample_mz:"); print(sample_mz);
        for(j in 1:length(MZS_vec)) {
            ## if( MZS_vec[j] - mz_off_set  <= mzs_vec[i] &&  mzs_vec[i] <= MZS_vec[j] + mz_off_set){
            # if( round(MZS_vec[j], round.digit) == sample_mz ){

            if( MZS_vec[j] == sample_mz ) {
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
    
    MF_score <- round((1000 * c ** 2)/ (a * b), 1)
    
    return(MF_score)
}

## screening with highest TScore from the matched list 
# arrangeProfiledPeaks2 <- function(profiled_peaks, stype=NULL)
arrangeProfiledPeaks2 <- function(profiled_peaks)
{
    profiled_final <- NULL
    unique_complist <- unique(profiled_peaks$CompoundWithTMS)
    for (j in 1:length(unique_complist))  {
        # j <- 12
        tmpdata <- profiled_peaks[which(profiled_peaks$CompoundWithTMS == unique_complist[j]), ]    
        tmpdata <- tmpdata[order(as.double(as.character(tmpdata$TScore)), decreasing=TRUE), ] 
        final.comp <- tmpdata[1, ]
        if(FALSE) { cat("compd:", unique_complist[j], "\n"); cat("tmpdata:\n"); print(tmpdata);  cat("final.comp:\n"); print(final.comp) }
        profiled_final <- rbind(profiled_final, final.comp)
    }
    
    profiled_final <- profiled_final[order(as.double(as.character(profiled_final$RI)), decreasing=FALSE), ] 
    # profiled_final <- profiled_final[order(as.double(as.character(profiled_final$RT))), ]
    rownames(profiled_final) <- c(1:nrow(profiled_final))

    # change the variable types
    profiled_final$CompoundWithTMS <- as.character(profiled_final$CompoundWithTMS)
    # profiled_final$Area <- as.numeric(as.character(profiled_final$Area))
    profiled_final$MatchFactor <- as.numeric(as.character(profiled_final$MatchFactor))  

    return(profiled_final)
}

##################################################################################
## Quantification

## get AREA for each peak

# find matching library & call calc concentration ftn
calibration <- function(dbLib, hmdbID, relative_area_comp) {
    if(DEBUG) {
        cat("\n\n## calibration() # hmdbid:", hmdbID, "\t area_ratio:", relative_area_comp,"\n")  
    }
  
    libVec <- dbLib[which(as.character(dbLib$HMDB_ID) == hmdbID), ];
    if(DEBUG) { cat("### libVec:\n"); print(libVec); }
    
    
    if(nrow(libVec) == 1) {
        conc <- calcConcentration(relative_area_comp, slope=libVec$Slope, intercept=libVec$Intercept, loq.value=libVec$LOQ) # loq.value: uM
    } else {
        if(nrow(libVec) > 1) {
            amsg <- paste("\n\n## Error: more than two calibration curves were found:", hmdbID,"\n\n")
            cat(amsg);
            cat(file=File.ErrorLog, amsg, append=TRUE)
            print(dbLib[which(as.character(dbLib$HMDB_ID) == hmdbID), ]);
            conc <- "Error: More than two calibration curves"
        } else {
            conc <- "NA (No 'Matched' Calibration Curve)" # no calibration
        }
        if(DEBUG) {
          amsg <- paste("\n\n## Error: Cannot find a record in library:", hmdbID,"\n\n")
          cat(amsg)
          cat(file=File.ErrorLog, amsg, append=TRUE)
        }
    }
    return(conc)
}

# area: after divided by internal standard area
# return: uM conc
calcConcentration <- function(area, slope, intercept, loq.value=NULL)
{  
  
  if( is.na(slope) | is.na(intercept) ) {
      if(DEBUG) {
          cat("# [calcConcentration] area:", area,"\t slope:", slope, "\t intercept:", intercept,"\n")
          cat("\t Slope & Intercept are NA\n")
      }
      return ("NA (No Calibration Curve)");
  } 
  
  loq.calc <- NULL
  if ( area > 0 & area > intercept ) {
      conc <- round( (area - intercept) * 1000 / slope , 2); # unit: uM
  } else {
      # LOQ due to negative concentration
      # round( 0.0001 / slope , 2); # < LOD generation
      if(is.na(loq.value)) {
          conc <- "NO LOQ value"
      } else {
          if (area == 0 & intercept >= 0) {
            conc <- paste("< ", loq.value, sep='') # uM - use a calculated LOQ instead of minimum concentration in mixture             
          } else if (area == 0 & intercept < 0) {
            # calculated LOQ with area_ratio=0
            loq.calc <- (area -intercept)/slope * 1000 # unit: uM
            conc <- paste("< ", round(loq.calc, 1), sep='') # uM - use a calculated LOQ instead of minimum concentration in mixture 
          } else if ( area > 0 & area < intercept ) {
            conc <- paste("< ", loq.value, sep='') # uM - use a calculated LOQ instead of minimum concentration in mixture             
          } else {
            conc <- "NO case"
          }
      }
  }
  
  if(DEBUG) {
    cat("area ratio:", area, "\n")
    cat("intercept:", intercept,"\n")
    cat("slope:", slope,"\n")
    cat("LOQ:", loq.value,"\n")
    cat("Calc LOQ:", loq.calc,"\n")
    cat("conc:", conc,"\n\n")
  }
  
  return (conc)
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
      sumOfAreas <- sum(as.numeric(as.character(tmpdata$Area.EICTarget))) # Area is after subtract the blank 
      if( is.na(sumOfAreas) ) sumOfAreas <- 0
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
# unit <- uM"
# internalStdCmpd <- "Ribitol"
quantification <- function(profiled.result, lib, lib.curve, internalStdCmpd, sample_file, stype) {
  
    if (DEBUG) { cat("\n\n## quantification input peaks:", nrow(profiled.result), "\n") }

    # combine areas for the same compound
    hmdbIDwithArea <- getSamePeakArea(profiled.result)
    # hmdbIDwithArea
    if(DEBUG) { cat("\n\n## combine areas for the same compound (hmdbIDwithArea):\n"); print(hmdbIDwithArea); }
    
    internalStd.hmdbID <- as.character(lib[which(lib$Compound == internalStdCmpd), 'HMDB_ID'])
    area.internalStd <- as.double(as.character(hmdbIDwithArea[which(hmdbIDwithArea$hmdbID == internalStd.hmdbID), "Area"]))
    if(DEBUG) {
        cat("\t# selected internal standard compound:", internalStdCmpd, "(", internalStd.hmdbID,")\n\n")
        cat("\t# area.internalStd:", area.internalStd,"\n")
        cat("## internalStdCmpd:\n"); 
        print(profiled.result[which(as.character(profiled.result$Compound) == internalStdCmpd), c(1:24)])
    }

    if (length(area.internalStd) == 0 ) {
        stopMessage(paste("## Error: Cannot find internal standard compound '", internalStdCmpd, "' in the library.
           Please check the internal standard compound name both sample peak and library ") )
    }
    
    # hmdbIDwithArea: hmdbID, +Compound, +Concentration, Area,  NPeaks
    if (DEBUG) {
        cat("\n\n########################################################################\n")
        cat("## Quantification for each Compound \n")
        cat("## Calculating concentration with ratio (Target Ion's EIC area/ ISTD's)\n") 
        cat("########################################################################\n")
    }
    quantifiedList <- NULL
    for ( hmdbID in hmdbIDwithArea$hmdbID ) {
        aCmpd <- hmdbIDwithArea[which(hmdbIDwithArea$hmdbID == hmdbID), ]
        area_ratio <- as.double(as.character(aCmpd$Area)) / area.internalStd
        
        if (DEBUG) {
          cat("\n## hmdbID:", hmdbID, "\t area_ratio:", area_ratio,"\n")
          cat("=================================================================\n")
        }
        
        if ( (hmdbID != internalStd.hmdbID) ) {  
            calc <- calibration(lib.curve, hmdbID, area_ratio) # unit uM
        } else {
            calc <- 'ISTD' # internal standard
        }
        tmp.concTable <- data.frame(hmdbID=aCmpd$hmdbID, Area=aCmpd$Area, Concentration=calc, row.names=NULL, stringsAsFactors=FALSE)
        quantifiedList <- rbind(quantifiedList, tmp.concTable) 
        # quantifiedList <- rbind(quantifiedList, cbind(aCmpd[,c(1,2)], Concentration=calc) )
    }
    if (DEBUG) {
      cat("\n\n## quantifiedList:\n"); print(quantifiedList)
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
        conc <- as.character(quantified.result[which(quantified.result$hmdbID == hmdbID), "Concentration"])

        # cat("\n\n## conc in genFinalReport:", conc, " hmdbid:", hmdbID,"\n")
        # print(quantified.result)
        
        if (is.na(conc)) {
            final.comp[1,"Concentration"] <- NA
        } else if (conc == "ISTD") {
            final.comp[1,"Concentration"] <- conc
        } else if (is.character(conc)) {
            final.comp[1,"Concentration"] <- conc # "< LOQ value"
        } else {
            final.comp[1,"Concentration"] <- as.double(conc)
        }  
        # print(quantified.result)
            
        profiled_final <- rbind(profiled_final, final.comp)
    }  

    if ( length(profiled_final[which(profiled_final$Concentration == -999), "Concentration"]) > 0 ) {
        profiled_final[which(profiled_final$Concentration == -999), "Concentration"] <- "MP" # Multiple Peaks
    }

    profiled_final <- profiled_final[order(as.numeric(as.character(profiled_final$RT))), ]

    return(profiled_final)
}


# check whether the internal standard is exist or not
existInternalStd <- function(internalStd, profiledPeakSet, libDB)
{
   # cat("profiledPeakSet:\n"); print(profiledPeakSet[,c(1:5)]);
   # cat("libDB:\n"); print(libDB[,c(1:5)]);
   if ( internalStd == "NONE" ) {
      return (FALSE)
   }
  
   if( DEBUG ) {
      cat("#\n\nNOTE: If there is an error (argument is of length zero)\n")
      cat("\t >> Solution: ISTD - add (ISTD) in compound name in library\n\n");
      cat("\t # internal std:", internalStd, "\n")
      cat("#profiledPeakSet$CompoundWithTMS:\n")
      print(profiledPeakSet$CompoundWithTMS)
   }
  
   if( ! (internalStd %in% profiledPeakSet$CompoundWithTMS) ) {
      # showErrMessage("## Warning:\n\t Can not find the internal standard (",internalStd,") in Sample Spectrum.\n")
      amsg <- paste("## Error:\n\t Cannot find the selected internal standard (",internalStd,") in the Sample Spectrum.\n")
      stopMessage(amsg)
   }
  
   if (FALSE)  {
       tryCatch({
           if( ! (internalStd %in% profiledPeakSet$CompoundWithTMS) ) {
             #showErrMessage("## Warning:\n\t Can not find the internal standard (",internalStd,") in Sample Spectrum.\n\t Program will report without quantification.\n\n")
             # showErrMessage("## Warning:\n\t Can not find the internal standard (",internalStd,") in Sample Spectrum.\n")
             amsg <- paste("## Error:\n\t Cannot find the selected internal standard (",internalStd,") in the Sample Spectrum.\n")
             stopMessage(amsg)
             # stop("\n\n## Warning: Can not find the internal standard (",internalStd,") in Sample Spectrum.\n\t Program will report without quantification.\n\n")
             # return (FALSE)
           }
           if( ! (internalStd %in% as.character(libDB$CompoundWithTMS)) ) {
             # showErrMessage("\n\n## Warning: Can not find the internal standard (",internalStd,") in the library database.\n\t Program will report without quantification.\n\n")
             # showErrMessage("\n\n## Warning: Can not find the internal standard (",internalStd,") in the library database.\n")
             amsg <-paste("\n\n## Warning:\n\t Cannot find the selected internal standard (",internalStd,") in the library database.\n")
             cat(amsg)
             cat(file=File.ErrorLog, amsg, append=TRUE)
             # stop("\n\n## Warning: Can not find the internal standard (",internalStd,") in the library database.\n\t Program will report without quantification.\n\n")     
             # return (FALSE)
           }      
         }, error = function(e) {
            print(e)
            stopMessage(e)
            # stop("## The selected Internal Standard cannot find in the selected library. \n\t Please select a Biofluid type and an Internal Standard correctly.")
         }
       )
   }
  
   return (TRUE)   
}

check.Concentration<-function(conc) 
{
    for (i in 1:length(conc)) {
        cat(i, "conc[i]:", conc[i], "\n") 
        if(!is.na(conc[i]) & conc[i] != "ISTD" & conc[i] != "NA" & conc[i] == 0) {
            # cat(i, "conc[i]:", conc[i], "--> <LOD\n") 
            conc[i] <- '< LOD' 
        } 
    }

    return (conc)
}

###################################################
## The End of the File
###################################################

