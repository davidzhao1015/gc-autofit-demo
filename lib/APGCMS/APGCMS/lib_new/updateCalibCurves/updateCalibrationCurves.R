##################################################################################################
## Generating Calibration Curve
## unit: uM 
##################################################################################################

rm(list=ls())
setwd('/Users/beomsoo/gcmsProfiling/gc-autofit/lib/APGCMS/APGCMS/lib_new/updateCalibCurves/')
# library(gdata)

## using profiled data - ratio of Target Ion's EIC peak area

  # set variables: one ISTD or two ISTD
  wkdir <- "./"
  infile.origin <- paste(wkdir,"lib_urine_CalibrationCurve_20160317.csv", sep="")  
  infile.new <- paste(wkdir, "CalibrationCurves_OrganicAcids_twoISTD_Mar1716.csv", sep="")  
  
  ## original lib 
  d.origin <- read.table(infile.origin, header=TRUE, sep=",", quote="\"", skip=0)
  names(d.origin)
  head(d.origin); nrow(d.origin); ncol(d.origin)

  # new lib
  d.new <- read.table(infile.new, header=TRUE, sep=",", quote="\"", skip=0)
  names(d.new)
  print(d.new); nrow(d.new); ncol(d.new)
  
  # combine conc with area  
  names(d.origin)
  names(d.new)
  head(dt.origin)
  head(d.new)
  d.new.c <- d.new[which(d.new$InternalSTD == "Cholesterol"), ]
  names(d.new.c)[2] <- "HMDB_ID"

  unique(d.origin$HMDB_ID)
  unique(d.new.c$HMDB_ID)
  d.new.c$HMDB_ID

  dm <- merge(d.origin, d.new.c, by=c('HMDB_ID'), all=TRUE)
  head(dm)
  write.csv(dm, file="merged_calibrationCurve_Cholesterol.csv", row.names=FALSE)
  
  
  
  # assign seqno (using in reporting)
  # set variables: one ISTD or two ISTD
  wkdir <- "./"
  infile.a <- paste(wkdir,"merged_calibrationCurve_Cholesterol.csv", sep="")  
  infile.b <- paste(wkdir, "lib_urine_20160316.csv", sep="")  
  
  ## original lib 
  d.a <- read.table(infile.a, header=TRUE, sep=",", quote="\"", skip=0)
  names(d.a)
  head(d.a); nrow(d.a); ncol(d.a)
  
  # new lib
  d.b <- read.table(infile.b, header=TRUE, sep=",", quote="\"", skip=0)
  names(d.b)
  d.b <- d.b[,c(1:4)]
  print(d.b); nrow(d.b); ncol(d.b)
  
  # combine conc with area  
  names(d.a)
  names(d.b)
  head(d.a)
  head(d.b)

  unique(d.origin$HMDB_ID)
  unique(d.new.c$HMDB_ID)
  d.new.c$HMDB_ID
  
  dm <- merge(d.a, d.b, by=c('HMDB_ID', "Compound"))
  head(dm)
  write.csv(dm, file="merged_calibrationCurve_Cholesterol_withSeqIndex.csv", row.names=FALSE)


  
  ######################################################
  ## unique M/Z detection
  
  rm(list=ls())
  setwd('/Users/beomsoo/gcmsProfiling/gc-autofit/lib/APGCMS/APGCMS/lib_new/updateCalibCurves/')
  # library(gdata)
  
  ## using profiled data - ratio of Target Ion's EIC peak area
  
  # set variables: one ISTD or two ISTD
  wkdir <- "./"
  infile <- paste(wkdir,"lib_serum_20160316.csv", sep="")  

  ## original lib 
  d.origin <- read.table(infile, header=TRUE, sep=",", quote="\"", skip=0)
  names(d.origin)
  head(d.origin); nrow(d.origin); ncol(d.origin)
  
  dw <- d.origin
  
  d1 <- dw[which(dw$HMDB_ID =="HMDB00883"),][1,]
  d2 <- dw[which(dw$HMDB_ID =="HMDB00161"),]
  d3 <- dw[which(dw$HMDB_ID =="HMDB00123"),][1,]
  
  d.mz1 <- as.numeric(unlist(strsplit(as.character(d1$MZ), split=" ")))
  d.mz2 <- as.numeric(unlist(strsplit(as.character(d2$MZ), split=" ")))
  d.mz3 <- as.numeric(unlist(strsplit(as.character(d3$MZ), split=" ")))
  
  intersect(setdiff(d.mz2, d.mz1), setdiff(d.mz2, d.mz3))
  intersect(intersect(d.mz1, d.mz2), d.mz3)
  
  d.int2 <- as.numeric(unlist(strsplit(as.character(d2$Intensity), split=" ")))
  plot(d.mz2, d.int2, type="h")
  
  
  
  
  