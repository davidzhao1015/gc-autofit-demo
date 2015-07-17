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


RProgram_dir <- "~/gcmsProfiling/APGCMS_developing"; setwd(RProgram_dir); getwd()
lib_dir <- file.path(RProgram_dir, "lib")

## now in the library, it includes RI, mz, intensity, 
## as well as slope and intercept for quantification (calibration curve)
libfunc.file <- "apgcms_funclib.R"
LibFile <- c("lib_serum_20140619.csv","lib_organicAcids_20140630.csv","libPeakID_saliva_20140501.csv","lib_milk_20140722.csv") 

## sample type and library 
SERUM <- 1
ORGANIC_ACID <- 2
SALIVA <- 3
MILK <- 4 

SampleType <- 3
setwd("/Users/beomsoo/gcmsProfiling/APGCMS_developing/IntensityPlots/Saliva/")
setwd("/Users/beomsoo/gcmsProfiling/APGCMS_developing/IntensityPlots")
setwd("/Users/beomsoo/gcmsProfiling/APGCMS_developing/lib")

## load Compound Name RI, MZ and Intensity library 
lib.peak <- read.csv(file=file.path(lib_dir, LibFile[SampleType]), head=TRUE, sep=",", quote = "\"");
lib.peak <- read.csv(file=file.path(lib_dir, "lib_alkane_20140807.csv"), head=TRUE, sep=",", quote = "\"");
names(lib.peak); nrow(lib.peak)

lib.peak[1,]
lib.peak$MZ
lib.peak$Intensity


cat("Compound,MZ20,Int20\n", file="Top20_MZInt.csv")
for ( i in 1:nrow(lib.peak)) {  
    ref_MZS_vec <- as.numeric(unlist(strsplit(as.character(lib.peak[i,"MZ"]), split=" ")))
    ref_INT_vec <- as.numeric(unlist(strsplit(as.character(lib.peak[i, "Intensity"]), split=" ")))                
    
    dat.lib <- as.data.frame(cbind(MZ=ref_MZS_vec, INT=ref_INT_vec))
    dat.lib <- dat.lib[order(dat.lib$INT, decreasing=TRUE),]
    
    # dat.lib <- dat.lib[order(-dat.lib$INT),]
    dat.lib <- dat.lib[1:20, ]
    # print(dat.lib)
    
    # library format
    dat.lib <- dat.lib[order(dat.lib$MZ), ]
    mzlist <- paste(noquote(dat.lib$MZ), collapse=" ")
    intlist <- paste(dat.lib$INT, collapse=" ")
    
    dat.lib <- cbind(Compound=as.character(lib.peak[i,"Compound"]), mzlist, intlist)
    write.table(dat.lib, file="Top20_MZInt.csv", sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
    
    idx <- c(1:nrow(dat.lib))
    dat.lib <- cbind(idx, dat.lib)
    
    # png(filename = paste("Plot_Intensity_", lib.peak[i,"Compound"],".png", sep=''), width = 1000, height = 800, units = "px", pointsize = 10)
    # plot(dat.lib$idx, dat.lib$INT, main=lib.peak[i,"Compound"])
    # dev.off()  
}

