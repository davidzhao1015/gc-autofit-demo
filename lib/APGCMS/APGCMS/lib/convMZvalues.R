###############################################################################################################
# Change Mass Information (m/z values) into Integer
#  50.1 -> 50
#  range: -0.3 ~ +0.7 
###############################################################################################################

toIntMZ <- function(mz.vec)
{
    mz.num <- as.numeric(unlist(strsplit(as.character(mz.vec), split=" ")))
    mz.int <- ifelse( (mz.num - floor(mz.num)) <= 0.7, floor(mz.num), ceiling(mz.num))
    paste(noquote(mz.int), collapse=" ")
}

toIntMZ(x)
x
x <- c(3.1, 3.3, 3.5, 3.6, 3.699, 3.8)

# mz <- unlist(as.character(d.in$MZ))
# length(mz)
# mz[1]

# d.in$MZ[1]

setwd("~/gcmsProfiling/gc-autofit/lib/APGCMS/APGCMS/lib/")
infile <- "lib_alkane_20150911.csv"
infile <- "lib_serum_20150911.csv"
infile <- "lib_urine_20150911.csv"
infile <- "lib_saliva_20150911.csv"
d.in <- read.csv(file=infile)
head(d.in)
names(d.in)

# toIntMZ(d.in$MZ[1])
mzlist <- sapply(d.in$MZ, toIntMZ)

d.out <- cbind(d.in, mzlist)
names(d.out)
head(d.out)

ofile <- "lib_alkane_20150911_new.csv"
ofile <- "lib_serum_20150911.csv"
ofile <- "lib_urine_20150911.csv"
ofile <- "lib_saliva_20150911.csv"
write.csv(d.out, file=ofile, row.names=FALSE)


{ ## for the reference 
      mz.value <- as.numeric(unlist(strsplit(as.character(mz[1]), split=" ")))
      summary(mz.value)
      
      mzlist <- paste(noquote(round(mzIntList[[i]]$mzInt[,"mz"],2)), collapse=",")
      
      # mz list -> one string with spaece
      peakmz <- paste(noquote(round(peak_mzInt_list[[i]]$mzInt[,"mz"],2)), collapse=" ")
      
      # mz list -> mz numeric
      ref_MZS_vec <- as.numeric(unlist(strsplit(as.character(alib.matched$MZ), split=" ")))
}