
options(install.packages.compile.from.source = "always")

# install.packages('Cairo')
# install.packages('gdata')
install.packages(c("Cairo","gdata"), type = "both")
if (!requireNamespace("BiocManager", quietly = TRUE))
    # install.packages("BiocManager")
    install.packages(c("BiocManager"), type = "both")

BiocManager::install("xcms")