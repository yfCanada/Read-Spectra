# Set to the working directory; eg: setwd("~/Documents/Read-Spectra/NIR spectra")
# Spectral data taken from Mightex spectrometer; 5 spectra taken from each sample. In this example, there are two samples; total 10 spectra
# Goal: read spectral data from the files and plot spectra colored by "sample number"

library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)

# A function read specific file saved from Mightex spectrometerand extract one column specified by ColNo (default is 2)
readData <- function(x, ColNo=12){
  temp <- read.csv(x, skip = 20, nrows = 3600)
  return(temp[,c(ColNo)])
}

# read file names in the directory
filenames <- list.files(pattern=".csv", full.names = FALSE)

# read first Column that represents Wavelength (nm)
WL <-  readData(filenames[1],ColNo =2) 
# Loop over files and extract 2nd column(reflectance spectrum) in each file and and cbind data together 
RefData <- data.frame(WL)
for(y in filenames){
  a <- readData(y)
  RefData <- cbind(RefData,a)
}

# give Refdata column names
colnames(RefData)<- c("WL", filenames)

# Tidy Refdata data frame by "gather"
Spectral_data <- gather(RefData, file, Reflectance, -WL)

# Add one column that represents  sample number; in this example, only two sample numbers
Spectra <- Spectral_data %>% mutate(sample = sub(pattern="(.*)_.*","\\1", file))

# Make teh sample number column into "factor" so they are easily color coded when calling qplot 
Spectra$sample <- as.factor(Spectra$sample)

# Plot the 10 spectra, color coded by sample number (or by file)
qplot(Spectra$WL, Spectra$Reflectance, data = Spectra, geom = "line", color = sample , xlim = c(430, 900),ylim =c (0,0.5),xlab = "Wavelength (nm)", ylab = "Reflectance (%)", main = "Sample# 1535788: N=2 \n Sample# 1535921: N=19")
