## Reads in Data files (e.g. from box) and selects events with certain criteria
## Exports to new csv

## Change stuff here
fileNamesToReadIn <- c("20_days")
locations <- "Perkins"
##

# Install Packages
if(!require(tidyverse)){
  install.packages("tidyverse")
  library(dplyr)
}

if(!require(readr)){
  install.packages("readr")
  library(readr)
}

apNameToLocation <- read_csv("./data/apNameToLocation.csv")

for(fileName in fileNamesToReadIn){
  # Read in data
  eventData <- read_csv(paste0("./data/", fileName,".csv"))
}