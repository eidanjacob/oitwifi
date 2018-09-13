## Reads in Data files (e.g. from box) and selects events with certain criteria
## Exports to new csv

## Change stuff here
fileNamesToReadIn <- c("20_days")
locations <- c("Perkins")
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
tableOut <- NULL

for(fileName in fileNamesToReadIn){
  # Read in data
  eventData <- read_csv(paste0("./data/", fileName,".csv"))
  # Merge with apName / location key
  eventData <- merge(eventData, apNameToLocation, by.x = "ap", by.y = "APname")
  # Filter for desired locations
  toKeep <- eventData %>%
    filter(location %in% locations)
  # Add to table
  tableOut <- rbind(tableOut, toKeep)
}

# Write new csv
write.csv(tableOut, 
          paste0(
            paste(fileNamesToReadIn, "filtered_for", locations, sep = "_", collapse = "_"),
            ".csv"
          ),
          row.names = FALSE)