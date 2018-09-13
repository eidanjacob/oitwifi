## Reads in Data files (e.g. from box) and selects events with certain criteria
## Exports to new csv

if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}

if(!require(readr)){
  install.packages("readr")
  library(readr)
}