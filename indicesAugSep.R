library(dplyr)
library(sf)
library(tidyr)
library(ggplot2)
library(lubridate)

### Function to calculate each of the indices for describing hydrological

get_augsep = function(qfile){
  ## Load file
  qData = read.csv(paste0("./output/q/", qfile)) 
  ## fix wonky headings
  hdngs = names(qData)
  colnames(qData) = hdngs[2:length(hdngs)]
  
  ## create month col
  qData$month = month(qData$time)
  ## create year col
  qData$year = year(qData$time)
  
  gageID = as.numeric(strsplit(qData[1,]$monitoring_location_id, "-")[[1]][2])
  
  ## Calculate the proportion of flows in august, september, and sum of both
  
  q = qData$value
  indx = seq(2,length(q))
  
  ## aug
  aug = qData %>%
    select(c(time, value, month, year)) %>%
    filter(year < 2026) %>%
    filter(month == 8) %>%
    group_by(year) %>%
    arrange(value, .by_group = T) %>%
    summarise(sumAug = sum(value, na.rm = T))
  
  sep = qData %>%
    select(c(time, value, month, year)) %>%
    filter(year < 2026) %>%
    filter(month == 9) %>%
    group_by(year) %>%
    arrange(value, .by_group = T) %>%
    summarise(sumSep = sum(value, na.rm = T))
  
  augSep = qData %>%
    select(c(time, value, month, year)) %>%
    filter(year < 2026) %>%
    filter(month %in% c(8,9)) %>%
    group_by(year) %>%
    arrange(value, .by_group = T) %>%
    summarise(sumAugSep = sum(value, na.rm = T))
  
  ## annual 
  ann = qData %>%
    select(c(time, value, month, year)) %>%
    filter(year < 2026) %>%
    group_by(year) %>%
    summarise(sum = sum(value, na.rm = T))
  
  augflow = mean(aug$sumAug)/mean(ann$sum)
  sepflow = mean(sep$sumSep)/mean(ann$sum)
  augSepflow = mean(augSep$sumAugSep)/mean(ann$sum)

  ## vector to return
  return(c(gageID, augflow, sepflow, augSepflow))
}

setwd('~/BSU/MRRMAid/qMetrics/')

## list files
nwis = list.files("./output/q/")
nwisComplete = c()

## Check for completeness - what does this mean?
for (f in nwis){
  length = length(read.csv(paste0("./output/q/", f))[,1])
  print(length)
  if(length > 3600){
    nwisComplete =append(nwisComplete, f)
  }
}

sigs = lapply(nwisComplete, get_augsep)
outSigs = do.call(rbind, sigs)
colnames(outSigs) = c("gageID", "augflow", "sepflow", "augSepflow")
write.csv(outSigs, "./output/indicesAugSep.csv")
