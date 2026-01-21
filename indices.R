library(dplyr)
library(sf)
library(tidyr)
library(ggplot2)
library(lubridate)

### Function to calculate each of the indices for describing hydrological
### signatures of catchments found in
### Vanderhoof et al. 2025 https://doi.org/10.1007/s11273-025-10066-z 

get_indices = function(qfile){
  ## Load file
  qData = read.csv(paste0("./output/q/", qfile)) 
  ## fix wonky headings
  hdngs = names(qData)
  colnames(qData) = hdngs[2:length(hdngs)]
  
  ## Calculate the indices used in Vanderhoof et al. 2025 https://doi.org/10.1007/s11273-025-10066-z
  
  ## ALL FLOWS
  ## Flashiness - The sum of the absolute value of the changes in discharge from 
  ## the day prior to the current day (discharge t2—discharge t1)  
  ## divided by the sum of the daily discharge values
  
  q = qData$value
  indx = seq(2,length(q))
  
  t2mint1 = function(i){
    return(abs(q[i] - q[i-1]))
  }
  
  absVal = sapply(indx, t2mint1)
  flashiness = sum(absVal, na.rm = T)/sum(q, na.rm = T)
  
  ## HIGH FLOWS
  ## Flashiness wet season - same as above but constrained to 3 months w highest Q
  
  ## create month col
  qData$month = month(qData$time)
  ## monthly means
  monthMeans = qData %>%
    select(c(monitoring_location_id, time, value, month)) %>%
    group_by(month) %>%
    summarise(avg = mean(value))
  
  wetMonths = sort_by(monthMeans, monthMeans$avg, decreasing = T)$month[1:3]
  
  ## wet months data frame
  wetMonthsDF = qData %>%
    select(c(monitoring_location_id, time, value, month)) %>%
    filter(month %in% wetMonths)
  
  ## calculate the index
  qWet = wetMonthsDF$value
  indxWet = seq(2,length(wetMonthsDF))
  
  t2mint1Wet = function(i){
    return(abs(qWet[i] - qWet[i-1]))
  }
  
  absValWet = sapply(indxWet, t2mint1Wet)
  flashinessWet = sum(absValWet)/sum(qWet)
  
  ## MAX30/area The flow rate for the 30 days per year with the highest flow rate, 
  ## summed over the 30 days, and averaged per year, divided by the watershed area
  
  ## Get area and other huc info
  hucs = st_read("./output/sageCAMELS.shp")
  hucs
  
  gageID = as.numeric(strsplit(qData[1,]$monitoring_location_id, "-")[[1]][2])
  
  hucInfo = hucs %>%
    filter(hru_id == gageID)
  
  ## get 30 days for each year w highest Q, sum them, average them
  ## create year col
  qData$year = year(qData$time)
  ## annual highest 30 days
  ann30 = qData %>%
    select(c(time, value, month, year)) %>%
    group_by(year) %>%
    arrange(desc(value), .by_group = T) %>%
    summarise(sum30 = sum(value[1:30]))
  
  max30 = mean(ann30$sum30, na.rm = T)
  
  max30area = max30/(hucInfo$AREA) ## need to change units? Currently cfs/m**2
  
  ## (Q10-Q95)/area Discharge exceeded 10% of the time (Q10) minus discharge exceeded 
  ## 95% of the time (Q95), divided by watershed area
  
  q10 = qData %>%
    select(c(time, value, month, year)) %>%
    group_by(year) %>%
    filter(value > quantile(value, 0.90, na.rm = T)) %>%
    summarize(sum10 = sum(value, na.rm = T))
  
  q95 = qData %>%
    select(c(time, value, month, year)) %>%
    group_by(year) %>%
    filter(value < quantile(value, 0.05, na.rm = T)) %>%
    summarize(sum95 = sum(value, na.rm = T))
  
  q10q95area = mean((q10$sum10-q95$sum95)/(hucInfo$AREA), na.rm = T) ## need to change units? Currently cfs/m**3
  
  ## LOW FLOWS 
  ## DryMonth/area - Average annual discharge in the driest month (excluding snow cover months) divided by watershed area
  
  ## driest month
  noSnow = monthMeans %>%
    filter(month %in% seq(4,10)) %>%## snow free
    arrange(avg)
  
  dryMonthArea = noSnow$avg[1]/hucInfo$AREA  ## need to change units? Currently cfs/m**2
  
  ## Baseflow index - The ratio of the average daily flow during the lowest annual
  ## 7-day flow (excluding snow cover conditions) to the annual average daily flow
  
  ## annual lowest 7 days
  annBase = qData %>%
    select(c(time, value, month, year)) %>%
    filter(year < 2026) %>%
    filter(month %in% seq(4,10)) %>%## snow free
    group_by(year) %>%
    arrange(value, .by_group = T) %>%
    summarise(mean7 = mean(value[1:7], na.rm = T))
  
  ## annual 
  ann = qData %>%
    select(c(time, value, month, year)) %>%
    filter(year < 2026) %>%
    group_by(year) %>%
    summarise(mean = mean(value, na.rm = T))
  
  baseflow = mean(annBase$mean7)/mean(ann$mean)
  
  ## vector to return
  return(c(gageID, flashiness, flashinessWet, max30area, q10q95area, dryMonthArea, baseflow))
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

sigs = lapply(nwisComplete, get_indices)
outSigs = do.call(rbind, sigs)
colnames(outSigs) = c("gageID", "flashiness", "flashinessWet", "max30area", "q10q95area", "dryMonthArea", "baseflow")
write.csv(outSigs, "./output/indices.csv")
