library(dplyr)
library(sf)
library(tidyr)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(trend)
library(foreign)
library(dataRetrieval)

## REMOVE ALL EXTRANEOUS LINES !!!

### Process

## basins only
data = read.dbf("../data/gagesIInarrowNFWeco500.dbf")
gages = unique(gages$GAGE_ID)

#basinPlot = ggplot(data = camels)+
#  geom_sf(aes(fill = elev_mean))
#basinPlot


## For each gage, get the discharge

## some functions for the plots
theme_dataRetrial_talk <- function(base_family = "serif",
                                   size = 25,
                                   ...){
  theme_bw(base_family = base_family, ...) +
    theme(
      plot.title = element_text(size = size),
      text = element_text(size = size),
      axis.text = element_text(size = size),
      plot.margin = unit(c(0.5,0.5,0.5,1), "cm"),
      legend.background = element_rect(color = "black", 
                                       fill = "transparent")
    )
}

dataRetrieval_timeseries_framework <- function(font_size = 25, point_size = 4){
  update_geom_defaults("point",
                       list(size = point_size,
                            fill = "darkgrey",
                            color = "darkgrey"))
  
  update_geom_defaults("line",
                       list(linewidth = 1))
  
  list_out <- list(theme_dataRetrial_talk(size = font_size))
  return(c(list_out))
}

unescape_html <- function(str){
  fancy_chars <- regmatches(str, gregexpr("&#\\d{3};",str))
  
  unescaped <- xml2::xml_text(xml2::read_html(paste0("<x>", fancy_chars, "</x>")))
  
  fancy_chars <- gsub(pattern = "&#\\d{3};",
                      replacement = unescaped, x = str)
  
  fancy_chars <- gsub("Â","", fancy_chars)
  return(fancy_chars)
}

wrap_text <- function(x, width = 40, collapse = "\n"){
  new_text <- paste(strwrap(x, 
                            width = width),
                    collapse = collapse)
  return(new_text)
}

# test gage 
testGage = gages[[1]]

#dv <- read_waterdata_daily(monitoring_location_id = testGage$monitoring_location_id,
#dv <- read_waterdata_daily(monitoring_location_id = paste0("USGS-", testGage),
dv <- read_waterdata_daily(monitoring_location_id = paste0("USGS-", testGage),
                           #dv <- read_waterdata_daily(monitoring_location_id = "10039500",
                           parameter_code = "00060",
                           #statistic_id = "00003",
                           time = c("2016-01-01", "2026-01-01"))
## temperature
dvT <- read_waterdata_daily(monitoring_location_id = paste0("USGS-", testGage),
                           #dv <- read_waterdata_daily(monitoring_location_id = "10039500",
                           parameter_code = "00010",
                           #statistic_id = "00003",
                           time = c("2016-01-01", "2026-01-01")) ## no temp data


ggplot(data = dv) +
#ggplot(data = dvT) +
  #ggplot(data = uv) +
  dataRetrieval_timeseries_framework() +
  geom_line(aes(
    x = time, 
    y = value)) +
  xlab("Date") +
  labs(title = "TEST GAGE",
  #labs(title = testGage$monitoring_location_name,
       caption = paste("Data pulled on:", 
                       #as.Date(attr(dv, "queryTime")))) +
                       as.Date(attr(dvT, "queryTime")))) +
  ylab(unescape_html(attr(dv, "variableInfo")$variableName))

## NEED TO GET THESE FROM 2016-2025 FOR ALL


gagesStr8 = ifelse(nchar(gagesStr) == 7, paste0("USGS-0", gagesStr), paste0("USGS-", gagesStr))

## function to apply to each site to save a .csv for each
nwis_Qts = function(site){ 
  dv = read_waterdata_daily(monitoring_location_id = paste0("USGS-", site),
                            parameter_code = "00060", ## discharge
                            #statistic_id = "00003",
                            time = c("2016-01-01", "2026-01-01"))
  write.csv(dv, file = paste0("../output/q/", site, ".csv")) 
}


## list the completed files and rm from the list
list.files("../output/q/")
remain = gages[194:length(gages)] ## MODIFY!

#lapply(gages, nwis_Qts) ## NEED TO RESUME - threw a "Too Many Requests" error 
lapply(remain, nwis_Qts) ## NEED TO RESUME - threw a "Too Many Requests" error 

## list the completed files and rm from the list

