library(dplyr)
library(sf)
library(tidyr)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(trend)
library(dataRetrieval)

## REMOVE ALL EXTRANEOUS LINES !!!

### Process
setwd('~/BSU/MRRMAid/qMetrics/')

## basins only
camels = st_read("./data/HCDN_nhru_final_671.shp")

#basinPlot = ggplot(data = camels)+
#  geom_sf(aes(fill = elev_mean))
#basinPlot

## attributes
attr = read.table("./data/camels_clim.txt", header = T, sep = ";")

## add a common property bc the ids are not named the same 
attr = attr %>%
  mutate(hru_id = gauge_id)
##Join them
camelsAttr = left_join(camels, attr, by = "hru_id")

#st_write(camelsAttr, "./output/camels.shp")


## Sagebrush biome ############
sage = st_read("../watershedResilience/data/sagebrushBiome.shp")

sageTransform = st_transform(sage, crs = st_crs(camels))

camelsValid = st_make_valid(camelsAttr)

## filter the camels
sageCamels <- camelsValid %>%
  st_filter(y = sageTransform, .predicate = st_within)## 61 hucs
  #st_filter(y = sageTransform, .predicate = st_intersects)## 73

#st_write(sageCamels, "./output/sageCAMELS.shp")

basinPlot = ggplot(data = sageCamels)+
  geom_sf(aes(fill = low_prec_timing))+
  geom_sf(data = sage, aes(color = "black"), fill = NA)+
  scale_color_manual(name = "", values = c( "black"),  labels = c("Sagebrush biome"))
basinPlot

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

gages = sageCamels$gauge_id

# test gage 
testGage = gages[1]

#dv <- read_waterdata_daily(monitoring_location_id = testGage$monitoring_location_id,
#dv <- read_waterdata_daily(monitoring_location_id = paste0("USGS-", testGage),
dv <- read_waterdata_daily(monitoring_location_id = "USGS-06037500",
                           #dv <- read_waterdata_daily(monitoring_location_id = "10039500",
                           parameter_code = "00060",
                           #statistic_id = "00003",
                           time = c("2016-01-01", "2026-01-01"))

ggplot(data = dv) +
  #ggplot(data = uv) +
  dataRetrieval_timeseries_framework() +
  geom_line(aes(
    x = time, 
    y = value)) +
  xlab("Date") +
  labs(title = "TEST GAGE",
  #labs(title = testGage$monitoring_location_name,
       caption = paste("Data pulled on:", 
                       as.Date(attr(dv, "queryTime")))) +
  ylab(unescape_html(attr(dv, "variableInfo")$variableName))

## NEED TO GET THESE FROM 2016-2025 FOR ALL

## change gage ints to strings (8 digits)
gagesStr = as.character(gages)
gagesStr8 = ifelse(nchar(gagesStr) == 7, paste0("USGS-0", gagesStr), paste0("USGS-", gagesStr))

## function to apply to each site to save a .csv for each
nwis_Qts = function(site){ 
  dv = read_waterdata_daily(monitoring_location_id = site,
                            parameter_code = "00060", ## discharge
                            #statistic_id = "00003",
                            time = c("2016-01-01", "2026-01-01"))
  write.csv(dv, file = paste0("./output/q/", site, ".csv")) ## is this what I want or should I make a big ugly dataframe?
}

nwis_Qts(gagesStr8[1])
lapply(gagesStr8, nwis_Qts)
