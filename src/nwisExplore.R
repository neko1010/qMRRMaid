library(dplyr)
library(sf)
library(tidyr)
library(dataRetrieval)
library(ggplot2)

## coupla links
#https://rconnect.usgs.gov/dataRetrieval_workshop/dataRetrieval_1.html#/nwis-discovery
#https://waterdata.usgs.gov/blog/beyond-basic-mapping/
#https://water.code-pages.usgs.gov/dataRetrieval/articles/read_waterdata_functions.html

#install.packages("dataRetrieval")


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

setwd('~/BSU/MRRMAid/qMetrics/')

## sage biome poly to filter gages
sage = st_read("../sageBiome/data/sagebrushBiome.shp")

## read all of the HUCs
hux = st_read('../watershedResilience/output/huc12metrics.shp')

## start with an example
test = hux[1,]
str(test)
huc = test$huc12

## Parameter Code 	Short Name
## 00060 	Discharge
## 00065 	Gage Height
## 00010 	Temperature
## 00400 	pH

## Statistic Code 	Short Name
## 00001 	Maximum
## 00002 	Minimum
## 00003 	Mean
## 00008 	Median

##whatData <- whatNWISdata(huc = huc, ##this one is decommissioned
##                          siteType = "ST")
##trimmed <- whatData |> 
##  select(site_no, 
##         data_type_cd, 
##         parm_cd,
##         stat_cd,
##         Start = begin_date,
##         End = end_date,
##         Count = count_nu)


## sites in ID?
sitesID <- read_waterdata_monitoring_location(state_name = "Idaho", 
                                               site_type = "Stream")

testGage = sitesID %>%
  filter(monitoring_location_number == "13138000") ## can find E fork big wood at Gimlet here, but only has HUC16 in the huc field... 

ggplot(data = sitesID) +
  geom_sf() +
  theme_minimal()

## active gages from here:  https://waterdata.usgs.gov/state/idaho/  - should figure another way...
activeID = as.character(c(13306370,13138000,13124265,13302500,13210810,13135520,13210986,12415135,13317000,13062500,13317660,13310199,13079300,13212890,13038500,13313000,13346800,13089500,13150430,13295000,13211205,13090500,13185000,13341050,13068500,13239000,10039500,13135500,13068495,13240000,13087995,13074400,13246000,13309220,13095500,13075910,13200000,13063000,13235000,12413131,13345000,13341570,13190500,13159800,132109867,13060000,12413500,10092700,13337095,13247500,13342450,12306500,10125500,13336500,12305000,13304050,13082500,13078000,13213000,13108150,13161930,13068300,13095175,13337000,12395000,13046995,13072400,13338950,12417650,10068500,12419000,12414500,13032500,13340000,13304700,13093383,13297350,13338500,13210824,13058510,13112000,13316500,13297355,13139510,13236500,12321500,13081500,13152500,13058520,13047600,13075000,13296000,12393501,13154500,13068501,13305310,13039500,13131000,13073000,13210045,13297380,13296500,13042500,13302005,13120500,13132100,13342500,13210831,13058530,12413355,13132373,13128900,13266000,13130300,13075500,131504301,13122000,13176400,13147900,13249500,12308000,13132500,13311000,13050500,13074810,13141500,13339500,13306385,13172500,13092747,13310850,13069500,12395500,13137300,12413860,13055250,12411000,12414900,13206000,13206305,13210980,13049500,13258500,13077000,13094000,13055340,13118700,13058529,13310700,13212549,13118975,13307000,13071010,13052200,13047500,13251000,12413125,13341140,13037500,13311250,12413000,13311450,13046000,13269000,13056500,13340600,13038000,13058000,13142500,13305000,12413470,12413130,13057155,13250000,13057300,13136550,13137500,13310800,13140800,12413210,12413875,13265500,13075983,13055000,13140335,13213100,13116500,13057132,13206400,13057000,13119000,13213072,12392155,13237920,13057940,13127000,13297330,13186000,13148500,13066000,12391950,13168500,12322000
))

## Filter for active sites
sitesID = sitesID %>%
  filter(monitoring_location_number %in% activeID)

## then for those in the list of Sage biome huc12s
sitesIDhuc12 = sitesID %>%
  filter(hydrologic_unit_code %in% hux$huc12)

length(unique(sitesIDhuc12$hydrologic_unit_code)) ##126 of 147 unique - some duplicates. 
sum(is.na(sitesIDhuc12$drainage_area)) ## 129 have valid drainage areas...


## How can I figure out which gages are at basin outlets? Compare drainage area to basin area? Maybe these are not always measured identically...


## sanity plot
ggplot(data = sitesIDhuc12) +
  geom_sf() +
  theme_minimal()

## test gage 
testGage = sitesIDhuc12[1,]

dv <- read_waterdata_daily(monitoring_location_id = testGage$monitoring_location_id,
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
  #labs(title = attr(dv, "siteInfo")$station_nm,
  labs(title = testGage$monitoring_location_name,
       caption = paste("Data pulled on:", 
                       as.Date(attr(dv, "queryTime")))) +
  ylab(unescape_html(attr(dv, "variableInfo")$variableName))

## REMOVE MAJOR RIVERS - SNAKE

## REMOVE watersheds with dams -HOW?

## NEED TO GET THESE FROM 2016-2025 FOR ALL

## function to apply to each site to save a .csv for each
nwis_Qts = function(site){
  siteData = sitesIDhuc12 %>% 
    filter(monitoring_location_id == site) 
  dv = read_waterdata_daily(monitoring_location_id = site$monitoring_location_id,
                             parameter_code = "00060", ## discharge
                             #statistic_id = "00003",
                             time = c("2016-01-01", "2026-01-01"))
  write.csv(dv, file = paste0("./", site, ".csv")) ## is this what I want or should I make a big ugly dataframe?
}
#huc_gages <- whatNWISdata(huc = huc , parameterCd = "00060", service="uv")
#head(huc_gages)


## write it
st_write(huxROI, "./pahLem/upperSalmon.shp")
