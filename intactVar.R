library(dplyr)
library(sf)
library(tidyr)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(corrplot)

setwd("BSU/MRRMAid/qMetrics/")

## Calculate intactness and variability for each
mrrmaid = read.csv("./data/camelsnarrowNFWeco500.csv")

## Pivot wider
mrrmaidWide = mrrmaid %>% pivot_wider(id_cols = c('gauge_d'), ## 18,861 obs
                                    names_from = 'date', values_from = 'mesicProp')


## If June mean is lower than July mean, rm June from the SD calculation

junes = mrrmaidWide %>%
  select(ends_with((c("06")))) 

juneMean = rowMeans(junes)

julys = mrrmaidWide %>%
  select(ends_with((c("07")))) 

julyMean = rowMeans(julys)

mrrmaidNoJune = mrrmaidWide %>%
  select(-ends_with(c("06", "09"))) ## remove September here too - these are energy limited system

## add intactness columns - august if energy limited, september otherwise

mrrmaidWide = cbind(mrrmaidWide, juneMean, julyMean) %>%
  mutate(SD = if_else(julyMean>juneMean,
                      apply(mrrmaidWide[,3:length(colnames(mrrmaidNoJune))], 1, sd),
                      apply(mrrmaidWide[,3:length(colnames(mrrmaid))], 1, sd))) %>%
  mutate(intact = if_else(julyMean>juneMean,
                          apply(mrrmaidWide[,3:length(colnames(mrrmaidNoJune))], 1, mean),
                          apply(mrrmaidWide[,3:length(colnames(mrrmaid))], 1, mean))) %>%
  mutate(CV = SD/intact)
## collate other covariates
camels = left_join(mrrmaidWide, mrrmaid%>%select(-c(mesicProp, date, system.index)), by = "gauge_d")
camels = unique(camels)

## outcomes
sigs = read.csv("./output/indices.csv")
sigs = sigs %>%
  rename(gauge_d = gageID)

camelSigs = left_join(sigs, camels, by = "gauge_d")

##explore and group using ecoregions
ecoLookup = c('Northwestern Glaciated Plains', 'Middle Rockies', 'Idaho Batholith',
              'Northwestern Great Plains', 'Snake River Plain', 'Eastern Cascades Slopes and Foothills', 
              'Columbia Plateau', 'Cascades','Sierra Nevada', 'Southern Rockies', 'Central Basin and Range', 
              'Mojave Basin and Range', 'Arizona/New Mexico Mountains', 'Arizona/New Mexico Plateau',
              'Wasatch and Uinta Mountains', 'Northern Basin and Range', 'Blue Mountains',
              'Colorado Plateaus', 'Wyoming Basin','High Plains')

ecoCode = c(42, 17, 16, 43, 12, 9, 10, 4, 5, 21, 13, 14, 23, 22, 19, 80, 11, 20, 18, 25)



dfEco = cbind.data.frame(ecoCode, ecoLookup)
colnames(dfEco) = c('min', 'ecoregion')

camelSigs = left_join(camelSigs, dfEco, by = "min")

vars = c("intact","CV", "AREA","Perimtr","aridity", "elev_mn","frc_snw",
         "hgh_prc_d","hgh_prc_f","lat_cen" ,"lon_cen","lw_prc_d","lw_prc_f",
         "p_mean","p_ssnlt","pet_men","flashiness" ,"flashinessWet",
         "max30area","q10q95area","dryMonthArea","baseflow")

camelSigsCorVars = camelSigs%>% select(all_of(vars))
camelSigsCor = cor(camelSigsCorVars,use="pairwise.complete.obs")
corrplot(camelSigsCor) ## not a lot to see here

## Intactness
flashIntact = ggplot(camelSigs, aes(x = intact, y = flashiness, color = ecoregion))+
  geom_point()+
  geom_smooth(method = lm)

flashIntact

baseIntact = ggplot(camelSigs, aes(x = intact, y = baseflow, color = ecoregion))+
  geom_point()+
  geom_smooth(method = lm)

baseIntact

dryAreaIntact = ggplot(camelSigs, aes(x = intact, y = dryMonthArea, color = ecoregion))+
  geom_point()+
  geom_smooth(method = lm)

dryAreaIntact


## Variability
flashCV = ggplot(camelSigs, aes(x = CV, y = flashiness, color = ecoregion))+
  geom_point()+
  geom_smooth(method = lm)

flashCV

baseCV = ggplot(camelSigs, aes(x = CV, y = baseflow, color = ecoregion))+
  geom_point()+
  geom_smooth(method = lm)

baseCV

dryAreaCV = ggplot(camelSigs, aes(x = CV, y = dryMonthArea, color = ecoregion))+
  geom_point()+
  geom_smooth(method = lm)

dryAreaCV

##
intactCV = ggplot(camelSigs, aes(x = intact, y = CV, color = ecoregion))+
  geom_point()+
  geom_smooth(method = lm)

intactCV

## six panel
ggarrange(flashIntact, flashCV, baseIntact, baseCV, dryAreaIntact, dryAreaCV, ncol = 2, nrow=3)

## map the units
camelsSHP = st_read("./output/sageCAMELS.shp")
## just the id and geom

camelsSHP = camelsSHP %>%
  select(hru_id, geometry) %>%
  rename(gauge_d = hru_id)

metricsSHP = left_join(camelsSHP,camelSigs,  by = "gauge_d")
metricsSHP = metricsSHP %>%
  filter(!is.na(ecoregion))

states = st_read("../watershedResilience/data/tl_2024_us_state.shp")
sage = st_read("../watershedResilience/data/sagebrushBiome.shp")

## transform the sage biome to states
sageNAD83 = st_transform(sage, crs = st_crs(states))
metricsNAD83 = st_transform(metricsSHP, crs = st_crs(states))
sageStates <- st_filter(states, sageNAD83, .predicate = st_intersects)


## map
ggPoly = ggplot(metricsNAD83) +
  geom_sf(aes(fill = ecoregion))+
  geom_sf(data = sageStates, color = "black", fill = NA, lwd = 1)+
  labs(x = "Longitude", y = "Latitude")
ggPoly

## Histogram
ggHist = ggplot(metricsNAD83, aes(x = ecoregion)) +
  geom_bar() +
  labs( x = "Ecoregion", y = "Frequency")+
  theme(axis.text.x = element_text(angle = 65, hjust = 1))+
  stat_count(binwidth = 1, 
             geom = 'text', 
             color = 'magenta', 
             aes(label = after_stat(count)),
             position = position_stack(vjust = 1))

ggHist
