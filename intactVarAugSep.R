library(dplyr)
library(sf)
library(tidyr)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(corrplot)

#setwd("BSU/MRRMAid/qMetrics/")

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
                      apply(mrrmaidWide[,3:length(colnames(mrrmaidNoJune))], 1, sd, na.rm = TRUE),
                      apply(mrrmaidWide[,3:length(colnames(mrrmaid))], 1, sd, na.rm = TRUE))) %>%
  mutate(meanTS = if_else(julyMean>juneMean,
                          apply(mrrmaidWide[,3:length(colnames(mrrmaidNoJune))], 1, mean, na.rm = TRUE),
                          apply(mrrmaidWide[,3:length(colnames(mrrmaid))], 1, mean, na.rm = TRUE))) %>%
  mutate(CV = SD/meanTS) %>% 
  mutate(intact = if_else(julyMean>juneMean,
                              apply(mrrmaidWide %>%select(ends_with("08")), 1, mean, na.rm = TRUE),
                              apply(mrrmaidWide %>%select(ends_with("09")), 1, mean, na.rm = TRUE)))
## collate other covariates
camels = left_join(mrrmaidWide, mrrmaid%>%select(-c(mesicProp, date, system.index)), by = "gauge_d")
camels = unique(camels)

## outcomes
sigs = read.csv("./output/indicesAugSep.csv")
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
         "p_mean","p_ssnlt","pet_men","augflow", "sepflow", "augSepflow")

camelSigsCorVars = camelSigs%>% select(all_of(vars))
camelSigsCor = cor(camelSigsCorVars,use="pairwise.complete.obs")
corrplot(camelSigsCor) ## not a lot to see here

## Intactness
augIntact = ggplot(camelSigs, aes(x = intact, y = augflow, color = ecoregion))+
  geom_point()+
  geom_smooth(method = lm)

augIntact

sepIntact = ggplot(camelSigs, aes(x = intact, y = sepflow, color = ecoregion))+
  geom_point()+
  geom_smooth(method = lm)

sepIntact

augSepIntact = ggplot(camelSigs, aes(x = intact, y = augSepflow, color = ecoregion))+
  geom_point()+
  geom_smooth(method = lm)

augSepIntact


### Variability
#flashCV = ggplot(camelSigs, aes(x = CV, y = flashiness, color = ecoregion))+
#  geom_point()+
#  geom_smooth(method = lm)
#
#flashCV
#
#baseCV = ggplot(camelSigs, aes(x = CV, y = baseflow, color = ecoregion))+
#  geom_point()+
#  geom_smooth(method = lm)
#
#baseCV
#
#dryAreaCV = ggplot(camelSigs, aes(x = CV, y = dryMonthArea, color = ecoregion))+
#  geom_point()+
#  geom_smooth(method = lm)
#
#dryAreaCV
#
###
#intactCV = ggplot(camelSigs, aes(x = intact, y = CV, color = ecoregion))+
#  geom_point()+
#  geom_smooth(method = lm)
#
#intactCV

## six panel
ggarrange(augIntact, sepIntact, augSepIntact, ncol = 3, nrow=1)

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

## mods
library(brms)

## bring in the soils
soils = read.csv("./data/attributes/attributes_gageii_Soils.csv")
soils = soils %>%
  select(c("STAID", "CLAYAVE", "SILTAVE", "SANDAVE")) %>%
  rename(gauge_d = STAID)

df = left_join(camelSigs, soils, by= "gauge_d")

## topo for slope
topo = read.csv("./data/attributes/attributes_gageii_Topo.csv")
topo = topo %>%
  select("STAID", "SLOPE_PCT")%>%
  rename(gauge_d = STAID)

df = left_join(df, topo, by= "gauge_d")

## Start with some simple ones
## p_mean, frac_snow, AREA,aridity, Clay frac 

## standardize the covs
df$p_mean_std = scale(df$p_mean)
df$frc_snw_std = scale(df$frc_snw)
df$AREA_std = scale(df$AREA)
df$aridity_std = scale(df$aridity)
df$CLAYAVE_std = scale(df$CLAYAVE)
df$SLOPE_std = scale(df$SLOPE_PCT)

df$intact_std = scale(df$intact)
df$CV_std = scale(df$CV)


## Factor
df$eco = factor(df$min)

### AUGUST ###

## priors
priorsAug= get_prior(augflow ~ p_mean_std + frc_snw_std + SLOPE_std
                        + aridity_std + CLAYAVE_std + intact_std + CV_std
                        #+ (CV_std|eco) ## varying slopes
                        + (1+ intact_std + CV_std|eco), ## varying slopes and intercepts
                        data = df, family = 'beta')## the outcome is a proportion - bound bt 0 and 1

priorsAug$prior[1:8] = "normal (0,1)"
priorsAug$prior[15:17] = "normal (0,0.2)"


## toy mod
modAug = brm(augflow ~ p_mean_std + frc_snw_std + SLOPE_std 
           + aridity_std + CLAYAVE_std + intact_std + CV_std
           + (1+ intact_std + CV_std|eco), ## varying slopes
           data = df, 
           family = Beta(),
           prior = priorsAug, ## nor priors - 0,1 for any fixed effects; 0.2 for random effects
           control = list(adapt_delta = 0.999,max_treedepth = 15), ## lower to trial
           cores=4,
           chains = 4, ## lower to trial
           iter=8000)## lower to trial

summary(modAug)
plot(modAug)
pp_check(modAug)
r2Aug = bayes_R2(modAug) ## 0.527725 (med), 0.3231969 (lower 2.5), 0.6669567 (upper 97.5)

### Sep ###

## priors
priorsSep= get_prior(sepflow ~ p_mean_std + frc_snw_std + SLOPE_std
                     + aridity_std + CLAYAVE_std + intact_std + CV_std
                     #+ (CV_std|eco) ## varying slopes
                     + (1+ intact_std + CV_std|eco), ## varying slopes and intercepts
                     data = df, family = 'beta')## the outcome is a proportion - bound bt 0 and 1

priorsSep$prior[1:8] = "normal (0,1)"
priorsSep$prior[15:17] = "normal (0,0.2)"


## toy mod
modSep = brm(sepflow ~ p_mean_std + frc_snw_std + SLOPE_std 
             + aridity_std + CLAYAVE_std + intact_std + CV_std
             + (1+ intact_std + CV_std|eco), ## varying slopes
             data = df, 
             family = Beta(),
             prior = priorsSep, ## nor priors - 0,1 for any fixed effects; 0.2 for random effects
             control = list(adapt_delta = 0.999,max_treedepth = 15), ## lower to trial
             cores=4,
             chains = 4, ## lower to trial
             iter=8000)## lower to trial

summary(modSep)
plot(modSep)
pp_check(modSep)
r2sep = bayes_R2(modSep) ## 0.4418061 0.08655643 0.2568538 0.5902931


### SUM ###

## priors
priorsAugSep= get_prior(augSepflow ~ p_mean_std + frc_snw_std + SLOPE_std
                     + aridity_std + CLAYAVE_std + intact_std + CV_std
                     #+ (CV_std|eco) ## varying slopes
                     + (1+ intact_std + CV_std|eco), ## varying slopes and intercepts
                     data = df, family = 'beta')## the outcome is a proportion - bound bt 0 and 1

priorsAugSep$prior[1:8] = "normal (0,1)"
priorsAugSep$prior[15:17] = "normal (0,0.2)"


## toy mod
modAugSep = brm(augSepflow ~ p_mean_std + frc_snw_std + SLOPE_std 
             + aridity_std + CLAYAVE_std + intact_std + CV_std
             + (1+ intact_std + CV_std|eco), ## varying slopes
             data = df, 
             family = Beta(),
             prior = priorsAugSep, ## nor priors - 0,1 for any fixed effects; 0.2 for random effects
             control = list(adapt_delta = 0.999,max_treedepth = 15), ## lower to trial
             cores=4,
             chains = 4, ## lower to trial
             iter=8000)## lower to trial

summary(modAugSep)
plot(modAugSep)
pp_check(modAugSep)
r2AugSep = bayes_R2(modAugSep)
