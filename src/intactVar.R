library(dplyr)
library(sf)
library(tidyr)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(corrplot)
library(foreign)
setwd("~/BSU/MRRMAid/qMetrics/GAGES-II")

## Calculate intactness and variability for each
#mrrmaid = read.csv("./data/gagesIInarrowNFWeco500.csv")
mrrmaid = read.dbf("./data/gagesIInarrowNFWeco500.dbf")

## Pivot wider
mrrmaidWide = mrrmaid %>% pivot_wider(id_cols = c('GAGE_ID'), ## 18,861 obs
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
                      apply(mrrmaidWide[,3:length(colnames(mrrmaidNoJune))], 1, sd, na.rm = T),
                      apply(mrrmaidWide[,3:length(colnames(mrrmaid))], 1, sd, na.rm = T))) %>%
  mutate(meanTS = if_else(julyMean>juneMean,
                          apply(mrrmaidWide[,3:length(colnames(mrrmaidNoJune))], 1, mean, na.rm = T),
                          apply(mrrmaidWide[,3:length(colnames(mrrmaid))], 1, mean, na.rm = T))) %>%
  mutate(CV = SD/meanTS) %>% 
  mutate(intact = if_else(julyMean>juneMean,
                              apply(mrrmaidWide %>%select(ends_with("08")), 1, mean, na.rm = T),
                              apply(mrrmaidWide %>%select(ends_with("09")), 1, mean, na.rm = T)))
## collate other covariates
#gagesII = left_join(mrrmaidWide, mrrmaid%>%select(-c(mesicProp, date, system.index)), by = "GAGE_ID")
gagesII = left_join(mrrmaidWide, mrrmaid%>%select(-c(mesicProp, date)), by = "GAGE_ID")
gagesIIuniq = unique(gagesII)

## outcomes
sigs = read.csv("./output/indices.csv")
sigs = sigs %>%
  rename(GAGE_ID = gageID)

## create numeric GAGE_ID variable for the lookup
gagesIIuniq$GAGE_ID = as.numeric(levels(gagesIIuniq$GAGE_ID))[gagesIIuniq$GAGE_ID]

gagesIISigs = left_join(sigs, gagesIIuniq, by = "GAGE_ID")

##explore and group using ecoregions
ecoLookup = c('Northwestern Glaciated Plains', 'Middle Rockies', 'Idaho Batholith',
              'Northwestern Great Plains', 'Snake River Plain', 'Eastern Cascades Slopes and Foothills', 
              'Columbia Plateau', 'Cascades','Sierra Nevada', 'Southern Rockies', 'Central Basin and Range', 
              'Mojave Basin and Range', 'Arizona/New Mexico Mountains', 'Arizona/New Mexico Plateau',
              'Wasatch and Uinta Mountains', 'Northern Basin and Range', 'Blue Mountains',
              'Colorado Plateaus', 'Wyoming Basin','High Plains')

ecoCode = c(42, 17, 16, 43, 12, 9, 10, 4, 5, 21, 13, 14, 23, 22, 19, 80, 11, 20, 18, 25)



dfEco = cbind.data.frame(ecoCode, ecoLookup)
colnames(dfEco) = c('mode', 'ecoregion')

gagesIISigs = left_join(gagesIISigs, dfEco, by = "mode")

## Bring in climate data
gagesIIclim = read.dbf("./data/gagesIIclim.dbf")
colnames(gagesIIclim)

## slope - i forgot to add it in the EE stack
slope = read.dbf("./data/gagesIISLOPE.dbf") %>%
  rename(slope = mean)

gagesIIclimsl = left_join(gagesIIclim, slope%>%select(c('GAGE_ID', 'slope')), by = "GAGE_ID")

## create numeric GAGE_ID variable for the lookup
gagesIIclimsl$GAGE_ID = as.numeric(levels(gagesIIclimsl$GAGE_ID))[gagesIIclimsl$GAGE_ID]

## Join them
gagesIIjoin = left_join(gagesIISigs, gagesIIclimsl%>%select(-c(PERIMETER, AREA)), by = "GAGE_ID")

## drop ecoregions with NA
gagesIIjoin = gagesIIjoin%>%drop_na(ecoregion)


vars = c("intact","CV", "AREA","PERIMETER","vpdmax", "snowfrac",
         "ppt","clay","slope","flashiness" ,"flashinessWet",
         "max30area","q10q95area","dryMonthArea","baseflow")

gagesIISigsCorVars = gagesIIjoin%>% select(all_of(vars))
gagesIISigsCor = cor(gagesIISigsCorVars,use="pairwise.complete.obs")
corrplot(gagesIISigsCor) ## not a lot to see here

## Intactness
flashIntact = ggplot(gagesIIjoin, aes(x = intact, y = flashiness, color = ecoregion))+
  geom_point()+
  geom_smooth(method = lm)

flashIntact


baseIntact = ggplot(gagesIIjoin, aes(x = intact, y = baseflow, color = ecoregion))+
  geom_point()+
  geom_smooth(method = lm)

baseIntact

dryAreaIntact = ggplot(gagesIIjoin, aes(x = intact, y = dryMonthArea, color = ecoregion))+
  geom_point()+
  geom_smooth(method = lm)

dryAreaIntact

## Variability
flashCV = ggplot(gagesIIjoin, aes(x = CV, y = flashiness, color = ecoregion))+
  geom_point()+
  geom_smooth(method = lm)

flashCV

baseCV = ggplot(gagesIIjoin, aes(x = CV, y = baseflow, color = ecoregion))+
  geom_point()+
  geom_smooth(method = lm)

baseCV

dryAreaCV = ggplot(gagesIIjoin, aes(x = CV, y = dryMonthArea, color = ecoregion))+
  geom_point()+
  geom_smooth(method = lm)

dryAreaCV

##
intactCV = ggplot(gagesIIjoin, aes(x = intact, y = CV, color = ecoregion))+
  geom_point()+
  geom_smooth(method = lm)

intactCV

## six panel
ggarrange(flashIntact, flashCV, baseIntact, baseCV, dryAreaIntact, dryAreaCV, ncol = 2, nrow=3)

## map the units
gagesSHP = st_read("./data/gagesIIclim.shp")
## just the id and geom

gagesSHP = gagesSHP %>%
  select(GAGE_ID, geometry) 

gagesSHP$GAGE_ID = as.numeric(gagesSHP$GAGE_ID)


metricsSHP = left_join(gagesSHP, gagesIIjoin, by = "GAGE_ID")
metricsSHP = metricsSHP %>%
  filter(!is.na(ecoregion))

## convert to sf obj
metricssf = st_as_sf(metricsSHP, wkt = "geometry")

states = st_read("../../watershedResilience/data/tl_2024_us_state.shp")
sage = st_read("../../watershedResilience/data/sagebrushBiome.shp")

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

#df = gagesIIjoin

## bring in the ELI
eli = read.csv('./data/gagesIIeli.csv')
eli = na.omit(eli) %>%
  select(c('GAGE_ID', 'eli_rho', 'eli_tau'))

df = left_join(gagesIIjoin, eli, by = "GAGE_ID")

## standardize the covs
df$ppt_std = scale(df$ppt)
df$snowfrac_std = scale(df$snowfrac)
df$AREA_std = scale(df$AREA)
df$vpdmax_std = scale(df$vpdmax)
df$clay_std = scale(df$clay)
df$slope_std = scale(df$slope)
df$eli_tau_std = scale(df$eli_tau)

df$intact_std = scale(df$intact)
df$CV_std = scale(df$CV)


## Factor
df$eco = factor(df$mode)


## need to rm baseflow = 0 for the beta OR add 0.000001
df$baseflowTEST = df$baseflow + 0.000001

## priors
priorsBASE= get_prior(baseflowTEST ~ ppt_std + snowfrac_std + slope_std
#priorsBASE= get_prior(baseflow ~ ppt_std + snowfrac_std + slope_std
                        + vpdmax_std + clay_std + intact_std + eli_tau_std
                        #+ (CV_std|eco) ## varying slopes
                         + (1+ intact_std + CV_std|eco), ## varying slopes and intercepts
                        #data = df, family = 'beta')## the outcome is a proportion - bound bt 0 and 1
                        #data = df, family = 'lognormal')## but also continuous and positive
                        data = df, family = 'gamma')## but maybe not 'over'dispersed

priorsBASE$prior[1:8] = "normal (0,1)"
#priorsBASE$prior[15:17] = "normal (0,0.2)"
priorsBASE$prior[13:16] = "normal (0,0.2)"


## toy mod
modBASE = brm(baseflowTEST ~ ppt_std + snowfrac_std + slope_std 
#modBASE = brm(baseflow~ ppt_std + snowfrac_std + slope_std 
           + vpdmax_std + clay_std + intact_std + eli_tau_std
           + (1+ intact_std + CV_std|eco), ## varying slopes
           data = df, 
           #family = Beta(),
           #family = lognormal(),
           family = Gamma(link="log"),
           prior = priorsBASE, ## nor priors - 0,1 for any fixed effects; 0.2 for random effects
           control = list(adapt_delta = 0.999,max_treedepth = 15), ## lower to trial
           cores=4,
           chains = 4,
           init = 0.1,
           iter=8000)

summary(modBASE)
plot(modBASE)
pp_check(modBASE)
r2BASE = bayes_R2(modBASE) ##0.3 beta 0.5 lognormal -try a gamma 0.374

## FLASHINESS - these are all positive, so maybe a log-normal since it is right skewed - could be negative tho...
## priors
priorsFlash = get_prior(flashiness ~ ppt_std + snowfrac_std + slope_std 
                       + vpdmax_std + clay_std + intact_std + CV_std
                       + (1+ intact_std + CV_std|eco), ## varying slopes
                       data = df, family = 'lognormal')
                       #data = df, family = 'beta') #testing...

priorsFlash$prior[1:8] = "normal (0,1)"
priorsFlash$prior[14:16] = "normal (0,0.2)"
#priorsFlash$prior[15:17] = "normal (0,0.2)" ## for beta


## toy mod
modFlash = brm(flashiness ~ ppt_std + snowfrac_std + slope_std 
          + vpdmax_std + clay_std + intact_std + CV_std
          + (1+ intact_std + CV_std|eco), ## varying slopes
          data = df, 
          family = lognormal(),
          #family = Beta(),
          prior = priorsFlash, ## nor priors - 0,1 for any fixed effects; 0.2 for random effects
          control = list(adapt_delta = 0.999,max_treedepth = 15), ## lower to trial
          cores=4,
          chains = 4, ## lower to trial
          iter=8000)## lower to trial

summary(modFlash)
plot(modFlash)
pp_check(modFlash)
r2Flash = bayes_R2(modFlash) ## 0.734 +- 0.148 lognormal; 0.8857 +- 0.0361 Beta; 0.8696 +- 0.0477 

## FLASHINESS wet season - these are all positive, so maybe a log-normal since it is right skewed - could be negative tho...
## Not a ton here AND fits worst of all

hist(df$flashinessWet)
min(df$flashinessWet)

## priors
priorsFlashWet = get_prior(flashinessWet ~ ppt_std + snowfrac_std + slope_std 
                       + vpdmax_std + clay_std + intact_std + CV_std
                       + (1+ intact_std + CV_std|eco), ## varying slopes
                       data = df, family = 'lognormal')
                       #data = df, family = 'beta') #testing...

priorsFlashWet$prior[1:8] = "normal (0,1)"
priorsFlashWet$prior[14:16] = "normal (0,0.2)"
#priorsFlash$prior[15:17] = "normal (0,0.2)" ## for beta


## toy mod
modFlashWet = brm(flashinessWet ~ ppt_std + snowfrac_std + slope_std 
          + vpdmax_std + clay_std + intact_std + CV_std
          + (1+ intact_std + CV_std|eco), ## varying slopes
          data = df, 
          family = lognormal(),
          #family = Beta(),
          prior = priorsFlashWet, ## nor priors - 0,1 for any fixed effects; 0.2 for random effects
          control = list(adapt_delta = 0.999,max_treedepth = 15), ## lower to trial
          cores=4,
          chains = 4, ## lower to trial
          iter=8000)## lower to trial

summary(modFlashWet)
plot(modFlashWet)
pp_check(modFlashWet)
r2FlashWet= bayes_R2(modFlashWet) ##


## DryMonth/Area - these are all positive, so log-normal since it is right skewed 

hist(df$dryMonthArea)
## priors
priorsDryArea = get_prior(dryMonthArea ~ ppt_std + snowfrac_std + slope_std 
                        + vpdmax_std + clay_std + intact_std + CV_std
                        + (1+ intact_std + CV_std|eco), ## varying slopes
                        data = df, family = 'lognormal')
#data = df, family = 'beta') #testing...

priorsDryArea$prior[1:8] = "normal (0,1)"
priorsDryArea$prior[14:16] = "normal (0,0.2)"


## toy mod
modDryArea = brm(dryMonthArea ~ ppt_std + snowfrac_std + slope_std 
               + vpdmax_std + clay_std + intact_std + CV_std
               + (1+ intact_std + CV_std|eco), ## varying slopes
               data = df, 
               family = lognormal(),
               #family = Beta(),
               prior = priorsDryArea, ## nor priors - 0,1 for any fixed effects; 0.2 for random effects
               control = list(adapt_delta = 0.999,max_treedepth = 15), ## lower to trial
               cores=4,
               chains = 4, ## lower to trial
               iter=8000)## lower to trial

summary(modDryArea)
plot(modDryArea)
pp_check(modDryArea)
r2DryArea= bayes_R2(modDryArea)  

## Max30/Area - these are all positive, so log-normal since it is right skewed 
## Mesic veg doesn't show much influence here
hist(df$max30area)
## priors
priorsMax30 = get_prior(max30area ~ ppt_std + snowfrac_std + slope_std 
                        + vpdmax_std + clay_std + intact_std + CV_std
                        + (1+ intact_std + CV_std|eco), ## varying slopes
                        data = df, family = 'lognormal')
#data = df, family = 'beta') #testing...

priorsMax30$prior[1:8] = "normal (0,1)"
priorsMax30$prior[14:16] = "normal (0,0.2)"


## toy mod
modMax30 = brm(max30area ~ ppt_std + snowfrac_std + slope_std 
               + vpdmax_std + clay_std + intact_std + CV_std
               + (1+ intact_std + CV_std|eco), ## varying slopes
               data = df, 
               family = lognormal(),
               #family = Beta(),
               prior = priorsMax30, ## nor priors - 0,1 for any fixed effects; 0.2 for random effects
               control = list(adapt_delta = 0.999,max_treedepth = 15), ## lower to trial
               cores=4,
               chains = 4, ## lower to trial
               iter=8000)## lower to trial

summary(modMax30)
plot(modMax30)
pp_check(modMax30)
r2Max30= bayes_R2(modMax30)  

## Q10-Q95/Area - these are all positive, so log-normal since it is right skewed 
## not much to see here either
hist(df$q10q95area)
## priors
priorsq10q95 = get_prior(q10q95area ~ ppt_std + snowfrac_std + slope_std 
                        + vpdmax_std + clay_std + intact_std + CV_std
                        + (1+ intact_std + CV_std|eco), ## varying slopes
                        data = df, family = 'lognormal')
#data = df, family = 'beta') #testing...

priorsq10q95$prior[1:8] = "normal (0,1)"
priorsq10q95$prior[14:16] = "normal (0,0.2)"


## toy mod
modq10q95 = brm(q10q95area ~ ppt_std + snowfrac_std + slope_std 
               + vpdmax_std + clay_std + intact_std + CV_std
               + (1+ intact_std + CV_std|eco), ## varying slopes
               data = df, 
               family = lognormal(),
               prior = priorsq10q95, ## nor priors - 0,1 for any fixed effects; 0.2 for random effects
               control = list(adapt_delta = 0.999,max_treedepth = 15), ## lower to trial
               cores=4,
               chains = 4, ## lower to trial
               iter=8000)## lower to trial

summary(modq10q95)
plot(modq10q95)
pp_check(modq10q95)
r2q10q95= bayes_R2(modq10q95)  
