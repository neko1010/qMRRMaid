library(dplyr)
library(sf)
library(tidyr)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(corrplot)
library(foreign)
library(tidybayes)
library(stringr)
library(forcats)
library(ggspatial)

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
  select(-ends_with(c("06", "09"))) ## remove September 

## add intactness columns - august if june<july, september otherwise

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
#sigs = read.csv("./output/indices.csv")
sigs = read.csv("./output/indices_km2.csv")
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

flashWetIntact = ggplot(gagesIIjoin, aes(x = intact, y = flashinessWet, color = ecoregion))+
  geom_point()+
  geom_smooth(method = lm)

flashWetIntact


q10q95areaIntact = ggplot(gagesIIjoin, aes(x = intact, y = q10q95area, color = ecoregion))+
  geom_point()+
  geom_smooth(method = lm)

q10q95areaIntact

max30AreaIntact = ggplot(gagesIIjoin, aes(x = intact, y = max30area, color = ecoregion))+
  geom_point()+
  geom_smooth(method = lm)

max30AreaIntact



## six panel
ggarrange(baseIntact, dryAreaIntact, q10q95areaIntact, max30AreaIntact, flashWetIntact, flashIntact, ncol = 2, nrow=3)

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

maptheme<-theme(axis.title = element_text(color="black",size=16),
                axis.text = element_text(color="black",size=8),
                axis.ticks  = element_line(color="black"))

ggPoly = ggplot(metricsNAD83) +
  geom_sf(aes(fill = ecoregion))+
  geom_sf(data = sageStates, color = "black", fill = NA, lwd = 1)+
  geom_sf(data = sage, color = "red", fill = NA)+
  labs(fill = "Ecoregion")+
  annotation_scale(location = "br", width_hint = 0.5) +ylab("")+xlab("")+
  theme_bw()+maptheme+xlab("Longitude")+ylab("Latitude")
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

## descriptive statistics for table
baseMed = median(gagesIIjoin$baseflow)
baseMin = min(gagesIIjoin$baseflow)
baseMax = max(gagesIIjoin$baseflow)

dryMoMed = median(gagesIIjoin$dryMonthArea)
dryMoMin = min(gagesIIjoin$dryMonthArea)
dryMoMax = max(gagesIIjoin$dryMonthArea)

q10q95areaMed = median(gagesIIjoin$q10q95area, na.rm = T)
q10q95areaMin = min(gagesIIjoin$q10q95area, na.rm = T)
q10q95areaMax = max(gagesIIjoin$q10q95area, na.rm = T)

max30areaMed = median(gagesIIjoin$max30area)
max30areaMin = min(gagesIIjoin$max30area)
max30areaMax = max(gagesIIjoin$max30area)

flashinessMed = median(gagesIIjoin$flashiness)
flashinessMin = min(gagesIIjoin$flashiness)
flashinessMax = max(gagesIIjoin$flashiness)

flashinessWetMed = median(gagesIIjoin$flashinessWet)
flashinessWetMin = min(gagesIIjoin$flashinessWet)
flashinessWetMax = max(gagesIIjoin$flashinessWet)


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

hist(df$baseflow)
min(df$baseflow)

df$baseflowQ= df$baseflow + 0.000001

## priors
priorsBASE= get_prior(baseflowQ~ snowfrac_std + clay_std + intact_std*eli_tau_std 
                        + (1|eco), 
                        data = df, family = 'beta')

priorsBASE$prior[1:6] = "normal (0,1)"
#priorsBASE$prior[12:14] = "normal (0,0.2)"
priorsBASE$prior[10:11] = "normal (0,0.2)"


## toy mod
modBASE = brm(baseflowQ ~ snowfrac_std  + clay_std + intact_std*eli_tau_std 
           #+ (1+ intact_std|eco), ## varying slopes
           + (1|eco), ## varying slopes
           data = df, 
           family = Beta(),
           prior = priorsBASE, ## nor priors - 0,1 for any fixed effects; 0.2 for random effects
           control = list(adapt_delta = 0.999,max_treedepth = 15), ## lower to trial
           cores=4,
           seed = 77,
           chains = 4,
           init = 0.1,
           iter=8000)

summary(modBASE)
#plot(modBASE)
pp_check(modBASE)
r2BASE = bayes_R2(modBASE) 

## Marginal effects plot
baseMarg <- modBASE%>% #matched 
  gather_draws(`b_.*`, regex = TRUE) %>% 
  mutate(component = ifelse(str_detect(.variable, "phi_"), "Precision", "Mean"),
         intercept = str_detect(.variable, "Intercept"))%>%
  mutate(name = case_when(
    #endsWith(.variable, "Intercept") ~ "Intercept",
    endsWith(.variable, "snowfrac_std") ~ "Snow fraction",
    endsWith(.variable, "clay_std") ~ "Clay fraction", 
    endsWith(.variable, "intact_std") ~ "Intactness",
    endsWith(.variable, "intact_std:eli_tau_std") ~ "Intactness*ELI", 
    endsWith(.variable, "eli_tau_std") ~ "ELI")) %>%
  filter(.variable != "b_Intercept")

ggbase = ggplot(baseMarg, aes(x = .value, y = fct_rev(name), fill = component)) +
  geom_vline(xintercept = 0) +
  stat_halfeye(aes(slab_alpha = intercept), 
               .width = c(0.89), point_interval = "median_hdi") +
  #scale_fill_viridis_d(option = "viridis", end = 0.6) +
  scale_slab_alpha_discrete(range = c(1, 0.4)) +
  guides(fill = "none", slab_alpha = "none") +
  labs(x = "Effect size", y = "Variable") +
  facet_wrap(vars(component), ncol = 1, scales = "free_y")+
  ggtitle("Baseflow")

baseCond = conditional_effects(modBASE, effects = "intact_std:eli_tau_std", prob = 0.89)
baseCondPlot = plot(baseCond)[[1]] +
  xlab("Intactness") +
  ylab("Baseflow") +
  labs(color = "ELI", fill = "ELI")
baseCondPlot

### Back transform the coeffs




## Compare with gamma
## priors
priorsBASEgam= get_prior(baseflowQ ~ snowfrac_std + clay_std + intact_std*eli_tau_std 
                      #priorsBASE= get_prior(baseflowQ ~ ppt_std + snowfrac_std + clay_std + intact_std*eli_tau_std 
                      + (1|eco), 
                      #+ (1+ intact_std|eco), ## varying slopes and intercepts
                      data = df, family = 'gamma')

priorsBASEgam$prior[1:6] = "normal (0,1)"
#priorsBASE$prior[12:14] = "normal (0,0.2)"
priorsBASEgam$prior[10:11] = "normal (0,0.2)"

modBASEgam = brm(baseflowQ ~ snowfrac_std  + clay_std + intact_std*eli_tau_std 
              #modBASE = brm(baseflowQ ~ ppt_std + snowfrac_std  + clay_std + intact_std*eli_tau_std 
              #+ (1+ intact_std|eco), ## varying slopes
              + (1|eco), ## varying slopes
              data = df, 
              family = Gamma(link="log"),
              prior = priorsBASEgam, ## nor priors - 0,1 for any fixed effects; 0.2 for random effects
              control = list(adapt_delta = 0.999,max_treedepth = 15), ## lower to trial
              cores=4,
              seed = 77,
              chains = 4,
              init = 0.1,
              iter=8000)

summary(modBASEgam)



## Normal
## Compare with gamma
## priors
priorsBASEnorm= get_prior(baseflowQ ~ snowfrac_std + clay_std + intact_std*eli_tau_std 
                         #priorsBASE= get_prior(baseflowQ ~ ppt_std + snowfrac_std + clay_std + intact_std*eli_tau_std 
                         + (1|eco), 
                         #+ (1+ intact_std|eco), ## varying slopes and intercepts
                         data = df, family = 'normal')

priorsBASEnorm$prior[1:6] = "normal (0,1)"
#priorsBASE$prior[12:14] = "normal (0,0.2)"
priorsBASEnorm$prior[10:11] = "normal (0,0.2)"

modBASEnorm = brm(baseflowQ ~ snowfrac_std  + clay_std + intact_std*eli_tau_std 
                 #modBASE = brm(baseflowQ ~ ppt_std + snowfrac_std  + clay_std + intact_std*eli_tau_std 
                 #+ (1+ intact_std|eco), ## varying slopes
                 + (1|eco), ## varying slopes
                 data = df, 
                 family = gaussian(),
                 prior = priorsBASEnorm, ## nor priors - 0,1 for any fixed effects; 0.2 for random effects
                 control = list(adapt_delta = 0.999,max_treedepth = 15), ## lower to trial
                 cores=4,
                 seed = 77,
                 chains = 4,
                 init = 0.1,
                 iter=8000)

summary(modBASEnorm)

### Use loo to compare - BETA SEEMS BEST!
#modBASE = add_criterion(modBASE, "waic")
#modBASEgam = add_criterion(modBASEgam, "waic")
#modBASEnorm = add_criterion(modBASEnorm, "waic")
#
#loo_compare(modBASE, modBASEgam, modBASEnorm, criterion = "waic")
#
#modBASE = add_criterion(modBASE, "kfold")
#modBASEgam = add_criterion(modBASEgam, "kfold")
#modBASEnorm = add_criterion(modBASEnorm, "kfold")
#
#loo_compare(modBASE, modBASEgam, modBASEnorm, criterion = "kfold")
#
#modBASE = add_criterion(modBASE, "loo")
#modBASEgam = add_criterion(modBASEgam, "loo")
#modBASEnorm = add_criterion(modBASEnorm, "loo")
#
#loo_compare(modBASE, modBASEgam, modBASEnorm, criterion = "loo")

### NO INTERACTION


## priors
priorsBASEnoint = get_prior(baseflowQ ~ snowfrac_std + clay_std + intact_std + eli_tau_std 
                        #+ (CV_std|eco) ## varying slopes
                         + (1|eco), 
                        data = df, family = 'beta')## but maybe not 'over'dispersed

priorsBASEnoint$prior[1:5] = "normal (0,1)"
#priorsBASE$prior[15:17] = "normal (0,0.2)"
priorsBASEnoint$prior[9:10] = "normal (0,0.2)"


## toy mod
modBASEnoint = brm(baseflowQ ~ snowfrac_std  + clay_std + intact_std + eli_tau_std 
           + (1|eco),
           data = df, 
           family = Beta(),
           prior = priorsBASEnoint, ## nor priors - 0,1 for any fixed effects; 0.2 for random effects
           control = list(adapt_delta = 0.999,max_treedepth = 15), ## lower to trial
           cores=4,
           seed = 77,
           chains = 4,
           init = 0.1,
           iter=8000)

summary(modBASEnoint)
plot(modBASEnoint)

r2modBASEnoint =bayes_R2(modBASEnoint)

## Marginal effects plot
baseMargnoint <- modBASEnoint%>% 
  gather_draws(`b_.*`, regex = TRUE) %>% 
  mutate(component = ifelse(str_detect(.variable, "phi_"), "Precision", "Mean"),
         intercept = str_detect(.variable, "Intercept"))%>%
  mutate(name = case_when(
    #endsWith(.variable, "Intercept") ~ "Intercept",
    endsWith(.variable, "snowfrac_std") ~ "Snow fraction",
    endsWith(.variable, "clay_std") ~ "Clay fraction", 
    endsWith(.variable, "intact_std") ~ "Intactness",
    endsWith(.variable, "eli_tau_std") ~ "ELI")) %>%
  filter(.variable != "b_Intercept")

ggbaseNoInt = ggplot(baseMargnoint, aes(x = .value, y = fct_rev(name), fill = component)) +
  geom_vline(xintercept = 0) +
  stat_halfeye(aes(slab_alpha = intercept), 
               .width = c(0.89), point_interval = "median_hdi") +
  #scale_fill_viridis_d(option = "viridis", end = 0.6) +
  scale_slab_alpha_discrete(range = c(1, 0.4)) +
  guides(fill = "none", slab_alpha = "none") +
  labs(x = "Effect size", y = "Variable") +
  facet_wrap(vars(component), ncol = 1, scales = "free_y")+
  ggtitle("Baseflow - no interaction")

## DryMonth/Area - these are all positive, so log-normal since it is right skewed 

hist(df$dryMonthArea)

## priors
priorsDryArea = get_prior(dryMonthArea ~snowfrac_std  + clay_std + intact_std*eli_tau_std
                          + (1|eco), ## varying slopes
                          data = df, family = 'gamma')
#data = df, family = 'beta') #testing...

priorsDryArea$prior[1:6] = "normal (0,1)"
priorsDryArea$prior[9:11] = "normal (0,0.2)"


## toy mod
modDryArea = brm(dryMonthArea ~ snowfrac_std  + clay_std + intact_std*eli_tau_std
                 + (1|eco), ## varying slopes
                 data = df, 
                 family = Gamma(link = "log"),
                 prior = priorsDryArea, ## nor priors - 0,1 for any fixed effects; 0.2 for random effects
                 control = list(adapt_delta = 0.999,max_treedepth = 15), ## lower to trial
                 cores=4,
                 chains = 4, ## lower to trial
                 iter=8000)## lower to trial

summary(modDryArea)
#plot(modDryArea)
pp_check(modDryArea)
r2DryArea= bayes_R2(modDryArea)  
dryAreaCond= conditional_effects(modDryArea, effects = "intact_std:eli_tau_std", prob = 0.89)
dryAreaCondPlot = plot(dryAreaCond)[[1]] +
  xlab("Intactness") +
  ylab("Dry Month/Area") +
  labs(color = "ELI", fill = "ELI")
dryAreaCondPlot

## Marginal effects plot
dryAreaMarg <- modDryArea%>% 
  gather_draws(`b_.*`, regex = TRUE) %>% 
  mutate(component = ifelse(str_detect(.variable, "phi_"), "Precision", "Mean"),
         intercept = str_detect(.variable, "Intercept"))%>%
  mutate(name = case_when(
    #endsWith(.variable, "Intercept") ~ "Intercept",
    endsWith(.variable, "snowfrac_std") ~ "Snow fraction",
    endsWith(.variable, "clay_std") ~ "Clay fraction", 
    endsWith(.variable, "intact_std") ~ "Intactness",
    endsWith(.variable, "intact_std:eli_tau_std") ~ "Intactness*ELI", 
    endsWith(.variable, "eli_tau_std") ~ "ELI")) %>%
  filter(.variable != "b_Intercept")

ggDryArea = ggplot(dryAreaMarg, aes(x = .value, y = fct_rev(name), fill = component)) +
  geom_vline(xintercept = 0) +
  stat_halfeye(aes(slab_alpha = intercept), 
               .width = c(0.89), point_interval = "median_hdi") +
  #scale_fill_viridis_d(option = "viridis", end = 0.6) +
  scale_slab_alpha_discrete(range = c(1, 0.4)) +
  guides(fill = "none", slab_alpha = "none") +
  labs(x = "Effect size", y = "Variable") +
  facet_wrap(vars(component), ncol = 1, scales = "free_y")+
  ggtitle("Dry month/Area")



#### FLASHINESS #############

hist(df$flashiness)

## priors
priorsFlash = get_prior(flashiness ~snowfrac_std  + clay_std + intact_std*eli_tau_std
                          + (1|eco), ## varying slopes
                          data = df, family = 'lognormal')

priorsFlash$prior[1:6] = "normal (0,1)"
priorsFlash$prior[9:11] = "normal (0,0.2)"


## toy mod
modFlash = brm(flashiness ~ snowfrac_std  + clay_std + intact_std*eli_tau_std
                 + (1|eco), ## varying slopes
                 data = df, 
                 family = lognormal(),
                 prior = priorsFlash, ## nor priors - 0,1 for any fixed effects; 0.2 for random effects
                 control = list(adapt_delta = 0.999,max_treedepth = 15), ## lower to trial
                 cores=4,
                 chains = 4, ## lower to trial
                 iter=8000)## lower to trial

summary(modFlash)
#plot(modFlash)
pp_check(modFlash)
r2Flash= bayes_R2(modFlash)  
flashCond= conditional_effects(modFlash, effects = "intact_std:eli_tau_std", prob = 0.89)
flashCondPlot = plot(flashCond)[[1]] +
  xlab("Intactness") +
  ylab("Flashiness") +
  labs(color = "ELI", fill = "ELI")
flashCondPlot

## Marginal effects plot
flashMarg <- modFlash%>% 
  gather_draws(`b_.*`, regex = TRUE) %>% 
  mutate(component = ifelse(str_detect(.variable, "phi_"), "Precision", "Mean"),
         intercept = str_detect(.variable, "Intercept"))%>%
  mutate(name = case_when(
    #endsWith(.variable, "Intercept") ~ "Intercept",
    endsWith(.variable, "snowfrac_std") ~ "Snow fraction",
    endsWith(.variable, "clay_std") ~ "Clay fraction", 
    endsWith(.variable, "intact_std") ~ "Intactness",
    endsWith(.variable, "intact_std:eli_tau_std") ~ "Intactness*ELI", 
    endsWith(.variable, "eli_tau_std") ~ "ELI")) %>%
  filter(.variable != "b_Intercept")


ggflash = ggplot(flashMarg, aes(x = .value, y = fct_rev(name), fill = component)) +
  geom_vline(xintercept = 0) +
  stat_halfeye(aes(slab_alpha = intercept), 
               .width = c(0.89), point_interval = "median_hdi") +
  #scale_fill_viridis_d(option = "viridis", end = 0.6) +
  scale_slab_alpha_discrete(range = c(1, 0.4)) +
  guides(fill = "none", slab_alpha = "none") +
  labs(x = "Effect size", y = "Variable") +
  facet_wrap(vars(component), ncol = 1, scales = "free_y")+
  ggtitle("Flashiness")

### FLASHINESS WET ###


hist(df$flashinessWet)

## priors
priorsFlashWet = get_prior(flashinessWet ~snowfrac_std  + clay_std + intact_std*eli_tau_std
                        + (1|eco), ## varying slopes
                        data = df, family = 'lognormal')
#data = df, family = 'beta') #testing...

priorsFlashWet$prior[1:6] = "normal (0,1)"
priorsFlashWet$prior[9:11] = "normal (0,0.2)"


## toy mod
modFlashWet = brm(flashinessWet ~ snowfrac_std  + clay_std + intact_std*eli_tau_std
               + (1|eco), ## varying slopes
               data = df, 
               family = lognormal(),
               prior = priorsFlashWet, ## nor priors - 0,1 for any fixed effects; 0.2 for random effects
               control = list(adapt_delta = 0.999,max_treedepth = 15), ## lower to trial
               cores=4,
               chains = 4, ## lower to trial
               iter=8000)## lower to trial

summary(modFlashWet)
#plot(modFlashWet)
pp_check(modFlashWet)
r2FlashWet= bayes_R2(modFlashWet)  
flashWetCond= conditional_effects(modFlashWet, effects = "intact_std:eli_tau_std", prob = 0.89)
flashWetCondPlot = plot(flashWetCond)[[1]] +
  xlab("Intactness") +
  ylab("Flashiness Wet") +
  labs(color = "ELI", fill = "ELI")
flashWetCondPlot

## Marginal effects plot
flashWetMarg <- modFlashWet%>% 
  gather_draws(`b_.*`, regex = TRUE) %>% 
  mutate(component = ifelse(str_detect(.variable, "phi_"), "Precision", "Mean"),
         intercept = str_detect(.variable, "Intercept"))%>%
  mutate(name = case_when(
    #endsWith(.variable, "Intercept") ~ "Intercept",
    endsWith(.variable, "snowfrac_std") ~ "Snow fraction",
    endsWith(.variable, "clay_std") ~ "Clay fraction", 
    endsWith(.variable, "intact_std") ~ "Intactness",
    endsWith(.variable, "intact_std:eli_tau_std") ~ "Intactness*ELI", 
    endsWith(.variable, "eli_tau_std") ~ "ELI")) %>%
  filter(.variable != "b_Intercept")


ggflashwet = ggplot(flashWetMarg, aes(x = .value, y = fct_rev(name), fill = component)) +
  geom_vline(xintercept = 0) +
  stat_halfeye(aes(slab_alpha = intercept), 
               .width = c(0.89), point_interval = "median_hdi") +
  #scale_fill_viridis_d(option = "viridis", end = 0.6) +
  scale_slab_alpha_discrete(range = c(1, 0.4)) +
  guides(fill = "none", slab_alpha = "none") +
  labs(x = "Effect size", y = "Variable") +
  facet_wrap(vars(component), ncol = 1, scales = "free_y")+
  ggtitle("Flashiness wet")

## Q10-Q95/Area
hist(df$q10q95area)

df$q10q95areaQ= df$q10q95area + 0.000001

## priors
priorsq10q95 = get_prior(q10q95areaQ ~snowfrac_std  + clay_std + intact_std*eli_tau_std
                         #+ (1+ intact_std|eco), ## varying slopes
                         + (1|eco),
                         data = df, family = 'lognormal') 
                      

priorsq10q95$prior[1:6] = "normal (0,1)"
#priorsq10q95$prior[11:13] = "normal (0,0.2)"
priorsq10q95$prior[9:10] = "normal (0,0.2)"


## toy mod
modq10q95 = brm(q10q95areaQ ~ snowfrac_std  + clay_std + intact_std*eli_tau_std
                #+ (1+ intact_std|eco), ## varying slopes
                + (1|eco), ## varying slopes
                data = df, 
                family = lognormal(),
                prior = priorsq10q95, ## nor priors - 0,1 for any fixed effects; 0.2 for random effects
                control = list(adapt_delta = 0.999,max_treedepth = 15), ## lower to trial
                cores=4,
                chains = 4, ## lower to trial
                iter=8000)## lower to trial

summary(modq10q95)
#plot(modq10q95)
pp_check(modq10q95)
r2q10q95= bayes_R2(modq10q95)  
q10q95Cond= conditional_effects(modq10q95, effects = "intact_std:eli_tau_std", prob = 0.89)
q10q95CondPlot = plot(q10q95Cond)[[1]] +
  xlab("Intactness") +
  ylab("(Q10-Q95)/Area") +
  labs(color = "ELI", fill = "ELI")
q10q95CondPlot

## Marginal effects plot
q10q95Marg <- modq10q95%>% 
  gather_draws(`b_.*`, regex = TRUE) %>% 
  mutate(component = ifelse(str_detect(.variable, "phi_"), "Precision", "Mean"),
         intercept = str_detect(.variable, "Intercept"))%>%
  mutate(name = case_when(
    #endsWith(.variable, "Intercept") ~ "Intercept",
    endsWith(.variable, "snowfrac_std") ~ "Snow fraction",
    endsWith(.variable, "clay_std") ~ "Clay fraction", 
    endsWith(.variable, "intact_std") ~ "Intactness",
    endsWith(.variable, "intact_std:eli_tau_std") ~ "Intactness*ELI", 
    endsWith(.variable, "eli_tau_std") ~ "ELI")) %>%
  filter(.variable != "b_Intercept")


ggq10q95 = ggplot(q10q95Marg, aes(x = .value, y = fct_rev(name), fill = component)) +
  geom_vline(xintercept = 0) +
  stat_halfeye(aes(slab_alpha = intercept), 
               .width = c(0.89), point_interval = "median_hdi") +
  #scale_fill_viridis_d(option = "viridis", end = 0.6) +
  scale_slab_alpha_discrete(range = c(1, 0.4)) +
  guides(fill = "none", slab_alpha = "none") +
  labs(x = "Effect size", y = "Variable") +
  facet_wrap(vars(component), ncol = 1, scales = "free_y")+
  ggtitle("(Q10-Q95)/Area")

## Max30/Area
hist(df$max30area)
## priors
priorsMax30 = get_prior(max30area ~ snowfrac_std  + clay_std + intact_std*eli_tau_std
                + (1|eco), ## varying sloaes
                        data = df, family = 'lognormal')

priorsMax30$prior[1:6] = "normal (0,1)"
priorsMax30$prior[9:11] = "normal (0,0.2)"


## toy mod
modMax30 = brm(max30area ~ snowfrac_std  + clay_std + intact_std*eli_tau_std
               + (1|eco), ## varying slopes
               data = df, 
               family = lognormal(),
               prior = priorsMax30, ## nor priors - 0,1 for any fixed effects; 0.2 for random effects
               control = list(adapt_delta = 0.999,max_treedepth = 15), ## lower to trial
               cores=4,
               chains = 4, ## lower to trial
               iter=8000)## lower to trial

summary(modMax30)
#plot(modMax30)
pp_check(modMax30)
r2Max30= bayes_R2(modMax30)
max30Cond= conditional_effects(modMax30, effects = "intact_std:eli_tau_std", prob = 0.89)
max30CondPlot = plot(max30Cond)[[1]] +
  xlab("Intactness") +
  ylab("Max 30/Area") +
  labs(color = "ELI", fill = "ELI")
max30CondPlot

## Marginal effects plot
max30Marg <- modMax30%>% 
  gather_draws(`b_.*`, regex = TRUE) %>% 
  mutate(component = ifelse(str_detect(.variable, "phi_"), "Precision", "Mean"),
         intercept = str_detect(.variable, "Intercept"))%>%
  mutate(name = case_when(
    #endsWith(.variable, "Intercept") ~ "Intercept",
    endsWith(.variable, "snowfrac_std") ~ "Snow fraction",
    endsWith(.variable, "clay_std") ~ "Clay fraction", 
    endsWith(.variable, "intact_std") ~ "Intactness",
    endsWith(.variable, "intact_std:eli_tau_std") ~ "Intactness*ELI", 
    endsWith(.variable, "eli_tau_std") ~ "ELI")) %>%
  filter(.variable != "b_Intercept")


ggmax30 = ggplot(max30Marg, aes(x = .value, y = fct_rev(name), fill = component)) +
  geom_vline(xintercept = 0) +
  stat_halfeye(aes(slab_alpha = intercept), 
               .width = c(0.89), point_interval = "median_hdi") +
  #scale_fill_viridis_d(option = "viridis", end = 0.6) +
  scale_slab_alpha_discrete(range = c(1, 0.4)) +
  guides(fill = "none", slab_alpha = "none") +
  labs(x = "Effect size", y = "Variable") +
  facet_wrap(vars(component), ncol = 1, scales = "free_y")+
  ggtitle("Max 30/Area")


### Some six panel plots ###
## histograms
histList = list(gghistogram(df$baseflow)+xlab("Baseflow"), gghistogram(df$dryMonthArea)+xlab("Dry Month/Area"),
                gghistogram(df$q10q95area)+xlab("(Q10-Q95)/Area"), gghistogram(df$max30area)+xlab("Max 30/Area"),
                gghistogram(df$flashinessWet)+xlab("Flashiness wet season"), gghistogram(df$flashiness)+xlab("Flashiness"))
hist6 = ggarrange(plotlist = histList, nrow = 3, ncol=2, 
                  labels = c("A", "B", "C", "D","E", "F"))
hist6
## marginal effects plts
marg6 = ggarrange(ggbase, ggDryArea, ggq10q95, ggmax30, ggflashwet, ggflash, 
                  nrow=3, ncol = 2, labels = c("A", "B", "C", "D","E", "F")  )
marg6


## conditional effects plts
cond6 = ggarrange(baseCondPlot, dryAreaCondPlot, q10q95CondPlot, max30CondPlot,
                  flashWetCondPlot, flashCondPlot, nrow = 3, ncol = 2, 
                  labels = c("A", "B", "C", "D","E", "F"))

cond6

##r2 table
r2table = rbind(r2BASE, r2DryArea, r2q10q95, r2Max30, r2FlashWet, r2Flash)

## Export model objects
saveRDS(modBASE, "./output/mods/base.rds")
saveRDS(modDryArea, "./output/mods/dryarea.rds")
saveRDS(modMax30, "./output/mods/max30.rds")
saveRDS(modq10q95, "./output/mods/q10q95.rds")
saveRDS(modFlash, "./output/mods/flash.rds")
saveRDS(modFlashWet, "./output/mods/flashWet.rds")


## conditional effects of ELI on ###
## Base
baseCond[[1]][1:3,]
baseCond[[1]][298:300,]

## Dry month/area
dryAreaCond[[1]][1:3,]
dryAreaCond[[1]][298:300,]

## Max30
max30Cond[[1]][1:3,]
max30Cond[[1]][298:300,]

## Max30
max30Cond[[1]][1:3,]
max30Cond[[1]][298:300,]

##FlashinessWet 
flashWetCond[[1]][1:3,]
flashWetCond[[1]][298:300,]

##Flashiness 
flashCond[[1]][1:3,]
flashCond[[1]][298:300,]
