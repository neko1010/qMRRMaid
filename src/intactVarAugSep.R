library(dplyr)
library(sf)
library(tidyr)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(corrplot)
library(foreign)
library(tidybayes)

setwd("~/BSU/MRRMAid/qMetrics/GAGES-II/")

mrrmaid = read.dbf("./data/gagesIInarrowNFWeco500.dbf")

## Pivot wider
mrrmaidWide = mrrmaid %>% pivot_wider(id_cols = c('GAGE_ID'),
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
gagesII = left_join(mrrmaidWide, mrrmaid%>%select(c(GAGE_ID, mode)), by = "GAGE_ID")
gagesII = unique(gagesII)

## outcomes
sigs = read.csv("./output/indicesAugSep.csv")
sigs = sigs %>%
  rename(GAGE_ID = gageID)

## create numeric GAGE_ID variable for the lookup
gagesII$GAGE_ID = as.numeric(levels(gagesII$GAGE_ID))[gagesII$GAGE_ID]

gagesIISigs = left_join(sigs, gagesII, by = "GAGE_ID")

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
#gagesIIjoin = left_join(gagesIISigs, gagesIIclimsl%>%select(-c(PERIMETER, AREA)), by = "GAGE_ID")
gagesIIjoin = left_join(gagesIISigs, gagesIIclimsl, by = "GAGE_ID")

## drop ecoregions with NA
gagesIIjoin = gagesIIjoin%>%drop_na(ecoregion)



vars = c("intact","CV", "AREA","PERIMETER","vpdmax", "snowfrac",
         "ppt","clay","slope","augflow" ,"sepflow",
         "augSepflow")

gagesIISigsCorVars = gagesIIjoin%>% select(all_of(vars))
gagesIISigsCor = cor(gagesIISigsCorVars,use="pairwise.complete.obs")
corrplot(gagesIISigsCor) ## not a lot to see here

## Intactness
augIntact = ggplot(gagesIIjoin, aes(x = intact, y = augflow, color = ecoregion))+
  geom_point()+
  geom_smooth(method = lm)

augIntact

sepIntact = ggplot(gagesIIjoin, aes(x = intact, y = sepflow, color = ecoregion))+
  geom_point()+
  geom_smooth(method = lm)

sepIntact

augSepIntact = ggplot(gagesIIjoin, aes(x = intact, y = augSepflow, color = ecoregion))+
  geom_point()+
  geom_smooth(method = lm)

augSepIntact

## six panel
ggarrange(augIntact, sepIntact, augSepIntact, ncol = 3, nrow=1)

## map the units
gagesIISHP = st_read("./data/gagesIInarrowNFWeco500.shp")
## just the id and geom

gagesIISHP = gagesIISHP %>%
  select(GAGE_ID, geometry) 

gagesIISHP = unique(gagesIISHP)

gagesIISHP$GAGE_ID = as.numeric(gagesIISHP$GAGE_ID)

metricsSHP = left_join(gagesIISHP,gagesIISigs,  by = "GAGE_ID")
metricsSHP = metricsSHP %>%
  filter(!is.na(ecoregion))

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

df$intact_std = scale(df$intact)
df$CV_std = scale(df$CV)


## Factor
df$eco = factor(df$mode)

### AUGUST ###

## priors
priorsAug= get_prior(augflow ~ ppt_std + snowfrac_std + slope_std
                     #  + vpdmax_std + clay_std +intact_std + CV_std 
                        + vpdmax_std + clay_std +intact_std*eli_tau,
                        #+ (1+ intact_std + CV_std|ecoregion), ## varying slopes and intercepts
                        #+ (1+ intact_std *eli_tau|ecoregion), ## varying slopes and intercepts
                        #+ (1+ intact_std*vpdmax_std + CV_std|eco), ## varying slopes and intercepts
                        data = df, family = 'beta')## the outcome is a proportion - bound bt 0 and 1

priorsAug$prior[1:9] = "normal (0,1)"
#priorsAug$prior[15:19] = "normal (0,0.2)"
#priorsAug$prior[16:20] = "normal (0,0.2)"



## August mod
modAug = brm(augflow ~  ppt_std + snowfrac_std + slope_std
           + vpdmax_std + clay_std + intact_std*eli_tau,
           #+ (1+ intact_std*eli_tau|ecoregion), ## varying slopes
           #+ (1+ intact_std*vpdmax_std + CV_std|ecoregion), ## higher r2 - 0.44
           data = df, 
           family = Beta(),
           prior = priorsAug, ## nor priors - 0,1 for any fixed effects; 0.2 for random effects
           control = list(adapt_delta = 0.999,max_treedepth = 15), ## lower to trial
           cores=4,
           chains = 4, 
           iter=10000)

summary(modAug)
ranef(modAug)
coefAug = coef(modAug)

plot(modAug)
pp_check(modAug)
r2Aug = bayes_R2(modAug)

## draws for the intacntess random effect
modAug %>%
  spread_draws(b_intact_std, r_ecoregion[ecoregion,randeff]) %>% ## these visbile in the $fit object
  filter(randeff == "intact_std") %>%  ## pulls all REs otherwise
  mutate(intactEff = b_intact_std + r_ecoregion) %>% ## calculating the estimates
  ggplot(aes(x = intactEff, y = ecoregion)) +
  stat_halfeye()

modAugdf = as.data.frame(modAug$fit)

# Extract draws in a long data frame
drawsAug <- modAug %>% 
  spread_draws( b_intact_std, r_ecoregion[ecoregion,rand_eff]) %>%
  filter(rand_eff == "intact_std") %>%
  mutate(intactEff = b_intact_std + r_ecoregion) ## replicated the coef() output!! 

# Calculate the mean and standard deviation per parameter
summaryStatsAug = drawsAug %>% 
  summarise(
    posterior_mean = mean(b_intact_std + r_ecoregion),
    posterior_sd = sd(b_intact_std +r_ecoregion)
    #posterior_mean = mean(r_ecoregion), ## OR THIS?
    #posterior_sd = sd(r_ecoregion)
  )


### Sep ###

## priors
priorsSep= get_prior(sepflow ~ ppt_std + snowfrac_std + slope_std
                     + vpdmax_std + clay_std  + intact_std * eli_tau,
                     #+ vpdmax_std + clay_std + intact_std + CV_std + eli_tau 
                     #+ (1+ intact_std*vpdmax_std + CV_std|ecoregion), ## varying slopes and intercepts - r2 0.56
                     #+ (1+ intact_std|ecoregion), ## varying slopes and intercepts
                     data = df, family = 'beta')## the outcome is a proportion - bound bt 0 and 1

priorsSep$prior[1:10] = "normal (0,1)"
priorsSep$prior[15:17] = "normal (0,0.2)"
#priorsSep$prior[16:20] = "normal (0,0.2)"

## Sep mod
modSep = brm(sepflow ~ ppt_std + snowfrac_std + slope_std
             + vpdmax_std + clay_std + intact_std * eli_tau, 
             #+ vpdmax_std + clay_std + intact_std + CV_std + eli_tau
             #+ (1+ intact_std*vpdmax_std + CV_std|ecoregion), ## varying slopes
             #+ (1+ intact_std |ecoregion), ## varying slopes
             data = df, 
             family = Beta(),
             prior = priorsSep, ## nor priors - 0,1 for any fixed effects; 0.2 for random effects
             control = list(adapt_delta = 0.999,max_treedepth = 15), ## lower to trial
             cores=4,
             chains = 4, ## lower to trial
             iter=10000)## lower to trial

summary(modSep)
plot(modSep)
pp_check(modSep)
r2sep = bayes_R2(modSep) 

## draws for the intacntess random effect
modSep %>%
  spread_draws(b_intact_std, r_ecoregion[ecoregion,randeff]) %>% ## these visbile in the $fit object
  filter(randeff == "intact_std") %>%  ## pulls all REs otherwise
  mutate(intactEff = b_intact_std + r_ecoregion) %>% ## calculating the estimates
  ggplot(aes(x = intactEff, y = ecoregion)) +
  stat_halfeye()

modSepdf = as.data.frame(modSep$fit)

# Extract draws in a long data frame
drawsSep <- modSep %>% 
  spread_draws( b_intact_std, r_ecoregion[ecoregion,rand_eff]) %>%
  filter(rand_eff == "intact_std") %>%
  mutate(intactEff = b_intact_std + r_ecoregion) ## replicated the coef() output!! 

# Calculate the mean and standard deviation per parameter
summaryStatsSep = drawsSep %>% 
  summarise(
    posterior_mean = mean(b_intact_std + r_ecoregion),
    posterior_sd = sd(b_intact_std +r_ecoregion)
    #posterior_mean = mean(r_ecoregion), ## OR THIS?
    #posterior_sd = sd(r_ecoregion)
  )
summaryStatsSep

### SUM ###

## priors
priorsAugSep= get_prior(augSepflow ~ ppt_std + snowfrac_std + slope_std
                        + vpdmax_std + clay_std + intact_std + CV_std + eli_tau
                     + (1+ intact_std + CV_std|ecoregion), ## varying slopes and intercepts
                     data = df, family = 'beta')## the outcome is a proportion - bound bt 0 and 1

priorsAugSep$prior[1:9] = "normal (0,1)"
priorsAugSep$prior[16:18] = "normal (0,0.2)"


## AugSep mod
modAugSep = brm(augSepflow ~ ppt_std + snowfrac_std + slope_std
                + vpdmax_std + clay_std + intact_std + CV_std + eli_tau
             + (1+ intact_std + CV_std|ecoregion), ## varying slopes
             data = df, 
             family = Beta(),
             prior = priorsAugSep, ## nor priors - 0,1 for any fixed effects; 0.2 for random effects
             control = list(adapt_delta = 0.999,max_treedepth = 15), ## lower to trial
             cores=4,
             chains = 4, ## lower to trial
             iter=10000)## lower to trial

summary(modAugSep)
plot(modAugSep)
pp_check(modAugSep)
r2AugSep = bayes_R2(modAugSep) 
