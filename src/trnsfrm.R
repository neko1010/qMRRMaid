library(brms)
library(tidyr)
library(marginaleffects)
library(ggplot2)
library(tidybayes)
library(dplyr)
library(forcats)
library(stringr)
library(ggpubr)

setwd("~/BSU/MRRMAid/qMetrics/GAGES-II")
## load model obj

modBASE = readRDS("./output/mods/base.rds")
modDryArea= readRDS("./output/mods/dryarea.rds")
modMax30 = readRDS("./output/mods/max30.rds")
modq10q95 = readRDS("./output/mods/q10q95.rds")
modFlash = readRDS("./output/mods/flash.rds")
modFlashWet = readRDS("./output/mods/flashWet.rds")

## Marginal effects plot
baseMarg <- modBASE%>%  
  gather_draws(`b_.*`, regex = TRUE) %>% 
  mutate(component = ifelse(str_detect(.variable, "phi_"), "Precision", "Mean"),
         intercept = str_detect(.variable, "Intercept"))%>%
  mutate(name = case_when(
    endsWith(.variable, "Intercept") ~ "Intercept",
    endsWith(.variable, "snowfrac_std") ~ "Snow fraction",
    endsWith(.variable, "clay_std") ~ "Clay fraction", 
    endsWith(.variable, "intact_std") ~ "Intactness",
    endsWith(.variable, "intact_std:eli_tau_std") ~ "Intactness*ELI", 
    endsWith(.variable, "eli_tau_std") ~ "ELI")) 
## casting a specific order
#mutate(name = factor(name, levels = c("Targeted condition", "Easement effect", 
#                                      "Post implementation", "Intercept", "Contributing Area", 
#                                      "Slope", "Value", "SPEI")))

ggbase = ggplot(baseMarg, aes(x = .value, y = fct_rev(name), fill = component)) +
  geom_vline(xintercept = 0) +
  stat_halfeye(aes(slab_alpha = intercept), 
               .width = c(0.8, 0.95), point_interval = "median_hdi") +
  #scale_fill_viridis_d(option = "viridis", end = 0.6) +
  scale_slab_alpha_discrete(range = c(1, 0.4)) +
  guides(fill = "none", slab_alpha = "none") +
  labs(x = "Effect size", y = "Variable") +
  facet_wrap(vars(component), ncol = 1, scales = "free_y")+
  ggtitle("Baseflow")
ggbase

odds_effect = function(posts, var.name){
  ## filter df for only draws of interest
  draws = posts%>%
    filter(.variable == var.name)
  ## central tendency and SD
  med = median(draws$.value)
  mean = mean(draws$.value)
  sd = sd(draws$.value)
  intVals = baseMarg%>%
    filter(.variable == "b_Intercept")
  int = median(intVals$.value)
  
  est = plogis(int + mean) - plogis(int)
  upper = plogis(int + (mean + 1.96*sd)) - plogis(int)
  lower = plogis(int + (mean - 1.96*sd)) - plogis(int)
  
  return( c(est, lower, upper))
}

draws = baseMarg%>%
  filter(.variable == "b_snowfrac_std")
## central tendency and SD
med = median(draws$.value)
mean = mean(draws$.value)
sd = sd(draws$.value)
intVals = baseMarg%>%
  filter(.variable == "b_Intercept")
int = median(intVals$.value)

summary(modBase)

est = plogis(int + mean) - plogis(int)
upper = plogis(int + (mean + 1.96*sd)) - plogis(int)
lower = plogis(int + (mean - 1.96*sd)) - plogis(int)


odds_effect80 = function(posts, var.name){
  ## filter df for only draws of interest
  draws = posts%>%
    filter(.variable == var.name)
  ## central tendency and SD
  med = median(draws$.value)
  mean = mean(draws$.value)
  sd = sd(draws$.value)
  
  est = plogis(int + mean) - plogis(int)
  upper = plogis(int + (mean + 1.282*sd)) - plogis(int)
  lower = plogis(int + (mean - 1.282*sd)) - plogis(int)
  
  return( c(est, lower, upper))
}

basesnow = odds_effect(baseMarg, "b_snowfrac_std")
basesnow
