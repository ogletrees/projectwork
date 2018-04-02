# Date: 2018-04-02
# S Ogletree
# Description: Working through for research poster on April 6, 2018

library(tidyverse)
library(broom)
library(purrr)
library(modelr)
library(lme4)
library(multilevel)
library(stargazer)
library(psych)
library(DescTools)
library(jtools)
library(lmerTest)

# the multilevel data. This is all InCity cbg
mlmdf <- readRDS("../data/cbg_mlm_data.rds")
str(mlmdf)
# there are some col names that are no so great due to combining the city and cbg sets, fix those
names(mlmdf)
mlmdf <- mlmdf %>% rename(ndvi_mean_cbg = mean_ndvi1, popden_cbg = PopDen151, evi_mean_cbg = mean_evi1, savi_mean_cbg = mean_savi1, GEOID_city = GEOID)

# lets get the cols we need for now
df2 <- mlmdf %>% dplyr::select(GEOID = GEOID1, InCity, Urban, MedHHinc, PcU18, CRMCYTOTC:CRMCYMVEH, disad, divindex, popden_cbg, ndvi_mean_cbg, evi_mean_cbg, savi_mean_cbg, city_st, Pop2015, PopDen15, MetroGDP, rate_violentcrime:rate_total, clust4, cluster6, cluster7, PerCapitaOfficers1000, mean_ndvi, mean_evi, mean_savi)
# and then we are going to adjust some variables scale to make interpretation easier
df2 <- df2 %>%  mutate(
  MedHHinc_000 = MedHHinc/1000,
  ndvi_mean_cbg_adj = ndvi_mean_cbg*10,
  evi_mean_cbg_adj = evi_mean_cbg*10,
  savi_mean_cbg_adj = savi_mean_cbg*10,
  PcU18_whole = PcU18 * 100
)

# check descriptives ------------------------------------------------------
options(scipen = 999)
options(digits = 12)

plot(density(log(df2$popden_cbg)))
describe(log(df2$popden_cbg + .1))

# add var for log pop density
df2$log_popden_cbg <- log(df2$popden_cbg + .1)

# make the climate clusters a factor
df2$clust4 <- as.factor(df2$clust4)
levels(df2$clust4)
levels(df2$clust4) <- c("warm_wet", "cool_wet", "warm_dry", "cool_dry")


# grand mean center variables
df3 <- df2 %>% mutate(
  grdctr_ndvi_adj = ndvi_mean_cbg_adj - mean(ndvi_mean_cbg_adj),
  grdctr_MedHHinc000 = MedHHinc_000 - mean(MedHHinc_000, na.rm=T),
  grdctr_disad = disad - mean(disad, na.rm=T),
  grdctr_diver = divindex - mean(divindex, na.rm=T),
  grdctr_U18 = PcU18_whole - mean(PcU18_whole, na.rm=T),
  grdctr_logpopden = log_popden_cbg - mean(log_popden_cbg, na.rm=T)
)


# ignore the grouping -----------------------------------------------------

mod_1a <- lm(df3$CRMCYTOTC ~ df3$ndvi_mean_cbg)
mod_1b <- lm(df3$CRMCYTOTC ~ df3$ndvi_mean_cbg + df3$MedHHinc_000 + df3$disad + df3$divindex + df3$log_popden_cbg + df3$PcU18_whole)

summary.lm(mod_1a)
summary.lm(mod_1b)

par(mfrow=c(2,2))
plot(mod_1a)
plot(mod_1b)

glance(mod_1a)[2];glance(mod_1a)[7] 
glance(mod_1b)[2];glance(mod_1b)[7] 

# add city ----------------------------------------------------------------

mod_2a <- lmer(CRMCYTOTC ~ grdctr_ndvi_adj + (1|city_st), data = df3)
glance(mod_2a)
tidy(mod_2a)
