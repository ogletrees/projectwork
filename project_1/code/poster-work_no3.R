# Date: 2018-04-01
# S Ogletree
# Description: Looking at interactions between level 2 and level 1

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
mlmdf <- readRDS("../../data/project_data/cbg_mlm_data.rds")
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
df_desc <- describe(df2)
df_desc[, c(2:5,8,9,11,12)]
plot(density(log(df2$popden_cbg)))
describe(log(df2$popden_cbg + .1))

# add var for log pop density
df2$log_popden_cbg <- log(df2$popden_cbg + .1)

# grand mean center variables
df3 <- df2 %>% mutate(
  grand_ndvi_adj = ndvi_mean_cbg_adj - mean(ndvi_mean_cbg_adj),
  grand_MedHHinc000 = MedHHinc_000 - mean(MedHHinc_000, na.rm=T),
  grand_disad = disad - mean(disad, na.rm=T),
  grand_diver = divindex - mean(divindex, na.rm=T),
  grand_U18 = PcU18_whole - mean(PcU18_whole, na.rm=T),
  grand_logpopden = log_popden_cbg - mean(log_popden_cbg, na.rm=T)
)
df3$clust4 <- as.factor(df3$clust4)

# interaction models, just NDVI and moderator ------------------------------------------------------

mod_m1 <- lmer(CRMCYTOTC ~ grand_ndvi_adj * rate_total +  (grand_ndvi_adj|city_st), data = df3)
interact_plot(mod_m1, pred = grand_ndvi_adj, modx = rate_total, main.title = "Relationship of NDVI on Crime, moderated by City total crime rate", x.label = "Mean NDVI (grand mean centered)", y.label = "Crime Index - Total Crime")
ggsave("int_plot_01_city rate.pdf", width = 11)

mod_m2 <- lmer(CRMCYTOTC ~ grand_ndvi_adj * MetroGDP +  (grand_ndvi_adj|city_st), data = df3)
interact_plot(mod_m2, pred = grand_ndvi_adj, modx = MetroGDP, main.title = "Relationship of NDVI on Crime, moderated by Metro GDP", x.label = "Mean NDVI (grand mean centered)", y.label = "Crime Index - Total Crime")
ggsave("int_plot_01_GDP.pdf", width = 11)

mod_m3 <- lmer(CRMCYTOTC ~ grand_ndvi_adj * PerCapitaOfficers1000 +  (grand_ndvi_adj|city_st), data = df3)
interact_plot(mod_m3, pred = grand_ndvi_adj, modx = PerCapitaOfficers1000, main.title = "Relationship of NDVI on Crime, moderated by Police per 1,000 poulation", x.label = "Mean NDVI (grand mean centered)", y.label = "Crime Index - Total Crime")
ggsave("int_plot_01_police.pdf", width = 11)

mod_m4 <- lmer(CRMCYTOTC ~ grand_ndvi_adj * clust4 +  (grand_ndvi_adj|city_st), data = df3)
interact_plot(mod_m4, pred = grand_ndvi_adj, modx = clust4, main.title = "Relationship of NDVI on Crime, moderated by climate region", x.label = "Mean NDVI (grand mean centered)", y.label = "Crime Index - Total Crime")
ggsave("int_plot_01_climate.pdf", width = 11)


# try interaction with controls -------------------------------------------

mod_a1 <- lme4::lmer(CRMCYTOTC ~ grand_ndvi_adj * rate_total + grand_MedHHinc000 + grand_disad + grand_diver + grand_logpopden + (grand_ndvi_adj|city_st), data = df3)
summary(mod_a1)
mod_a1_alt <- lmer(CRMCYTOTC ~ grand_ndvi_adj + rate_total + grand_MedHHinc000 + grand_disad + grand_diver + grand_logpopden + (grand_ndvi_adj|city_st), data = df3)
glance(mod_a1_alt)
glance(mod_a1)
anova(mod_a1_alt, mod_a1)

summary(mod_m1)
summary(mod_a1)
interact_plot(mod_a1, pred = grand_ndvi_adj, modx = rate_total)
