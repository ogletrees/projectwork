# Date: 2018-04-04
# S Ogletree
# Description: did I get the climate categories wrong?

library(dplyr)
library(ggplot2)
library(broom)
library(lme4)
library(lmerTest)
library(dotwhisker)
library(stargazer)
library(jtools)
library(sjPlot)

# data --------------------------------------------------------------------

d <- readRDS("../data/full_mlm_datset.rds")
# str(d)
# grand mean center level 2 variables
d <- d %>% mutate(
  gndctr_MetroGDP = MetroGDP - mean(MetroGDP),
  gndctr_Police = PerCapitaOfficers1000 - mean(PerCapitaOfficers1000),
  gndctr_rt_total = rate_total - mean(rate_total),
  gndctr_rt_violent = rate_violentcrime - mean(rate_violentcrime),
  gndctr_rt_property = rate_propertycrime - mean(rate_propertycrime)
)

# also a dataset with only urban cbg
d_urb <- d %>% filter(Urban == 1)

# save a list of cities and their climate category
city_cli <- d %>% select(city_st, clust4, cluster6, cluster7) %>% distinct(city_st, .keep_all = T)

levels(d$clust4) <- c("warm_dry", "cool_wet", "warm_wet", "cool_dry")
str(d)

city <- readRDS("../data/city_all.rds")
ctp <- city %>% select(city_st, mean_an_temp, mean_an_precip)
checkk <- city_cli %>% left_join(ctp)

checkk %>% ggplot(aes(mean_an_temp, mean_an_precip)) + geom_point(aes(color = clust4))

# save back out
saveRDS(d, "../data/full_mlm_datset.rds")
