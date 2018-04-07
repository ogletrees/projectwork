# Date: 2018-04-03
# S Ogletree
# Description: Constructing the lean multilevel dataset

library(dplyr)

df <- readRDS("../data/cbg_mlm_data.rds")

# there are some col names that are no so great due to combining the city and cbg sets, fix those
names(mlmdf)
mlmdf <- df %>% rename(ndvi_mean_cbg = mean_ndvi1, popden_cbg = PopDen151, evi_mean_cbg = mean_evi1, savi_mean_cbg = mean_savi1, GEOID_city = GEOID)

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

df2$log_popden_cbg <- log(df2$popden_cbg + .1)

# make the climate clusters a factor
df2$clust4 <- as.factor(df2$clust4)
levels(df2$clust4)
levels(df2$clust4) <- c("warm_dry", "cool_wet", "warm_wet", "cool_dry")

str(df2)
names(df2)

df3 <- df2 %>% select(GEOID, InCity, Urban, ndvi_mean_cbg_adj, evi_mean_cbg_adj, savi_mean_cbg_adj, MedHHinc_000, PcU18_whole, log_popden_cbg, divindex, disad, CRMCYTOTC:CRMCYMVEH, city_st, clust4, cluster6, cluster7, PopDen15, Pop2015, MetroGDP, PerCapitaOfficers1000, rate_violentcrime:rate_total, ndvi_mean_city = mean_ndvi, evi_mean_city = mean_evi, savi_mean_city = mean_savi)
names(df3)

# add grand mean centered versions
df4 <- df3 %>% mutate(
  gndctr_ndvi_cbg_adj = ndvi_mean_cbg_adj - mean(ndvi_mean_cbg_adj),
  gndctr_MedHHinc000 = MedHHinc_000 - mean(MedHHinc_000, na.rm=T),
  gndctr_disad = disad - mean(disad, na.rm=T),
  gndctr_diver = divindex - mean(divindex, na.rm=T),
  gndctr_U18 = PcU18_whole - mean(PcU18_whole, na.rm=T),
  gndctr_logpopden = log_popden_cbg - mean(log_popden_cbg, na.rm=T)
)

summary(df4)

df4 <- df4 %>% group_by(city_st) %>% mutate(
  gpc_ndvi = mean(ndvi_mean_cbg_adj),
  gpc_medhh =  mean(MedHHinc_000, na.rm=T),
  gpc_disad = mean(disad, na.rm=T),
  gpc_diver = mean(divindex, na.rm=T),
  gpc_U18 = mean(PcU18_whole, na.rm=T),
  gpc_logpop = mean(log_popden_cbg, na.rm=T)
  ) %>% ungroup()
df4 <- df4 %>% mutate(
  grpctr_ndvi_cbg_adj = ndvi_mean_cbg_adj - gpc_ndvi,
  grpctr_MedHHinc000 = MedHHinc_000 - gpc_medhh,
  grpctr_disad = disad - gpc_disad,
  grpctr_diver = divindex - gpc_diver,
  grpctr_PcU18whole = PcU18_whole - gpc_U18,
  grpctr_log_popden_cbg = log_popden_cbg - gpc_logpop
  )

check1 <- df4 %>% group_by(city_st) %>% summarise(themean = mean(gndctr_ndvi_cbg_adj))
# df4 %>% filter(city_st =="Philadelphia_PA") %>% summarise(thegroupmean = mean(grpctr_ndvi_cbg_adj))
round(check1$themean, 7)
round(mean(df4$grpctr_ndvi_cbg_adj), 7)

# test cause it looks weird
t1 <- df4 %>% group_by(city_st) %>% summarise(momean1 = mean(ndvi_mean_cbg_adj))
t2 <- df4 %>% group_by(city_st) %>% summarise(momean2 = mean(gndctr_ndvi_cbg_adj))
t3 <- df4 %>% group_by(city_st) %>% summarise(momean3 = mean(grpctr_ndvi_cbg_adj))
t1 <- t1 %>% left_join(t2) %>% left_join(t3)
summary(t1)
t1$momean3 <- round(t1$momean3, 7)
# OK, I think that's all good now.
#----------------------------------------------------------
saveRDS(df4, "../data/full_mlm_datset.rds")

summary(df4)
