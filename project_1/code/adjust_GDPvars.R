# Date: 2018-05-26
# S Ogletree
# Description: Adjust GDP variables

library(dplyr)

mdf <- readRDS("../data/full_mlm_datset.rds")

summary(mdf$gndctr_MetroGDP)
summary(mdf$PCgdp15)

# first, grand center per capita GDP
mpdgdp <- mean(mdf$PCgdp15)
mdf$gndctr_PCgdp <- mdf$PCgdp15 - mpdgdp
summary(mdf$gndctr_PCgdp)

mdf$PCgdp000 <- mdf$PCgdp15/1000
summary(mdf$PCgdp000)

mdf$MetroGDPbil <- mdf$MetroGDP/1000
summary(mdf$MetroGDPbil)

mdf$gndctr_PCgdp000 <- mdf$PCgdp000 - mean(mdf$PCgdp000)
mdf$gndctr_MetroGDPbil <- mdf$MetroGDPbil - mean(mdf$MetroGDPbil)
summary(mdf$gndctr_PCgdp000)
summary(mdf$gndctr_MetroGDPbil)

# add the newer cliamte region names
city <- readRDS("../data/city_all.rds")
ccr <- city %>% select(city_st, clust90_4, clustname)
mdf <- mdf %>% left_join(ccr, by = "city_st")
sum(is.na(mdf$clust90_4))

saveRDS(mdf, "../data/mlm_incity_data.rds")
