# Date: 2018-05-02
# S Ogletree
# Description: daily weather for cbg's

library(dplyr)
library(weathermetrics)
library(tidyr)
library(ggplot2)

# get all cbg's - should have lat lon
cbg_all <- readRDS("../../data/cbg_data/cbg_base.rds")
str(cbg_all)

# the GridMET data, one for each variable
tmax <- readRDS("../../data/GridMET_data/tmax.rds")
tmin <- readRDS("../../data/GridMET_data/tmin.rds")
rmax <- readRDS("../../data/GridMET_data/rhmax.rds")
rmin <- readRDS("../../data/GridMET_data/rhmin.rds")
srad <- readRDS("../../data/GridMET_data/srad.rds")
wind <- readRDS("../../data/GridMET_data/windvel.rds")

# convert to long format
tmax_long <- tmax %>% gather(dt, t_maxK, -cbg_id)
head(tmax_long)
tmax_long$dt <- gsub("tmax_", "", tmax_long$dt)
head(tmax_long)
tmin_long <- tmin %>% gather(dt, t_minK, -cbg_id)
tmin_long$dt <- gsub("tmin_", "", tmin_long$dt)

temps_cbg <- tmax_long %>% left_join(tmin_long, by = c("cbg_id", "dt"))
head(temps_cbg)
temps_cbg$t_meanK <- (temps_cbg$t_max + temps_cbg$t_min)/2

# inspect the progress
#temps_cbg %>% filter(cbg_id == "010730051031") %>% ggplot(aes(dt, t_meanK, group=1)) + geom_line()

# convert to celsius
temps_cbg$t_maxC <- kelvin.to.celsius(temps_cbg$t_maxK, round = 2)
temps_cbg$t_minC <- kelvin.to.celsius(temps_cbg$t_minK, round = 2)
temps_cbg$t_meanC <- kelvin.to.celsius(temps_cbg$t_meanK, round = 2)
head(temps_cbg)

# relative humidity
rmax_long <- rmax %>% gather(dt, r_max, -cbg_id)
rmax_long$dt <- gsub("rhmax_", "", rmax_long$dt)
head(rmax_long)
rmin_long <- rmin %>% gather(dt, r_min, -cbg_id)
rmin_long$dt <- gsub("rhmin_", "", rmin_long$dt)
head(rmin_long)

rh_cbg <- rmax_long %>% left_join(rmin_long, by = c("cbg_id", "dt")) %>% mutate(rh_mean = (r_max + r_min)/2)
head(rh_cbg)

# wind
ws_long <- wind %>% gather(dt, ws10m, -cbg_id)
head(ws_long)
ws_long$dt <- gsub("windvel_", "", ws_long$dt)

# solar
srad_long <- srad %>% gather(dt, s_rad, -cbg_id)
head(srad_long)
srad_long$dt <- gsub("srad_", "", srad_long$dt)

# build the whole dataset

wx_data <- temps_cbg %>% left_join(rh_cbg, by = c("cbg_id", "dt"))
head(wx_data)
wx_data <- wx_data %>% left_join(ws_long, by = c("cbg_id", "dt"))
head(wx_data)
wx_data <- wx_data %>% left_join(srad_long, by = c("cbg_id", "dt"))
head(wx_data)

# write out the wx data
saveRDS(wx_data, "../../data/GridMET_data/cbg_wx_data_long.rds")
