# Date: 2018-06-07
# S Ogletree
# Description: Continued UTCI prep

library(oce)
library(dplyr)
library(stringr)

macdata <- readRDS("H:/data/Part_2/cbg_UTCIcalc_alldays.rds")
head(macdata)
str(macdata)
macdata$date_time <- paste(str_replace(macdata$dt,"(\\d{4})(\\d{2})(\\d{2})$","\\1/\\2/\\3"), "12:00:00")
tail(macdata)

cbg <- readRDS("../../project_1/data/cbg_all.rds")
head(cbg)
clat <- cbg %>% select(GEOID, lat = INTPTLAT, lon = INTPTLON)
macdata <- macdata %>% left_join(clat, by = c("cbg_id"="GEOID"))
sum(is.na(macdata$lat))

macdata %>% select(cbg_id, date_time, t = t_meanC, f = rh_mean, v10m = ws10m, Kglob = s_rad, lat, lon) %>% saveRDS("H:/data/Part_2/input_BioKlima.rds")


# try to split it up since no other program can handle the biggness
nrow(macdata)/5
mdf <- split(macdata, rep(1:5, 5728018))

write.table(mdf[[1]], "H:/data/Part_2/input_BioKlima_1.txt", sep = ",", row.names = F)
