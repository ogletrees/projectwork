# Date: 2018-02-22
# S Ogletree
# Description: Prep for other tools

library(dplyr)
library(stringr)
library(lubridate)
library(biometeoR)
library(ggplot2)

df <- read.csv("../../data/climate_wx_data/cbg_level_data_sample.csv", stringsAsFactors = F, colClasses = c("GEOID"="character"))
str(df)

df <- df %>% select(ID = GEOID, date, Ta = tmeanC, RH = rhmean, v = windv, G = srad)
df$time <- "12:00"
df$hour <- "12"
df$min <- "00"
df$Kglob <- df$G

df$yr <- str_sub(df$date, 1, 4)
df$mo <- str_sub(df$date, 5, 6)
df$dy <- str_sub(df$date, 7, 8)
df$date <- str_c(df$dy,df$mo,df$yr, sep = ".")
df$sundate <- str_c(df$yr,df$mo,df$dy, sep = "-")
head(df)

df$t <- df$Ta
df$f <- df$RH
df$v10m <- df$v
#--------------------------------------------
head(df)
cbg_ref <- readRDS("../../data/climate_wx_data/cbg_pt_tz.rds")

df <- df %>% left_join(cbg_ref, by = c("ID" ="GEOID"))
#--------------------------------------------

write.table(df, "../../data/climate_wx_data/cbg_climate_input.txt", sep="\t", quote = F, row.names = F)

df_ray <- df %>% select(ID, date, lat, lon, tzone, time, Ta, RH, v, G)
write.table(df_ray, "../../data/climate_wx_data/cbg_climate_input_RayMan.txt", sep="\t", quote = F, row.names = F)
#--------------------------------------------
rayOut <- read.delim("../../data/climate_wx_data/ray_test_20180222.txt", sep = "\t", skip = 5, stringsAsFactors = F, header = F)
rayOut <- rayOut[-1,]
str(rayOut)
hh <- read.delim("../../data/climate_wx_data/ray_test_20180222.txt", sep = "\t", skip = 3, stringsAsFactors = F, header = F)
hh <- hh[1,]
colnames(rayOut) <- hh[1,]




ggplot(rayOut, aes(Ta)) + geom_density(color = "red") + geom_density(aes(UTCI), color = "blue")
ggplot(rayOut, aes(RH)) + geom_density(color = "red")
ggplot(rayOut, aes(Gact)) + geom_density(color = "red")
ggplot(rayOut, aes(v)) + geom_density(color = "red")
ggplot(rayOut, aes(azim.)) + geom_density(color = "red")
ggplot(rayOut, aes(PET)) + geom_density(color = "red") + geom_density(aes(UTCI), color = "blue") + geom_density(aes(Ta), color = "green")
