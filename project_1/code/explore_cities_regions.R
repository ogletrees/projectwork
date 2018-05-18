library(dplyr)

df <- readRDS("../data/city_all.rds")
head(df)

kgc <- df %>% select(city_st, Pop2015, PopDen15, clim_class, ALAND_SQMI, mean_an_temp, mean_an_precip, mean_ndvi, clust4)
kgc <- kgc %>% mutate(KGmain = substring(clim_class, 1, 1))
head(kgc)
table(kgc$KGmain)
table(kgc$clust4)
kgc <- kgc %>% mutate(KGmain2 = substring(clim_class, 1, 2))
table(kgc$KGmain2)

p1 <- kgc %>% filter(clust4 == 1) %>% select(city_st, PopDen15) %>% arrange(PopDen15)
p2 <- kgc %>% filter(clust4 == 2) %>% select(city_st, PopDen15) %>% arrange(PopDen15)
p3 <- kgc %>% filter(clust4 == 3) %>% select(city_st, PopDen15) %>% arrange(PopDen15)
p4 <- kgc %>% filter(clust4 == 4) %>% select(city_st, PopDen15) %>% arrange(PopDen15)
p1
p2
p3
p4
mean(df$PopDen15)
