library(tidyverse)

df <- readRDS("../data/cbg_mlm_data.rds")

df$clust4 <- as.factor(df$clust4)
levels(df$clust4)
levels(df$clust4) <- c("warm_dry", "cool_wet", "warm_wet", "cool_dry")

df %>% select(city_st, mean_an_temp, mean_an_precip,  mean_ndvi1, climate_region = clust4) %>% group_by(climate_region) %>% summarise(mean_temp = mean(mean_an_temp), mean_precip = mean(mean_an_precip)/10, mean_ndvi = mean(mean_ndvi1)*100) %>% ggplot(aes(climate_region, mean_temp)) + geom_point(color="red", size=3) + geom_point(aes(y = mean_precip), color="blue", size=3) + geom_point(aes(y = mean_ndvi), color="green", size=3)

p <- df %>% select(city_st, mean_an_temp, mean_an_precip,  mean_ndvi1, climate_region = clust4) %>% group_by(climate_region) %>% summarise(mean_temp = mean(mean_an_temp), mean_precip = mean(mean_an_precip)/10, mean_ndvi = mean(mean_ndvi1)*100) %>% gather(measure, value, -climate_region)

plot(p$climate_region, p$value)
p %>% ggplot(aes(climate_region, value)) + geom_point(aes(color=measure), size=3) + colScale
ggsave("H:/output/poster_GRADS18/region_measures.pdf", useDingbats=F)

myColors <- c("green", "blue", "red")
names(myColors) <- levels(p$measure)
colScale <- scale_colour_manual(name = "climate_region",values = myColors)


tmm <- df %>% group_by(city_st) %>% summarise(vcmean = mean(CRMCYPERC))
mean(tmm$vcmean)
sd(tmm$vcmean)
