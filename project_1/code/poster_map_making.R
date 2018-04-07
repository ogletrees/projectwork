# Date: 2018-04-04
# S Ogletree
# Description: Make a map of cities colored by region

libr
library(dplyr)
library(sf)
library(ggplot2)
library(maps)
df <- readRDS("../data/city_all.rds")
str(df)
df <- df %>% select(city_st, clust4, lat =INTPTLAT, lon = INTPTLON)

sdf <- st_as_sf(df, coords = c(x = "lon", y = "lat"))
str(sdf)

ggplot(sdf) + geom_sf()


usa <- map_data("usa")
str(usa)
states <- st_as_sf(map("usa"))
str(states)

st_crs(sdf)
sdf <- sdf %>% st_set_crs(5070)
st_crs(states)
states <- states %>% st_transform(5070)

ggplot(states) + geom_sf() + geom_sf(data = sdf)
ggplot() + geom_sf(data = states)  + geom_sf(data = sdf, size = 1, aes(fill = factor(clust4), color = NA))  + ggthemes::theme_map()+ theme(panel.grid.major = element_line(colour = 'transparent')) 

state2 <- map_data("state")
ggplot(data = state2) + 
  geom_polygon(aes(x = long, y = lat, group = group), color = "black", fill = "white") + 
  coord_fixed(1.3) +
  geom_point(data = df, aes(x = lon, y = lat, color = factor(clust4))) +
   ggthemes::theme_map()+ theme(panel.grid.major = element_line(colour = 'transparent'))
ggsave("H:/output/poster_GRADS18/city_pt_region.pdf", width = 15, useDingbats=FALSE)

df2 <- readRDS("../data/city_all.rds")
df2 %>% group_by(clust4) %>% summarise(tmm = mean(mean_an_temp), pmm = mean(mean_an_precip)) %>% ggplot(aes(tmm, pmm, color=factor(clust4))) + geom_point()
