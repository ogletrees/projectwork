# Date: 2018-05-19
# S Ogletree
# Description: Verify climate cluster categories

city <- readRDS("../data/city_all.rds")

str(city$clust90_4)
city <- city %>% mutate(clustname = case_when(
  clust90_4 == 1 ~ "warm-wet-hi",
  clust90_4 == 2 ~ "warm-dry-hi",
  clust90_4 == 3 ~ "cool-wet-lo",
  clust90_4 == 4 ~ "cool-dry-lo"
))
city$clustname <- as.factor(city$clustname)
levels(city$clustname)
str(city$clustname)

us <- map_data("state")

city %>% filter(clustname == "warm-wet-hi") %>% View()
city %>% ggplot(aes(INTPTLON, INTPTLAT, color= clustname)) + geom_point() 

ggplot() + geom_map(data=us, map=us, aes(x=long, y=lat, map_id=region)) + geom_point(data = city, aes(INTPTLON, INTPTLAT, color= clustname)) + ggtitle("City Climate Clusters based on 3 variables")
ggsave("../figures/CityClimateClusters_20180519.pdf", width = 11)
