library(dplyr)

d <- readRDS("../data/city_all.rds")
gdp <- read.csv("../data/gmpPCGDP.csv", stringsAsFactors = F)
str(gdp)
str(d)
d$MSA_fips <- as.character(d$MSA_fips)
d$PCgdp15 <- gdp$X2015[match(d$MSA_fips, gdp$GeoFIPS)]


mm <- readRDS("../data/full_mlm_datset.rds")

# add temp, precip, and per capita GDP

xtra <- d %>% select(city_st, PCgdp15, city_t_mean = mean_an_temp, city_p_mean = mean_an_precip, city_t_range = t_range, city_t_min = min_tmin, city_t_max = max_tmax)

mm <- mm %>% left_join(xtra, by = "city_st")
mm %>% sample_n(12) %>% View()

saveRDS(mm, "../data/full_mlm_datset.rds")
