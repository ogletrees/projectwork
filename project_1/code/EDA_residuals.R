# Date: 2018-04-14
# S Ogletree
# Description: Modeling exploration based on Bristol Centre for Multilevel Modeling

library(lme4)
library(dplyr)
library(ggplot2)
library(plotly)
df <- readRDS("../data/full_mlm_datset.rds")

mod1 <- lmer(CRMCYPERC ~ 1 + (1|city_st), data = df)
moderesid <- residuals(mod1)$city_st
head(residuals(mod1, level = 2), 20)

summary(mod1)
# get the predicted intercepts for cities. The predicted mean of crime for city based on cbg units in city.
ccc <- coef(mod1)$city_st
# calculate the residuals at level 2, the difference between the model fixed effect (intercept) and the predicted for the city
ccc$rr <- ccc$`(Intercept)`-146.107
# rank them
ccc$rrank <- rank(ccc$rr)
# add the city names as a col
ccc$city_st <- row.names(ccc)
# plot the residuals
p <- ggplot(ccc, aes(rrank,rr, text = city_st )) + geom_point() + ggtitle("L2 residuals - Violent Crime Index")
# and an interactive version
ggplotly(p)

# # or
# cm <- df %>% group_by(city_st) %>% summarize(cmean = mean(CRMCYPERC))
# cm$rres <- cm$cmean-146.107 
# cm %>% mutate(rrank = rank(rres)) %>% ggplot(aes(rrank, rres)) + geom_point()
# # actually these are pretty much the same

# filter by climate region and look at the residuals for level 2
creg <- df %>% group_by(city_st) %>% summarise(clim = first(clust4))
ccc <- ccc %>% left_join(creg)

c1 <- ccc %>% filter(clim == "warm_dry")
# plot the residuals
p1 <- ggplot(ccc, aes(rrank,rr, text = city_st )) + geom_point() + ggtitle("L2 residuals - Violent Crime Index") + facet_wrap(~clim) + geom_hline(yintercept = 0, linetype = 2)
# and an interactive version
ggplotly(p1)

p1
plot(density(ccc$rr))

# level 1 residuals can be obtained with the residuals function
L1res <- as.data.frame(residuals(mod1))
head(L1res)
L1res$rrank <- rank(L1res$`residuals(mod1)`)
ggplotly(L1res %>% ggplot(aes(rrank, `residuals(mod1)`)) + geom_point())
plot(density(L1res$`residuals(mod1)`))
