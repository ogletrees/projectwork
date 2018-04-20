df <- readRDS("../data/city_all.rds")
library(ggplot2)
library(plotly)

p <- ggplot(df, aes(PerCapitaOfficers1000, rate_propertycrime, text = city_st, color = factor(clust4))) +geom_point()
p

pp <- ggplotly(p)
pp

