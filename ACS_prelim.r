# Date: 2017-12-20
# S Ogletree
# Description: Checking out the ACS tables

library(tidycensus)
library(dplyr)
v15 <- load_variables(2015, "acs5", cache = TRUE)
# v16 <- load_variables(2016, "acs5", cache = TRUE)
est15 <- v15 %>% filter(grepl("E$", name))
est15$id <- row.names(est15)

roi <- c(362, 363, 364, 365, 560, 561, 562, 591, 592, 593, 594, 602:604, 612, 613, 614,6:9, 30:33, 8115:8130,9462:9469,9477:9482,10660:10672,11217,14348:14354,6578:6583)

voi <- est15 %>% filter(id %in% roi)
v_pop <- voi[9:25,]
v_age <- voi[1:8,]
v_ed <- voi[32:47,]
v_famHH <- voi[26:31,]
v_inc_pov <- voi[48:75,]
v_employ <- voi[76:82,]
