library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(purrr)
library(broom)
library(sjmisc)
library(sjPlot)
library(reshape)
library(tidyr)

df <- readRDS("../data/full_mlm_datset.rds")
clist <- df %>% distinct(city_st)

out <- data.frame("NDVIest" = NA, "NDVIse" = NA, "citylist" = NA)

for (i in 1:100) {
  csamp <- df %>% sample_n(1000)
  df_sub <- df %>% filter(city_st %in% csamp$city_st)
  #out[i, 3] <- toString(list(csamp$city_st))
  mm <- lm(CRMCYPERC ~  ndvi_mean_cbg_adj + disad + divindex + log_popden_cbg + MedHHinc_000, data = df_sub)
  out[i,1] <- mm$coefficients[2]
  out[i,2] <- sqrt(diag(vcov(mm)))[2]
}

out %>% ggplot(aes(NDVIest)) + geom_density()
mean(out$NDVIest); mean(out$NDVIse); 1.96 * mean(out$NDVIse)
head(out)
 