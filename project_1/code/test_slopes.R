library(jtools)
library(lme4)
library(dplyr)
library(ggplot2)
city <- readRDS("../data/city_all.rds")
df <- readRDS("../data/mlm_incity_data.rds")

df_na <- df %>% filter(!is.na(gndctr_MedHHinc000)& !is.na(gndctr_disad))

df_urban <- df %>% filter(Urban == 1)

df_urban_nona <- df_na %>% filter(Urban == 1)

mod_v_06 <- lmer(CRMCYPERC ~ gndctr_ndvi_cbg_adj*clustname + gndctr_MedHHinc000 + gndctr_disad + gndctr_diver + gndctr_logpopden + 
                   gndctr_PCgdp000 + gndctr_rt_violent + (gndctr_ndvi_cbg_adj|city_st), data = df_na)
mod_p_06 <- lmer(CRMCYPROC ~ gndctr_ndvi_cbg_adj*clustname + gndctr_MedHHinc000 + gndctr_disad + gndctr_diver + gndctr_U18 + gndctr_logpopden + 
                   gndctr_rt_property + (gndctr_ndvi_cbg_adj|city_st), data = df_na)

# sim_slopes(mod_v_06, pred = gndctr_ndvi_cbg_adj, modx = clustname, data = df_na, johnson_neyman = F)
vcov(mod_v_06)
summ(mod_v_06)
summ(mod_p_06)
interact_plot(mod_v_06, pred = gndctr_ndvi_cbg_adj, modx = clustname, data = df_na, main.title = "Interaction - Violent Crime",interval = T )
interact_plot(mod_p_06, pred = gndctr_ndvi_cbg_adj, modx = clustname, data = df_na, main.title = "Interaction - Property Crime" ,interval = T) + theme_apa()

p_preds <- coef(mod_p_06)$city_st
v_preds <- coef(mod_v_06)$city_st
head(preds)
plot(density(p_preds$gndctr_ndvi_cbg_adj))
plot(density(v_preds$gndctr_ndvi_cbg_adj))
dd <- density(preds$gndctr_ndvi_cbg_adj)
str(dd)
quantile(p_preds$gndctr_ndvi_cbg_adj,probs=c(0.25,0.75), type=5)
quantile(v_preds$gndctr_ndvi_cbg_adj,probs=c(0.05,0.95), type=5)
p_preds %>% ggplot(aes(gndctr_ndvi_cbg_adj)) + geom_density() + geom_vline(xintercept = -80.38124) + geom_vline(xintercept = -21.34967) + labs(title = "Property Crime - NDVI Slopes - 95%")
v_preds %>% ggplot(aes(gndctr_ndvi_cbg_adj)) + geom_density() + geom_vline(xintercept = -61.1582336) + geom_vline(xintercept = 0.7819035) + labs(title = "Violent Crime - NDVI Slopes - 95%")
