# Date: 2018-04-21
# S Ogletree
# Description: Some more exploring

invisible(lapply(c("dplyr", "ggplot2", "janitor", "lme4", "multilevel", "arm"),library, character.only=TRUE))

df <- readRDS("../data/full_mlm_datset.rds")
df_urb <- df %>% filter(Urban == 1)

mod_1 <- lmer(CRMCYPERC ~ gndctr_ndvi_cbg_adj*clust4  + gndctr_MedHHinc000 + gndctr_disad + gndctr_diver + gndctr_U18 + gndctr_logpopden + gndctr_MetroGDP + gndctr_Police + gndctr_rt_violent + (1|city_st), data = df)

mod_2 <- lmer(CRMCYPERC ~ gndctr_ndvi_cbg_adj*clust4  + gndctr_MedHHinc000 + gndctr_disad + gndctr_diver + gndctr_U18 + gndctr_logpopden + gndctr_MetroGDP + gndctr_Police + gndctr_rt_violent + (1|city_st), data = df_urb)

display(mod_1)
display(mod_2)


display(mod_1)

methods(class = "merMod")
round(fixef(mod_1), 6)
min(hatvalues(mod_1))
se.coef(mod_1) # these are the stardard errors for fixed and random (each group)

VarCorr(mod_1)$city_st
vcov(mod_1)
sqrt(diag(vcov(mod_1)))

