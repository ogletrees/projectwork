# Date: 2018-06-03
# S Ogletree
# Description: Summary for Larson

library(lme4)
library(dplyr)
library(ggplot2)
library(jtools)
library(broom)

df <- readRDS("../data/mlm_incity_data.rds")

df_na <- df %>% filter(!is.na(gndctr_MedHHinc000)& !is.na(gndctr_disad))

mod_v_06 <- lmer(CRMCYPERC ~ gndctr_ndvi_cbg_adj*clustname + gndctr_MedHHinc000 + gndctr_disad + gndctr_diver + gndctr_logpopden + 
                   gndctr_PCgdp000 + gndctr_rt_violent + (gndctr_ndvi_cbg_adj|city_st), data = df_na)
mod_p_06 <- lmer(CRMCYPROC ~ gndctr_ndvi_cbg_adj*clustname + gndctr_MedHHinc000 + gndctr_disad + gndctr_diver + gndctr_U18 + gndctr_logpopden + 
                   gndctr_rt_property + (gndctr_ndvi_cbg_adj|city_st), data = df_na)

v_fe <- fixef(mod_v_06)
p_fe <- fixef(mod_p_06)
v_vars <- diag(vcov(mod_v_06))
p_vars <- diag(vcov(mod_p_06))
as.data.frame(v_fe) %>% tibble::rownames_to_column(var = "term")

glance(mod_v_06)
tidy(mod_v_06)
tidy(mod_p_06)


v_coef <- coef(mod_v_06)$city_st %>% add_rownames(var = "city_st")
p_coef <- coef(mod_p_06)$city_st %>% add_rownames(var = "city_st")

df_na %>% ggplot(aes(gndctr_ndvi_cbg_adj, CRMCYPERC)) + geom_point(alpha = 0) + geom_abline(aes(slope=gndctr_ndvi_cbg_adj, intercept= `(Intercept)`), data = v_coef, alpha = .2) + labs(x = "NDVI", y = "Violent Crime Index", title = "Violent Crime random slopes")
ggsave("/Volumes/OGLE32GB/output/Larson_20180603/VC_random_slopes.pdf", width = 11)

df_na %>% ggplot(aes(gndctr_ndvi_cbg_adj, CRMCYPERC)) + geom_point(alpha = 0) + geom_abline(aes(slope=gndctr_ndvi_cbg_adj, intercept= `(Intercept)`), data = p_coef, alpha = .2) + labs(x = "NDVI", y = "Property Crime Index", title = "Property Crime random slopes")
ggsave("/Volumes/OGLE32GB/output/Larson_20180603/PC_random_slopes.pdf", width = 11)


interact_plot(mod_v_06, pred = gndctr_ndvi_cbg_adj, modx = clustname, data = df_na, main.title = "Interaction - Violent Crime") # ,interval = T 
ggsave("/Volumes/OGLE32GB/output/Larson_20180603/VC_interaction.pdf", width = 11)
interact_plot(mod_p_06, pred = gndctr_ndvi_cbg_adj, modx = clustname, data = df_na, main.title = "Interaction - Property Crime" ) + theme_apa()
ggsave("/Volumes/OGLE32GB/output/Larson_20180603/PC_interaction.pdf", width = 11)


# plot climate vars
v_clim_int <- tidy(mod_v_06) %>% slice(c(1, 3, 4, 5))
v_clim_int$term <- gsub("clustname", "", v_clim_int$term)
v_clim_int %>% ggplot(aes(term, estimate)) + geom_point() + geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=.1) + geom_hline(yintercept = 0, linetype=2) + labs(title= "Climate Type Intercepts", subtitle= "Violent Crime")

p_clim_int <- tidy(mod_p_06) %>% slice(c(1, 3, 4, 5))
p_clim_int$term <- gsub("clustname", "", p_clim_int$term)
p_clim_int %>% ggplot(aes(term, estimate)) + geom_point() + geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=.1) + geom_hline(yintercept = 0, linetype=2) + labs(title= "Climate Type Intercepts", subtitle= "Property Crime")

p_clim_int$int <- 103.554099 + p_clim_int$estimate
p_clim_int[1, 6] <- 103.554099
p_clim_int %>% ggplot(aes(term, int)) + geom_point() + geom_errorbar(aes(ymin=int-(1.96*std.error), ymax=int+(1.96*std.error)), width=.1)

v_clim_int$int <- 169.86028 + v_clim_int$estimate
v_clim_int[1, 6] <- 169.86028
v_clim_int %>% ggplot(aes(term, int)) + geom_point() + geom_errorbar(aes(ymin=int-(1.96*std.error), ymax=int+(1.96*std.error)), width=.1)




# the positive cities
poscity <- df_na %>% filter(city_st %in% c("Detroit_MI", "Newark_NJ", "Chicago_IL"))
poscity %>% ggplot(aes(gndctr_ndvi_cbg_adj, CRMCYPERC)) + geom_point()
negcity <- df_na %>% filter(!city_st %in% c("Detroit_MI", "Newark_NJ", "Chicago_IL"))
negcity %>% ggplot(aes(gndctr_ndvi_cbg_adj, CRMCYPERC)) + geom_point(alpha=.2)

poscity %>% ggplot(aes(gndctr_ndvi_cbg_adj, CRMCYPERC)) + geom_point(alpha=.2) + geom_smooth() + facet_wrap(~city_st) +labs(title = "Positive Violent Crime Relationship", x = "Mean NDVI (grand mean centered & x10)", y = "Violent Crime Index")
ggsave("/Volumes/OGLE32GB/output/Larson_20180603/PositiveCities.pdf", width = 11)

 
ctylist <- sample(negcity$city_st, 3)
negcity %>% filter(city_st %in% ctylist) %>% ggplot(aes(gndctr_ndvi_cbg_adj, CRMCYPERC)) + geom_point(alpha=.2) + geom_smooth() + facet_wrap(~city_st)
