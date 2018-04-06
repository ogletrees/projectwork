# Date: 2018-04-02
# S Ogletree
# Description: Model approach for poster on April 6, 2018

library(dplyr)
library(ggplot2)
library(broom)
library(lme4)
library(lmerTest)
library(dotwhisker)
library(stargazer)
library(jtools)
library(sjPlot)

# data --------------------------------------------------------------------

d <- readRDS("../data/full_mlm_datset.rds")
# str(d)
# grand mean center level 2 variables
d <- d %>% mutate(
  gndctr_MetroGDP = MetroGDP - mean(MetroGDP),
  gndctr_Police = PerCapitaOfficers1000 - mean(PerCapitaOfficers1000),
  gndctr_rt_total = rate_total - mean(rate_total),
  gndctr_rt_violent = rate_violentcrime - mean(rate_violentcrime),
  gndctr_rt_property = rate_propertycrime - mean(rate_propertycrime)
)

# also a dataset with only urban cbg
d_urb <- d %>% filter(Urban == 1)

#d %>% group_by(city_st) %>% do(tidy(cor.test(.$gndctr_ndvi_cbg_adj, .$CRMCYTOTC))) %>% ggplot(aes(city_st, estimate)) + geom_point()

# modeling ----------------------------------------------------------------


mod_1a <- lme4::lmer(CRMCYTOTC ~ 1 + (1|city_st), data = d)
mod_1b <- lme4::lmer(CRMCYPERC ~ 1 + (1|city_st), data = d)
mod_1c <- lme4::lmer(CRMCYPROC ~ 1 + (1|city_st), data = d)

mod_2a <- lme4::lmer(CRMCYTOTC ~ gndctr_ndvi_cbg_adj + (1|city_st), data = d)
mod_2b <- lme4::lmer(CRMCYPERC ~ gndctr_ndvi_cbg_adj + (1|city_st), data = d)
mod_2c <- lme4::lmer(CRMCYPROC ~ gndctr_ndvi_cbg_adj + (1|city_st), data = d)

mod_3a <- lme4::lmer(CRMCYTOTC ~ gndctr_ndvi_cbg_adj + gndctr_MedHHinc000 + gndctr_disad + gndctr_diver + gndctr_U18 + gndctr_logpopden + (1|city_st), data = d)
mod_3b <- lme4::lmer(CRMCYPERC ~ gndctr_ndvi_cbg_adj + gndctr_MedHHinc000 + gndctr_disad + gndctr_diver + gndctr_U18 + gndctr_logpopden +(1|city_st), data = d)
mod_3c <- lme4::lmer(CRMCYPROC ~ gndctr_ndvi_cbg_adj + gndctr_MedHHinc000 + gndctr_disad + gndctr_diver + gndctr_U18 + gndctr_logpopden + (1|city_st), data = d)

mod_4a <- lme4::lmer(CRMCYTOTC ~ gndctr_ndvi_cbg_adj + gndctr_MedHHinc000 + gndctr_disad + gndctr_diver + gndctr_U18 + gndctr_logpopden + 
                       + MetroGDP + PerCapitaOfficers1000 + clust4 + rate_total + (1|city_st), data = d)
mod_4b <- lme4::lmer(CRMCYPERC ~ gndctr_ndvi_cbg_adj + gndctr_MedHHinc000 + gndctr_disad + gndctr_diver + gndctr_U18 + gndctr_logpopden +
                       + MetroGDP + PerCapitaOfficers1000 + clust4 + rate_violentcrime + (1|city_st), data = d)
mod_4c <- lme4::lmer(CRMCYPROC ~ gndctr_ndvi_cbg_adj + gndctr_MedHHinc000 + gndctr_disad + gndctr_diver + gndctr_U18 + gndctr_logpopden + 
                       + MetroGDP + PerCapitaOfficers1000 + clust4 + rate_propertycrime + (1|city_st), data = d)

mod_5a <- lme4::lmer(CRMCYTOTC ~ gndctr_ndvi_cbg_adj * clust4 + gndctr_MedHHinc000 + gndctr_disad + gndctr_diver + gndctr_U18 + gndctr_logpopden + 
                       + gndctr_MetroGDP + gndctr_Police + gndctr_rt_total + (1|city_st), data = d)
mod_5b <- lme4::lmer(CRMCYPERC ~ gndctr_ndvi_cbg_adj * clust4 + gndctr_MedHHinc000 + gndctr_disad + gndctr_diver + gndctr_U18 + gndctr_logpopden +
                       + gndctr_MetroGDP + gndctr_Police + gndctr_rt_violent + (1|city_st), data = d)
mod_5c <- lme4::lmer(CRMCYPROC ~ gndctr_ndvi_cbg_adj * clust4 + gndctr_MedHHinc000 + gndctr_disad + gndctr_diver + gndctr_U18 + gndctr_logpopden + 
                       + gndctr_MetroGDP + gndctr_Police + gndctr_rt_property + (1|city_st), data = d)

# examine -----------------------------------------------------------------

sjt.lmer(mod_2a, mod_3a, mod_4a, mod_5a, p.kr = F, show.aic = T, separate.ci.col = F, show.icc = F, p.numeric = F)
summary(mod_2a)
sjp.lmer(mod_5a,mod_5b, type = "fe", p.kr = F, show.intercept = T, y.offset = .3)

alab <- c("Percent under 18 years", "City violent crime rate (per 1,000)", "City total crime rate (per 1,000)", "City property crime rate (per 1,000)", "Police per 1,000", "NDVI X warm & dry", "NDVI X cool & wet", "NDVI X cool & dry", "NDVI - greenspace", "Metro GDP", "Median household income (000's)", "Population density (log)", "Diveristy index", "Disadvantage index", "Climate Region - warm & dry", "Climate Region - cool & wet", "Climate Region - cool & dry")

plot_models(mod_5a, mod_5b, mod_5c, m.labels = c("Total Crime Index", "Violent Crime Index", "Property Crime Index"), title = "Fixed effects estimates", axis.labels = rev(alab))
ggsave("../figures/coef_plot_mod5.pdf", width = 11)

fixef(mod_5a)
r_m5a <- ranef(mod_5a)$city_st
sd(r_m5a$`(Intercept)`)
tidy(mod_5a)     

interact_plot(mod_5a, pred = gndctr_ndvi_cbg_adj, modx = clust4, x.label = "Mean NDVI (mean centered)", y.label = "Total Crime Index", main.title = "Interaction between NVDI and Climate Region", legend.main = "Climate Region")
ggsave("../figures/interactplot_ndvi_climate.pdf", width = 11)

interact_plot(mod_5b, pred = gndctr_ndvi_cbg_adj, modx = clust4, x.label = "Mean NDVI (mean centered)", y.label = "Violent Crime Index", main.title = "Interaction between NVDI and Climate Region", legend.main = "Climate Region")
ggsave("../figures/interactplot_ndvi_climate_vc.pdf", width = 11)

interact_plot(mod_5c, pred = gndctr_ndvi_cbg_adj, modx = clust4, x.label = "Mean NDVI (mean centered)", y.label = "Property Crime Index", main.title = "Interaction between NVDI and Climate Region", legend.main = "Climate Region")
ggsave("../figures/interactplot_ndvi_climate_pc.pdf", width = 11)


sjt.lmer(mod_5a, mod_5b, mod_5c, p.kr = F, separate.ci.col = F, show.icc = F, show.aic = T, p.numeric = F, show.dev = T, depvar.labels = c("Total Crime Index", "Violent Crime Index", "Property Crime Index") )

# slopes varying ----------------------------------------------------------
levels(d$clust4)
levels(d$clust4) <- c("warm_dry", "cool_wet", "warm_wet", "cool_dry")

mod_6a <- lme4::lmer(CRMCYTOTC ~ gndctr_ndvi_cbg_adj * clust4 + gndctr_MedHHinc000 + gndctr_disad + gndctr_diver + gndctr_U18 + gndctr_logpopden + 
                       + gndctr_MetroGDP + gndctr_Police + gndctr_rt_total + (gndctr_ndvi_cbg_adj|city_st), data = d)
summary(mod_6a)
# this will get varying slopes for the estimate of NDVI on crime
rs_ndvi <- coef(mod_6a)$city_st


# diagnostics -------------------------------------------------------------

plot(fitted(mod_5a), residuals(mod_5a), main = "total crime")
plot(fitted(mod_5b), residuals(mod_5b), main = "violent crime")
plot(fitted(mod_5c), residuals(mod_5c), main = "property crime")

qqnorm(residuals(mod_5c))
plot(mod_5a)
plot(fitted(mod_5a), mod_5a@frame$CRMCYTOTC)
plot(density(sqrt(d$CRMCYTOTC)))
base::summary(sqrt(d$CRMCYTOTC))

mod_7a <- lme4::lmer(sqrt(CRMCYTOTC) ~ gndctr_ndvi_cbg_adj * clust4 + gndctr_MedHHinc000 + gndctr_disad + gndctr_diver + gndctr_U18 + gndctr_logpopden + 
                       + gndctr_MetroGDP + gndctr_Police + gndctr_rt_total + (1|city_st), data = d)
plot(mod_7a)
summary(mod_7a)
