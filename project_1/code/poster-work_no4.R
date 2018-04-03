# Date: 2018-04-03
# S Ogletree
# Description: Models for poster presentation

library(dplyr)
library(ggplot2)
library(broom)
library(lme4)
library(lmerTest)
library(dotwhisker)
library(stargazer)
library(jtools)

# data --------------------------------------------------------------------

d <- readRDS("../data/full_mlm_datset.rds")
str(d)
# also revert the levels of clust4
levels(d$clust4) <- c("1", "2", "3", "4")
# also a dataset with only urban cbg
d_urb <- d %>% filter(Urban == 1)


# initial EDA -------------------------------------------------------------
# check in on the correlations
dcor <- round(d %>% select_if(is.numeric) %>% cor(use = "complete.obs"), 3)
# corrplot::corrplot(dcor)


# modeling ----------------------------------------------------------------
# not considering the city (group), all cbg's
mod_1a <- lm(CRMCYTOTC ~ gndctr_ndvi_cbg_adj, data = d)
mod_1b <- lm(CRMCYTOTC ~ gndctr_ndvi_cbg_adj + gndctr_MedHHinc000 + gndctr_disad + gndctr_diver + gndctr_U18 + gndctr_logpopden, data = d)
# now with only the urban ones
mod_1c <- lm(CRMCYTOTC ~ gndctr_ndvi_cbg_adj, data = d_urb)
mod_1d <- lm(CRMCYTOTC ~ gndctr_ndvi_cbg_adj + gndctr_MedHHinc000 + gndctr_disad + gndctr_diver + gndctr_U18 + gndctr_logpopden, data = d_urb)

tidy(mod_1a)
tidy(mod_1b)
tidy(mod_1c)
tidy(mod_1d)
dwplot(list(mod_1a, mod_1b, mod_1c, mod_1d), show_intercept = TRUE) %>% 
  relabel_predictors(c(gndctr_ndvi_cbg_adj = "Mean NDVI",                       
                       gndctr_MedHHinc000 = "Median HH Income (1,000)", 
                       gndctr_disad = "Disadvantage Index", 
                       gndctr_diver = "Diversity Index", 
                       gndctr_U18 = "% Under 18 years", 
                       gndctr_logpopden = "Poulation Density (log)")) +
  xlab("Coefficient Estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("Predicting Crime Index -Total")
ggsave("../figures/no_city_coeffs.pdf", width = 11)


# multilevel models -------------------------------------------------------
# null model
mod_2a <- lmer(CRMCYTOTC ~ 1 + (1|city_st), data = d)
mod_2b <- lmer(CRMCYTOTC ~ 1 + (1|city_st), data = d_urb)

summary(mod_2a)
summary(mod_2b)
# just add the IV of interest
mod_2c <- lmer(CRMCYTOTC ~ gndctr_ndvi_cbg_adj + (1|city_st), data = d)
mod_2d <- lmer(CRMCYTOTC ~ gndctr_ndvi_cbg_adj + (1|city_st), data = d_urb)

# add all level 1 covariates
mod_2e <- lmer(CRMCYTOTC ~ gndctr_ndvi_cbg_adj + gndctr_MedHHinc000 + gndctr_disad + gndctr_diver + gndctr_U18 + gndctr_logpopden + (1|city_st), data = d)
mod_2f <- lmer(CRMCYTOTC ~ gndctr_ndvi_cbg_adj + gndctr_MedHHinc000 + gndctr_disad + gndctr_diver + gndctr_U18 + gndctr_logpopden + (1|city_st), data = d_urb)

# add level 2 covariates too!
mod_2g <- lmer(CRMCYTOTC ~ gndctr_ndvi_cbg_adj + gndctr_MedHHinc000 + gndctr_disad + gndctr_diver + gndctr_U18 + gndctr_logpopden + MetroGDP + PerCapitaOfficers1000 + clust4 + rate_total + (1|city_st), data = d)
mod_2h <- lmer(CRMCYTOTC ~ gndctr_ndvi_cbg_adj + gndctr_MedHHinc000 + gndctr_disad + gndctr_diver + gndctr_U18 + gndctr_logpopden + MetroGDP + PerCapitaOfficers1000 + clust4 + rate_total + (1|city_st), data = d_urb)


# investigate the models --------------------------------------------------

sjPlot::sjt.lmer(mod_2c, mod_2e, mod_2g)

# add the interaction for climate -----------------------------------------

mod_2i <- lme4::lmer(CRMCYTOTC ~ gndctr_ndvi_cbg_adj * clust4 + gndctr_MedHHinc000 + gndctr_disad + gndctr_diver + gndctr_U18 + gndctr_logpopden + MetroGDP + PerCapitaOfficers1000 + rate_total + (1|city_st), data = d)
summary(mod_2i)
sjPlot::sjt.lmer(mod_2i, p.kr = F)
vcov(mod_2i)
# try an interaction plot
interact_plot(mod_2i, pred = gndctr_ndvi_cbg_adj, modx = clust4, modxvals = c("1", "2", "3", "4"))

mod_2j <- lme4::lmer(CRMCYPERC ~ gndctr_ndvi_cbg_adj * clust4 + gndctr_MedHHinc000 + gndctr_disad + gndctr_diver + gndctr_U18 + gndctr_logpopden + MetroGDP + PerCapitaOfficers1000 + rate_violentcrime + (1|city_st), data = d)
summary(mod_2j)
sjPlot::sjt.lmer(mod_2j, p.kr = F)
vcov(mod_2j)
# try an interaction plot
interact_plot(mod_2j, pred = gndctr_ndvi_cbg_adj, modx = clust4, modxvals = c("1", "2", "3", "4"))

mod_2k <- lme4::lmer(CRMCYPROC ~ gndctr_ndvi_cbg_adj * clust4 + gndctr_MedHHinc000 + gndctr_disad + gndctr_diver + gndctr_U18 + gndctr_logpopden + MetroGDP + PerCapitaOfficers1000 + rate_propertycrime + (1|city_st), data = d)
summary(mod_2k)
sjPlot::sjt.lmer(mod_2k, p.kr = F)
sjPlot::sjt.lmer(mod_2i,mod_2j,mod_2k, p.kr = F,show.aic = TRUE, separate.ci.col = FALSE)
# try an interaction plot
interact_plot(mod_2k, pred = gndctr_ndvi_cbg_adj, modx = clust4, modxvals = c("1", "2", "3", "4"))

# look at violent and property too ----------------------------------------

# null model
mod_3a <- lme4::lmer(CRMCYPERC ~ 1 + (1|city_st), data = d)
mod_3b <- lme4::lmer(CRMCYPERC ~ 1 + (1|city_st), data = d_urb)


# just add the IV of interest
mod_3c <- lme4::lmer(CRMCYPERC ~ gndctr_ndvi_cbg_adj + (1|city_st), data = d)
mod_3d <- lme4::lmer(CRMCYPERC ~ gndctr_ndvi_cbg_adj + (1|city_st), data = d_urb)

sjPlot::sjt.lmer(mod_3c, mod_2c, p.kr = F,show.aic = TRUE, separate.ci.col = FALSE)
stargazer(mod_3d, type = "text")
