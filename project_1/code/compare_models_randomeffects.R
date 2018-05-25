# Date: 2018-05-25
# S Ogletree
# Description: Some model exploration

library(dplyr)
library(ggplot2)
library(lme4)
library(multilevel)
library(sjPlot)
library(sjmisc)
library(broom)
library(merTools)
library(stargazer)
library(texreg)
library(purrr)
library(tidyr)

city <- readRDS("../data/city_all.rds")
df <- readRDS("../data/full_mlm_datset.rds")
newclust <- city[, c(1, 69)]
df <- df %>% left_join(newclust)
df_na <- df %>% filter(!is.na(gndctr_MedHHinc000)& !is.na(gndctr_disad))
df_urban <- df %>% filter(Urban == 1)
df_urban_nona <- df_na %>% filter(Urban == 1)


# OLS ---------------------------------------------------------------------

mod_ols <- lm(CRMCYPERC ~ ndvi_mean_cbg_adj + MedHHinc_000 + divindex + disad + log_popden_cbg, data = df)
mod_lmm <- lmer(CRMCYPERC ~ ndvi_mean_cbg_adj + MedHHinc_000 + divindex + disad + log_popden_cbg + (1|city_st), data = df)

summary(mod_ols)
summary(mod_lmm)
plot(mod_ols)

# Compare separate models with LMM ----------------------------------------

# nest by city
nest_df <- df %>% group_by(city_st) %>% nest()
citymods <- function(df) {lm(CRMCYPERC ~ ndvi_mean_cbg_adj, data = df)}
nest_df <- nest_df %>% mutate(model = map(data, citymods))
df_unnest <- nest_df %>% mutate(glance = map(model, tidy)) %>% unnest(glance, .drop = TRUE)

mod_lmm2 <- lmer(CRMCYPERC ~ ndvi_mean_cbg_adj + (1|city_st), data = df)
coef_2 <- coef(mod_lmm2)$city_st %>% rename(IntLMM = `(Intercept)`) %>% mutate(city_st = row.names(.))
sep_int <- df_unnest %>% filter(term == '(Intercept)') %>% left_join(coef_2)
sep_int %>% slice(1:20) %>% ggplot(aes(reorder(city_st, estimate), estimate)) + geom_point()+geom_point(aes(y=IntLMM, color="red")) + geom_hline(yintercept = mean(df$CRMCYPERC)) +  scale_color_manual(labels = "LMM", values = "red") + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = .5))
