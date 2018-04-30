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

csamp <- clist %>% sample_n(10)

df_sub <- df %>% filter(city_st %in% csamp$city_st)

#---------------------------------------------------

df_sub %>% ggplot(aes(ndvi_mean_cbg_adj,CRMCYPERC, color = city_st)) + geom_point() + scale_color_brewer(palette="Paired")

df_sub %>% ggplot(aes(ndvi_mean_cbg_adj,CRMCYPERC, group=city_st)) + geom_point(alpha=.1) + geom_smooth(method = "lm", se = F)

df_sub %>% ggplot(aes(ndvi_mean_cbg_adj,CRMCYPERC, group=city_st)) + geom_point(alpha=.1) + facet_wrap(~city_st) + stat_smooth(method="lm",fullrange = TRUE)

#--------------------------------------------------
dnest <- df_sub %>% group_by(city_st) %>% tidyr::nest()


# a model to apply - for violent crime index
city_mod <- function(df){ lm(CRMCYPERC ~  ndvi_mean_cbg_adj + disad + divindex + log_popden_cbg + MedHHinc_000, data = df)}
# apply the model 
mods <- dnest %>% mutate(model = map(data, city_mod))
# pull out a glance of the models- gets model diagnostics like r.square, and drop the data
by_city <- mods %>% mutate(glance = map(model, glance)) %>% unnest(glance, .drop = TRUE)
head(by_city)
by_city %>% ggplot(aes(reorder(city_st, r.squared), r.squared)) + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))


# pull out the estimates
by_city_est <- mods %>% mutate(tidyframe = map(model, tidy)) %>% unnest(tidyframe, .drop = TRUE)

by_city_est %>% filter(term == "ndvi_mean_cbg_adj" & p.value < .05) %>% ggplot(aes(reorder(city_st, estimate), estimate)) + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))



# how many are statistically significant
by_city_est %>% filter(term == "ndvi_mean_cbg_adj" & p.value < .05) %>% count()

# plot marginal effects, as facet grid
fit <- df_sub %>% filter(city_st == "Spokane_WA") %>% city_mod()
sjp.lm(fit, type = "eff", title = "Spokane, WA")
sjp.lm(fit, title = "Spokane_WA, MI - Violent Crime Index")


# plot of variable mean in cities and one standard deviation
df_sub %>% group_by(city_st) %>% summarise(vmean = mean(CRMCYPERC), vsd = sd(CRMCYPERC)) %>% ggplot(aes(reorder(city_st, vmean), vmean)) + geom_point() + geom_errorbar(aes(ymin=vmean-vsd, ymax=vmean+vsd), width=.1) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0, size = 7))

df_sub %>% group_by(city_st) %>% summarise(gmean = mean(ndvi_mean_cbg_adj), gsd = sd(ndvi_mean_cbg_adj)) %>% ggplot(aes(reorder(city_st, gmean), gmean)) + geom_point() + geom_errorbar(aes(ymin=gmean-gsd, ymax=gmean+gsd), width=.1) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0))

#-----------------------------------------------------------------------------

mods %>% mutate(glance = map(model, augment), res=glance %>% map(".resid"), mm = res %>% map(mean)) %>% tidyr::unnest(mm)

                