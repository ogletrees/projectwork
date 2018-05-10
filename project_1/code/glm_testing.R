library(multilevel)
d <- readRDS("../data/full_mlm_datset.rds")
mod1 <- lm(d$CRMCYPERC ~ d$ndvi_mean_cbg_adj)
mod2 <- gls(CRMCYPERC ~ ndvi_mean_cbg_adj, data = d)
mod3 <- glm(CRMCYPERC ~ ndvi_mean_cbg_adj, data = d, family = Gamma(link = "log"))

summary(mod1)
summary(mod2)
summary(mod3)
logLik(mod3)
anova(mod2, mod3)

exp(coef(mod3))

plot(density(resid(mod3)))
plot(density(resid(mod1)))
plot(density(resid(mod2)))

mod4 <- lm(log(CRMCYPERC) ~ ndvi_mean_cbg_adj, data = d)
summary(mod4)     
plot(density(resid(mod4)))

mod5 <- glm(CRMCYPERC ~ ndvi_mean_cbg_adj, data = d)
summary(mod5)
sd(resid(mod5))


plog <- predict(mod3)
pno <- predict(mod1)
ccc <- cbind(plog, pno)
