#####################
### Data analysis ###
#####################

library(openxlsx)
library(dplyr)  
library(tidyverse)
library(lme4)
library(ggplot2)

blutidf_3yo <- read.csv("data/blutidf_3yo.csv")

lm1 <- lm(cs ~ yo, data = blutidf_3yo)
summary(lm1)
anova(lm1)

lm2 <- lm(suc ~ yo, data = blutidf_3yo)
summary(lm2)
anova(lm2)

lm3 <- lm(fed ~ yo, data = blutidf_3yo)
summary(lm3)
anova(lm3)

pt(2.488, df=)

glmm1 <- lmer(cs ~ yo + w + (1|year) + (1|site) + (1|ring), data = blutidf_3yo)
summary(glmm1)
2 * (1 - pnorm(abs(coef(summary(glmm1))["yo", "t value"])))  # p-val = 0.005143159 assuming a N distribution. Does this mean that clutch size increases by ~0.18 on average per year that the birds grow?
2 * (1 - pnorm(abs(coef(summary(glmm1))["w", "t value"])))  # p-val = 0.3447631 assuming a N distribution. Does this mean, then, that selective disappearance doesn't have a significant effect on clutch size differences among birds of different age?
# According to these, could we (preliminary) conclude that within-individual processes significantly explain the increase of clutch size as age increases?


glmm2 <- lmer(suc ~ yo + w + (1|year) + (1|site) + (1|ring), data = blutidf_3yo)
summary(glmm2)
2 * (1 - pnorm(abs(coef(summary(glmm2))["yo", "t value"]))) # p-val = 0.6007668 assuming a N distribution. 
2 * (1 - pnorm(abs(coef(summary(glmm2))["w", "t value"]))) # p-val = 0.01283942 assuming a N distribution.
# However, it seems that it's the other way around for number of chicks successfully fledged? It looks like between-individuals processes have a significant effect. 


glmm3 <- lmer(fed ~ yo + w + (1|year) + (1|site) + (1|ring), data = blutidf_3yo)
summary(glmm3)
2 * (1 - pnorm(abs(coef(summary(glmm3))["yo", "t value"])))  # p-val = 0.000296272 assuming a N distribution. 
2 * (1 - pnorm(abs(coef(summary(glmm3))["w", "t value"])))  # p-val = 0.7022408 assuming a N distribution. 

plot(blutidf_3yo$cs ~ blutidf_3yo$yo)
abline(lm1)
          