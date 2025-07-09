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



##################################################
### Analysis including environmental predictor ### 
##################################################


# 1. Elevation

glmm4 <- lmer(suc ~ yo + w + elevation + yo*elevation + w*elevation + (1|year) + (1|site) + (1|ring), data = blutidf_3yo)
summary(glmm4)
2 * (1 - pnorm(abs(coef(summary(glmm4))["yo", "t value"])))  #  p-val = 0.612898
2 * (1 - pnorm(abs(coef(summary(glmm4))["w", "t value"])))  # p-val = 0.3295119
2 * (1 - pnorm(abs(coef(summary(glmm4))["elevation", "t value"])))  # p-val = 0.7924903
2 * (1 - pnorm(abs(coef(summary(glmm4))["yo:elevation", "t value"])))  # p-val = 0.8139274
2 * (1 - pnorm(abs(coef(summary(glmm4))["w:elevation", "t value"])))  # p-val = 0.5318948 

glmm5 <- lmer(cs ~ yo + w + elevation + yo*elevation + w*elevation + (1|year) + (1|site) + (1|ring), data = blutidf_3yo)
summary(glmm5)
2 * (1 - pnorm(abs(coef(summary(glmm5))["yo", "t value"])))  #  p-val = 0.612898
2 * (1 - pnorm(abs(coef(summary(glmm5))["w", "t value"])))  # p-val = 0.3295119
2 * (1 - pnorm(abs(coef(summary(glmm5))["elevation", "t value"])))  # p-val = 0.7924903
2 * (1 - pnorm(abs(coef(summary(glmm5))["yo:elevation", "t value"])))  # p-val = 0.8139274
2 * (1 - pnorm(abs(coef(summary(glmm5))["w:elevation", "t value"])))  # p-val = 0.5318948

glmm6 <- lmer(fed ~ yo + w + elevation + yo*elevation + w*elevation + (1|year) + (1|site) + (1|ring), data = blutidf_3yo)
summary(glmm6)
2 * (1 - pnorm(abs(coef(summary(glmm6))["yo", "t value"])))  #  p-val = 0.612898
2 * (1 - pnorm(abs(coef(summary(glmm6))["w", "t value"])))  # p-val = 0.3295119
2 * (1 - pnorm(abs(coef(summary(glmm6))["elevation", "t value"])))  # p-val = 0.7924903
2 * (1 - pnorm(abs(coef(summary(glmm6))["yo:elevation", "t value"])))  # p-val = 0.8139274
2 * (1 - pnorm(abs(coef(summary(glmm6))["w:elevation", "t value"])))  # p-val = 0.5318948



#2. Site occupancy

glmm7 <- lmer(suc ~ yo + w + site_occ + yo*site_occ + w*site_occ + (1|year) + (1|site) + (1|ring), data = blutidf_3yo)
summary(glmm7)
2 * (1 - pnorm(abs(coef(summary(glmm7))["yo", "t value"])))  #  p-val = 0.9611638
2 * (1 - pnorm(abs(coef(summary(glmm7))["w", "t value"])))  # p-val = 0.7645105
2 * (1 - pnorm(abs(coef(summary(glmm7))["site_occ", "t value"])))  # p-val = 0.7364674
2 * (1 - pnorm(abs(coef(summary(glmm7))["yo:site_occ", "t value"])))  # p-val = 0.8120249
2 * (1 - pnorm(abs(coef(summary(glmm7))["w:site_occ", "t value"])))  # p-val = 0.5732922

glmm8 <- lmer(cs ~ yo + w + site_occ + yo*site_occ + w*site_occ + (1|year) + (1|site) + (1|ring), data = blutidf_3yo)
summary(glmm8)
2 * (1 - pnorm(abs(coef(summary(glmm8))["yo", "t value"])))  #  p-val = 0.5465311
2 * (1 - pnorm(abs(coef(summary(glmm8))["w", "t value"])))  # p-val = 0.5043828
2 * (1 - pnorm(abs(coef(summary(glmm8))["site_occ", "t value"])))  # p-val = 0.7034447
2 * (1 - pnorm(abs(coef(summary(glmm8))["yo:site_occ", "t value"])))  # p-val = 0.7114516
2 * (1 - pnorm(abs(coef(summary(glmm8))["w:site_occ", "t value"])))  # p-val = 0.2981551

glmm9 <- lmer(fed ~ yo + w + site_occ + yo*site_occ + w*site_occ + (1|year) + (1|site) + (1|ring), data = blutidf_3yo)
summary(glmm9)
2 * (1 - pnorm(abs(coef(summary(glmm9))["yo", "t value"])))  #  p-val = 0.01976594
2 * (1 - pnorm(abs(coef(summary(glmm9))["w", "t value"])))  # p-val = 0.8157821
2 * (1 - pnorm(abs(coef(summary(glmm9))["site_occ", "t value"])))  # p-val = 5.201948e-08
2 * (1 - pnorm(abs(coef(summary(glmm9))["yo:site_occ", "t value"])))  # p-val = 0.2309721
2 * (1 - pnorm(abs(coef(summary(glmm9))["w:site_occ", "t value"])))  # p-val = 0.7092915


