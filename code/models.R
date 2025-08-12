
##############
### Models ###
##############

library(glmmTMB)
library(ggplot2)
library(DHARMa)
library(dplyr)

fed_df <- read.csv("data/fed_df_2025.csv")
cs_df <- read.csv("data/cs_df_2025.csv")
suc_df <- read.csv("data/suc_df_2025.csv")
habitat_foliage_scores <- read.csv("data/Habitat_SiteII.csv")

####################
### Final Models ###
####################

## First egg lay date ##

fed_df$total_FS <- habitat_foliage_scores[match(fed_df$site, habitat_foliage_scores$Site),"Total"]  # assigning each observation in the data set to its corresponding foliage score value according to site

fed_df$site_year <- as.factor(paste(fed_df$site, fed_df$year, sep="_"))  # combining site and year into one variable

fed_model <- glmmTMB(fed ~ scale(yo)*scale(total_FS) + scale(w)*scale(total_FS) +  # Fixed effects and their interactions
                         (1|ring) + (1|site_year),  # random effects
                       data = fed_df,
                       REML = T)  # dataset

summary(fed_model)


## Clutch size ##

cs_df$total_FS <- habitat_foliage_scores[match(cs_df$site, habitat_foliage_scores$Site),"Total"]  # assigning each observation in the data set to its corresponding foliage score value according to site

cs_df$site_year <- as.factor(paste(cs_df$site, cs_df$year, sep="_"))  # combining site and year into one variable

cs_model <- glmmTMB(cs ~ scale(yo)*scale(total_FS) + scale(w)*scale(total_FS) +  # Fixed effects and their interactions
                         (1|ring) + (1|site_year),  # random effects
                       data = cs_df,
                      REML = T)  # dataset

summary(cs_model)


## Number of fledgelings ##

suc_df$total_FS <- habitat_foliage_scores[match(suc_df$site, habitat_foliage_scores$Site),"Total"]  # assigning each observation in the data set to its corresponding foliage score value according to site

suc_df$site_year <- as.factor(paste(suc_df$site, suc_df$year, sep="_"))  # combining site and year into one variable

suc_model_1 <- glmmTMB(suc ~ scale(yo)*scale(total_FS) + scale(w)*scale(total_FS) +  # Fixed effects and their interactions
                        (1|ring) + (1|site_year),  # random effects
                      data = suc_df,  # dataset
                      family = poisson())  # conditional sampling distribution of the response

summary(suc_model_1)  # AIC = 7154

suc_model_2 <- glmmTMB(suc ~ scale(yo)*scale(total_FS) + scale(w)*scale(total_FS) +  # Fixed effects and their interactions
                         (1|ring) + (1|site_year),  # random effects
                       data = suc_df,  # dataset
                       family = nbinom2())  # conditional sampling distribution of the response

summary(suc_model_2)  # AIC = 7156

suc_model_3 <- glmmTMB(suc ~ scale(yo)*scale(total_FS) + scale(w)*scale(total_FS) +  # Fixed effects and their interactions
                         (1|ring) + (1|site_year),  # random effects
                       ziformula = ~1,  # specifying a zero-inflated model where we don't expect 0s to arise under a specific predictor a priori
                       data = suc_df,  # dataset
                       family = poisson())  # conditional sampling distribution of the response

summary(suc_model_3)  # AIC = 6797.8

suc_model_4 <- glmmTMB(suc ~ scale(yo)*scale(total_FS) + scale(w)*scale(total_FS) +  # Fixed effects and their interactions
                         (1|ring) + (1|site_year),  # random effects
                       ziformula = ~1,  # specifying a zero-inflated model where we don't expect 0s to arise under a specific predictor a priori
                       data = suc_df,  # dataset
                       family = nbinom2())  # conditional sampling distribution of the response

summary(suc_model_4)  # AIC = 6799.8

# Number of fledgelings model with the lowest AIC is ZIP

# Final model for number of fledgelings:

suc_model <- glmmTMB(suc ~ scale(yo)*scale(total_FS) + scale(w)*scale(total_FS) +  # Fixed effects and their interactions
                         (1|ring) + (1|site_year),  # random effects
                       ziformula = ~1,  # specifying a zero-inflated model where we don't expect 0s to arise under a specific predictor a priori
                       data = suc_df,  # dataset
                       family = nbinom2(), # conditional sampling distribution of the response
                       REML = T)  

summary(suc_model)


#########################
### Model diagnostics ###
#########################


## First egg lay date ##

fed_simulationOutput <- simulateResiduals(fittedModel = fed_model, plot = F)  # this DHARMa function calculates randomized quantile residuals

fed_res <- as.data.frame(residuals(fed_simulationOutput)) %>% rename(res = "residuals(fed_simulationOutput)")  # we can then extract the residuals using this function...

hist(fed_res$res, main = "Distribution of calculated residuals", xlab = "Calculated (scaled) residuals", col = "#248fc9")  #...and plot them to see their distribution

plot(fed_simulationOutput)  # this function creates two plots:

# plotQQunif (left panel) creates a qq-plot to detect overall deviations from the expected distribution, by default with added tests for correct distribution (KS test), dispersion and outliers.
# plotResiduals (right panel) produces a plot of the residuals against the predicted value (or alternatively, other variable). Simulation outliers (data points that are outside the range of simulated values) are highlighted as red stars.
# By default, plotResiduals plots against predicted values. However, we can also use it to plot residuals against a specific other predictors (highly recommend):

plotResiduals(fed_simulationOutput, form = fed_df$yo)  # because the x-axis has few values (3 age groups), plotResiduals() has plotted a box plot with additional tests instead of a scatter plot.

plotResiduals(fed_simulationOutput, form = fed_df$w)

plotResiduals(fed_simulationOutput, form = fed_df$total_FS)

# Using supporting goodness-of-fit tests

testUniformity(fed_simulationOutput, alternative = "two.sided", plot = T)

testQuantiles(fed_simulationOutput)

testDispersion(fed_simulationOutput)

# Although DHARMa flags violation of Normality and heterogeneity, these deviations are within the expectation given the type of the data.



## Clutch size ##

cs_simulationOutput <- simulateResiduals(fittedModel = cs_model, plot = F)  # randomized quantile residuals

cs_res <- as.data.frame(residuals(cs_simulationOutput)) %>% rename(res = "residuals(cs_simulationOutput)")  # extracting residuals

hist(cs_res$res, main = "Distribution of calculated residuals", xlab = "Calculated (scaled) residuals", col = "#248fc9")  # plotting residuals

plot(cs_simulationOutput)  

plotResiduals(cs_simulationOutput, form = cs_df$yo)  

plotResiduals(cs_simulationOutput, form = cs_df$w)

plotResiduals(cs_simulationOutput, form = cs_df$total_FS)

# Using supporting goodness-of-fit tests

testUniformity(cs_simulationOutput, alternative = "two.sided", plot = T)

testQuantiles(cs_simulationOutput)

testDispersion(cs_simulationOutput)

testOutliers(cs_simulationOutput)

# DHARMa flags some outliers, but deviations seem to be within expectations


## Number of fledgelings ##

suc_simulationOutput <- simulateResiduals(fittedModel = suc_model, plot = F)  # randomized quantile residuals

suc_res <- as.data.frame(residuals(suc_simulationOutput)) %>% rename(res = "residuals(suc_simulationOutput)")  # extracting residuals

hist(suc_res$res, main = "Distribution of calculated residuals", xlab = "Calculated (scaled) residuals", col = "#248fc9")  # plotting residuals

plot(suc_simulationOutput)  

plotResiduals(suc_simulationOutput, form = suc_df$yo)  

plotResiduals(suc_simulationOutput, form = suc_df$w)

plotResiduals(suc_simulationOutput, form = suc_df$total_FS)

# Using supporting goodness-of-fit tests

testUniformity(suc_simulationOutput, alternative = "two.sided", plot = T)

testQuantiles(suc_simulationOutput)

testDispersion(suc_simulationOutput)  # underdispersion

testOutliers(suc_simulationOutput)

# There's underdispersion, but when there's no model overfitting, consensus is that it's okay to not correct for it.
