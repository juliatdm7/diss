
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

fed_df$total_FS <- habitat_foliage_scores[match(fed_df$site, habitat_foliage_scores$Site),"Total"]

fed_df$site_year <- as.factor(paste(fed_df$site, fed_df$year, sep="_"))

fed_model <- glmmTMB(fed ~ scale(yo)*scale(total_FS) + scale(w)*scale(total_FS) +  # Fixed effects and their interactions
                         (1|ring) + (1|site_year),  # random effects
                       data = fed_df,
                       REML = T)  # dataset

summary(fed_model)


## Clutch size ##

cs_df$total_FS <- habitat_foliage_scores[match(cs_df$site, habitat_foliage_scores$Site),"Total"]

cs_df$site_year <- as.factor(paste(cs_df$site, cs_df$year, sep="_"))

cs_model <- glmmTMB(cs ~ scale(yo)*scale(total_FS) + scale(w)*scale(total_FS) +  # Fixed effects and their interactions
                         (1|ring) + (1|site_year),  # random effects
                       data = cs_df,
                      REML = T)  # dataset

summary(cs_model)


## Number of fledgelings ##

suc_df$total_FS <- habitat_foliage_scores[match(suc_df$site, habitat_foliage_scores$Site),"Total"]

suc_df$site_year <- as.factor(paste(suc_df$site, suc_df$year, sep="_"))

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


#########################
### Model diagnostics ###
#########################


# First egg lay date
