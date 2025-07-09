#####################
### Data analysis ###
#####################

library(openxlsx)
library(dplyr)  
library(tidyverse)
library(glmmTMB)
library(ggplot2)

blutidf_3yo <- read.csv("data/blutidf_3yo_II.csv")
fed_df <- read.csv("data/fed_df_II.csv")
cs_df <- read.csv("data/cs_df_II.csv")
suc_df <- read.csv("data/suc_df.csv")

# First egg lay date:

fed_df$site_year <- as.factor(paste(fed_df$site, fed_df$year, sep="_"))  # creating a new column that joins site and year to take into account that there might be variation not only between sites and between years but also between sites at different years

fed_model_I <- glmmTMB(fed ~ yo + w + total_FS +  # predictors (fixed effects)
                         (1|ring) + (1|site_year),  # random effects
                       data = fed_df)  # dataset

summary(fed_model_I)
