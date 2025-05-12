#####################
### Data analysis ###
#####################

library(openxlsx)
library(dplyr)  
library(tidyverse)
library(ggplot2)
library(lme4)

blutidf <- read.xlsx("data/blutidf.xlsx")

lm1 <- lm(cs ~ yo, data = blutidf)
summary(lm1)
anova(lm1)
          