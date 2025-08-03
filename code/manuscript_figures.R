##########################
### Manuscript figures ###
##########################


### Figures in the main text ###

# Loading packages

library(ggeffects)
library(gridExtra)
library(ggplot2)
library(dplyr)

# Loading data sets

blutidf <- read.csv("data/blutidf_2025.csv")
blutidf_3yo <- read.csv("data/blutidf_3yo_2025.csv")
fed_df <- read.csv("data/fed_df_2025.csv")
cs_df <- read.csv("data/cs_df_2025.csv")
suc_df <- read.csv("data/suc_df_2025.csv")
habitat_foliage_scores <- read.csv("data/Habitat_SiteII.csv")
environment <- read.csv("data/environment_II.csv")