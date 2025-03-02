
########################
### Data preparation ###
########################

# Loading necessary packages
library(dplyr)  

# Loading original datasets
adults <- read.csv("data/Adults.csv")  # loading adult blue tits data
adultx <- as_tibble(adults)  # converting data into a tibble (easier to manage)
adultx  # visualising first rows of data
birdphen <- read.csv("data/Bird_Phenology.csv")  # loading phenology data
birdphenx <- as_tibble(birdphen)  #converting data into a tibble (easier to manage)
birdphenx  # visualising first rows of data





