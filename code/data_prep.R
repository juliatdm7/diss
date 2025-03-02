
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

# Selecting variables that will be useful for the project from the phenology data
blutiphen <- birdphenx %>% filter(species == "bluti")  # first, we will select and extract cases that are of blue tits
# Since we aim to study breeding success trends in blue tit populations across age groups and across sites, we will extract those variables that may be useful to study breeding success: first egg lay date ("fed"), number of hatched eggs ("number.hatched") and fledling success (suc)
blutiphen <- blutiphen %>% select(year, site, box, fed, number.hatched, suc)  # we also select columns that will help us collide both databases into one by identifying individuals (i.e.: year, site and box)

# Selecting variables that will be useful for the project from the adult data
blutiadults <- adultx %>% select(ring, year, site, box, age, sex)

