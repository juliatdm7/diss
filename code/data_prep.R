
########################
### Data preparation ###
########################

# Loading necessary packages
library(dplyr)  

# Loading original datasets
adults <- read.csv("data/Adults.csv")  # loading adult blue tits data
adults <- as_tibble(adults)  # converting data into a tibble (easier to manage)
adults  # visualising first rows of data
allbirdphen <- read.csv("data/Bird_Phenology.csv")  # loading phenology data
allbirdphen <- as_tibble(allbirdphen)  #converting data into a tibble (easier to manage)
allbirdphen  # visualising first rows of data

# Selecting variables that will be useful for the project from the phenology data
# Since we aim to study breeding success trends in blue tit populations across age groups and across sites, we will extract those variables that may be useful to study breeding success: first egg lay date ("fed"), number of hatched eggs ("number.hatched") and fledling success (suc)
blutiphen <- allbirdphen %>% filter(species == "bluti")  # first, we will select and extract cases that are of blue tits
blutiphen <- blutiphen %>% select(year, site, box, fed, number.hatched, suc)  # we also select columns that will help us collate both databases into one by identifying individuals (i.e.: year, site and box)

# Selecting variables that will be useful for the project from the adult data
adultx <- adults %>% filter(season != "winter", sex == "F")  # we will remove all data coming from adults captured in the winter (as we may not have their corresponding breeding season data), from which we have 410 observations removed, and keep data from females only (removing 1979 cases, if I'm not mistaken).
blutiadults <- adultx %>% select(ring, year, site, box, age)  # finally, we select the columns that we will use

# Rearranging datasets in ascending order of years, site and nestboxes
blutiphen <- blutiphen %>% arrange(year, site, box)  
blutiadults <- blutiadults %>% arrange(year, site, box)  

### Creating a new database by collating  blutiadults_F and blutiphen using columns "year", "site" and "box" ###

# First, let's find out if there are duplicates by "year", "site" and "box" in both datasets:
blutiphen %>%
  count(year, site, box) %>%
  filter(n > 1)  # there seem to be 5 duplicates in bluetiphen
blutiadults %>%
  count(year, site, box) %>%
  filter(n > 1)  # there seem to be 9 duplicates in blutiadults

# Since I don't know yet what they mean or if I want to remove them or not, I will not eliminate them

# Now, I expect that phenological data will not be available for all individuals in blutiadults database, so I want to know how many cases (rows) in blutiphen lack their corresponding case in blutiadults_F:
# I will use the function anti_join() to find it out:
blutiadults %>%
  anti_join(blutiphen, by = c("year", "site", "box"))  # there are 5 rows present in blutiadults for which we do not have the data in blutiphen

blutiphen %>%
  anti_join(blutiadults, by = c("year", "site", "box"))  # there are 395 rows in  blutiphen for which we do not have data in blutiadults

# I want to keep all the rows in blutiphen even if there is no corresponding data for it in blutiadults, but I don't really care about data in bluetiadults for which I have no data in blutiphen, as I won't be able to use it in the models.
# Therefore, I'll filter blutiadults according to columns "year", "site" and "box" in blutiphen to keep those with their corresponding matching and I will collate that selection with blutiphen subdatabase:
filt_blutiadults <- blutiadults %>% 
  semi_join(blutiphen, by = c("year", "site", "box")) # 1689 cases for which there is a corresponding case in blutiphen
bluti1 <- filt_blutiadults %>%
  right_join(blutiphen, by = c("year", "site", "box")) # 2089 cases for which there is either info on both blutiadults database and blutiphen database or only on blutiphen database

bluti1 %>% arrange(year, site, box)  # Let's rearrange the dataset again for better clarity when looking at it
