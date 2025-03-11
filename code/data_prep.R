
########################
### Data preparation ###
########################

# Loading necessary packages
library(dplyr)  
library(tidyverse)

# Loading original datasets
alladults <- as_tibble(read.csv("data/Adults.csv"))  # loading adult blue tits data as a tibble
alladults  # visualising first rows of data
allbirdphen <- as_tibble(read.csv("data/Bird_Phenology.csv"))  # loading phenology data as a tibble (easier to manage)
allbirdphen  # visualising first rows of data

# Selecting variables that will be useful for the project from the phenology data
# Since we aim to study breeding success trends in blue tit populations across age groups and across sites, we will extract those variables that may be useful to study breeding success: first egg lay date ("fed"), number of hatched eggs ("number.hatched") and fledling success (suc)
blutiphen <- allbirdphen %>% filter(species == "bluti")  # first, we will select and extract cases that are of blue tits
blutiphen <- blutiphen %>% select(year, site, box, fed, number.hatched, suc)  # we also select columns that will help us collate both databases into one by identifying individuals (i.e.: year, site and box)

# Selecting variables that will be useful for the project from the adult data
adults <- alladults %>% filter(season != "winter", sex == "F")  # we will remove all data coming from adults captured in the winter (as we may not have their corresponding breeding season data), from which we have 410 observations removed, and keep data from females only.

# After this filtering, we remove 1979 observations (of male birds and of winter recordings), and we have a total of 1694 female adults ringed and identified of which we should have breeding success data in blutiphen (or, at least, of most of them)
blutiadults <- adults %>% select(ring, year, site, box, age)  # finally, we select the columns that we will use

# Rearranging datasets in ascending order of years, site and nestboxes
blutiphen <- blutiphen %>% arrange(year, site, box)  
blutiadults <- blutiadults %>% arrange(year, site, box)  



### Creating a new database by collating  blutiadults and blutiphen using columns "year", "site" and "box" ###

# First, let's find out if there are duplicates by "year", "site" and "box" in both datasets:
blutiphen %>%
  count(year, site, box) %>%
  filter(n > 1)  # there seem to be 5 duplicates in bluetiphen
# These duplicates could be second broods (within the same breeding season and from the same female) or could be relays (if the first brood did not success):
  # 2019, AVN site, box 2 seems to be a relay as the first entry has no success input
  # 2019, EDI site, box 1 is also this case (except for the fact that in the first entry there is a 0 instead of a NA)
  # 2019, MUN site, box 1 also probably follows the re-lay case
  # as also probably does 2021 site DOW, nestbox 4
blutiadults %>%
  count(year, site, box) %>%
  filter(n > 1)  # there seem to be 9 duplicates in blutiadults
# These duplicates are re-measurements of the same females' tarsus length and body mass but later on (1-5 days later).

# Since we haven't decided yet if we want (or have to) remove them or not, I will not eliminate them

# Now, I expect that phenological data will not be available for all individuals in blutiadults database, so I want to know how many cases (rows) in blutiphen lack their corresponding case in blutiadults:
# I will use the function anti_join() to find it out:
no_bs_data <- blutiadults %>%
  anti_join(blutiphen, by = c("year", "site", "box"))  # there are 5 rows present in blutiadults for which we do not have the data in blutiphen

no_adult_data <- blutiphen %>%
  anti_join(blutiadults, by = c("year", "site", "box"))  # there are 395 rows in  blutiphen for which we do not have data in blutiadults
# We store these problematic cases in their separate rows in case we want to reach for them in the future


# I want to keep all the rows in blutiphen even if there is no corresponding data for it in blutiadults, but I don't really care about data in bluetiadults for which I have no data in blutiphen, as I won't be able to use it in the models.
# Therefore, I'll filter blutiadults according to columns "year", "site" and "box" in blutiphen to keep those with their corresponding matching and I will collate that selection with blutiphen subdatabase:
filt_blutiadults <- blutiadults %>% 
  semi_join(blutiphen, by = c("year", "site", "box")) # 1689 cases for which there is a corresponding case in blutiphen
bluti1 <- filt_blutiadults %>%
  right_join(blutiphen, by = c("year", "site", "box")) # 2089 cases for which there is either info on both blutiadults database and blutiphen database or only on blutiphen database

# There's a problem 

##bluti2 <- blutiadults %>%
##  full_join(blutiphen, by = c("year", "site", "box"))
##bluti3 <- blutiphen %>%
##  full_join(blutiadults, by = c("year", "site", "box"))
##bluti4 <- blutiphen %>%
##  right_join(blutiadults, by = c("year", "site", "box"))
##bluti5 <- blutiphen %>%
## left_join(blutiadults, by = c("year", "site", "box"))
# Above there are some other options that I have rejected either because they don't keep the information that I'm interested in.

bluti1 <- bluti1 %>% arrange(year, site, box)   # Let's rearrange the dataset again for better clarity when reading it
head(bluti1)

bluti4 <- blutiphen %>%
  inner_join(blutiadults, by =c("year", "site", "box"))

### Exploring the dataset ###

unique(bluti1$year)  # Data from years 2014-2024

length(unique(bluti1$ring))

nr_birds <- bluti1 %>%
  count(ring) # I think this would be useful to identify how many birds there actually are and how many times they are repeated in the dataset (how many years their breeding attempts have been recorded)
# According to that bit of code, we have 1151 female blue tits for which we have recorded at least 1 breeding season...
nr_birds %>% filter(is.na(ring))# ...and we have 395 breeding attempts recording for which we don't have the information of the parents.

mean(nr_birds$n)  # on average, each female breeds for 2 years approx. (I think?), assuming that we have recorded all lifetime breeding attempts

max(nr_birds[1:1150, 2])  # the maximum number of breeding attempts recorded for the same female is 8...
nr_birds[which(nr_birds$n == max(nr_birds[1:1150, 2])),"ring"]  # ...and it's of female S921907

str(bluti1)

#bluti1$YEAR <- as.factor(bluti1$year)
#bluti1$site <- as.factor(bluti1$site)
#bluti1$suc <- as.numeric(bluti1$suc)

bluti2 <- bluti1[-which(bluti1$suc < 0),]
bluti2$suc <- as.numeric(bluti2$suc)
hist(bluti2$suc, xlab = "Number of chicks successfully fledged")

### Creating a new column that specifies age (in years old) of the bird at the recorded breeding attempt ###


 
#years <- matrix(0, nrow=1, ncol=nrow(bluti3))  # here I create a matrix that has a single row and as many columns as rows has bluti3 (as cases are, after filtering NAs)
#y <- 0  # here I create a variable that I will use within the loop 
#for (i in 1:nrow(bluti3)) {
#  y <- 0  # reset the variable to 0 after every iteration
#  for (a in 1:i) {
#    if (bluti3[a, "ring"] == bluti3[i, "ring"]) {
#      y <- y + 1  # if the ring number from the iteration has been before, add the amount of times it has appeared so far
#    }
#  }
#  years[1,i] <- y  # store for each case, how many years we've recorded that bird up until the one in which that recording was made
#}
#
## Now, we can combine this with the informaton we have in the "age" column in bluti3 subdataset to estimate how old was each bird in each case:
#
#bluti3$y_old <- 0
#for (i in 1:nrow(bluti3)) {
#  if (bluti3[i,"age"] == 5) {
#    bluti3[i,"y_old"] <- 1
#  } else if (bluti3[i,"age"] == 6) {
#    if (years[1,i] == 1) {
#      bluti3[i, "y_old"] <- 2  # WARNING! Here we're assuming that all female birds that we catch for the first time and classify as "age = 6" are 2 years old, BUT THEY COULD BE OLDER SINCE WE CANNOT DETERMINE THEIR EXACT AGE PAST 2 YEARS THROUGH THEIR PLUMMAGE
#    } else if (years[1,i] > 1) {
#      bluti3[i, "y_old"] <- years[1,i]
#    } else {
#      bluti3[i, "y_old"] <- NA
#    }
#  }
#}

# Creating a new data frame (tibble) that summarises breeding attempts per identified individual per year

bluti3 <- bluti2 %>%
  filter(!is.na(ring), !is.na(age))  # first, I need to remove all rows containing NAs in "ring" or "age" columns because otherwise the loop will not work
br_attempts <- bluti3 %>% 
  group_by(year) %>%
  count(ring)

# Converting br_attempts into a individual-by-year matrix
ID_by_year <- br_attempts %>% 
  pivot_wider(names_from=year,values_from=c(n))

x <- bluti3 %>% select(ring, year, age) %>% mutate(y_old = 0)
case <- as.data.frame(matrix(0, nrow=1, ncol=4))
colnames(case) <- c("ring", "year", "age", "y_old")
case[1,"ring"] <- x[1,"ring"]
years <- matrix(0, nrow=1, ncol=nrow(bluti3))  # here I create a matrix that has a single row and as many columns as rows has bluti3 (as cases are, after filtering NAs)
y <- 0  # here I create a variable that I will use within the loop
for (i in 1:nrow(x)) {
  case[,1:4] <- 0  # reset the variable to 0 after every iteration
  case[1,] <- x[i,]
  for (a in 1:i) {
    case[,1:4] <- 0  # reset the variable to 0 after every iteration
    case[2,] <- x[a,]
    if (x[a, "ring"] == x[i, "ring"] &
        !x[a,"year"] == case[1,"year"]) {
      y <- y + 1  # if the ring number from the iteration has been before, add the amount of times it has appeared so far
    }
    case[1,] <- x[a,]
  }
  years[1,i] <- y  # store for each case, how many years we've recorded that bird up until the one in which that recording was made
}
