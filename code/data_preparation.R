
########################
### Data preparation ###
########################

# Loading necessary packages
library(dplyr)  
library(tidyverse)
library(openxlsx)

# Loading original datasets
alladults <- as_tibble(read.csv("data/AdultsII.csv"))  # loading adult blue tits data as a tibble
alladults  # visualising first rows of data
allbirdphen <- as_tibble(read.csv("data/Bird_PhenologyII.csv"))  # loading phenology data as a tibble (easier to manage)
allbirdphen  # visualising first rows of data

# Selecting variables that will be useful for the project from the phenology data
# Since we aim to study breeding success trends in blue tit populations across age groups and across sites, we will extract those variables that may be useful to study breeding success: first egg lay date ("fed"), number of hatched eggs ("number.hatched") and fledling success (suc)
blutiphen <- allbirdphen %>% filter(species == "bluti")  # first, we will select and extract cases that are of blue tits
blutiphen <- blutiphen %>% select(year, site, box, fed, cs, suc)  # we also select columns that will help us collate both databases into one by identifying individuals (i.e.: year, site and box)

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
  filter(n > 1)  # there seem to be 5 duplicates in blutiphen
# These duplicates could be second broods (within the same breeding season and from the same female) or could be relays (if the first brood did not success):
  # 2019, AVN site, box 2 seems to be a relay as the first entry has no success input
  # 2019, EDI site, box 1 is also this case (except for the fact that in the first entry there is a 0 instead of a NA)
  # 2019, MUN site, box 1 also probably follows the re-lay case
  # as also probably does 2021 site DOW, nestbox 4
blutiadults %>%
  count(year, site, box) %>%
  filter(n > 1)  # there seem to be 9 duplicates in blutiadults
# These duplicates are re-measurements of the same females' tarsus length and body mass but later on (1-5 days later).

blutiadults %>%
  count(ring, year, site, box) %>%
  filter(n > 1)  # In nestbox 7 of site BLG in year 2020 two females were catched for the same nest. What should we do about it?

# I'm particularly interested in removing duplicates so that they don't complicate calculating the age of each individual in each event we've recorded of them
# I'm not particularly worried about each case is removed for blutiadults as both will store the same information
# However, for blutiphen I am interested in storing the re-lays rather than the failed breeding attempts (ask Hannah confirmation about this).
# I'm gonna try to rearrange my dataset so that relays appear before failed attempts as the unique() function in dplyr will keep the first row it encounters.

blutiphen <- blutiphen %>% arrange(year, site, box, desc(suc))  # now, the first row in the duplicates should be the failed attempt
uqblutiphen <- distinct(blutiphen, year, site, box, .keep_all = T) 

uqblutiadults <- distinct(blutiadults, ring, year, site, box, .keep_all = T)

# Now, I expect that phenological data will not be available for all individuals in blutiadults database, so I want to know how many cases (rows) in blutiphen lack their corresponding case in blutiadults:
# I will use the function anti_join() to find it out:
no_bs_data <- uqblutiadults %>%
  anti_join(uqblutiphen, by = c("year", "site", "box"))  # there are 5 rows present in blutiadults for which we do not have the data in blutiphen

no_adult_data <- uqblutiphen %>%
  anti_join(uqblutiadults, by = c("year", "site", "box"))  # there are 395 rows in  blutiphen for which we do not have data in blutiadults
# We store these problematic cases in their separate rows in case we want to reach for them in the future


# Ignore what's in between rows of ###:

################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################
## I want to keep all the rows in blutiphen even if there is no corresponding data for it in blutiadults, but I don't really care about data in bluetiadults for which I have no data in blutiphen, as I won't be able to use it in the models.
## Therefore, I'll filter blutiadults according to columns "year", "site" and "box" in blutiphen to keep those with their corresponding matching and I will collate that selection with blutiphen subdatabase:
##filt_blutiadults <- blutiadults %>% 
##  semi_join(blutiphen, by = c("year", "site", "box")) # 1689 cases for which there is a corresponding case in blutiphen
##bluti1 <- filt_blutiadults %>%
##  right_join(blutiphen, by = c("year", "site", "box")) # 2089 cases for which there is either info on both blutiadults database and blutiphen database or only on blutiphen database

##bluti2 <- blutiadults %>%
##  full_join(blutiphen, by = c("year", "site", "box"))
##bluti3 <- blutiphen %>%
##  full_join(blutiadults, by = c("year", "site", "box"))
##bluti4 <- blutiphen %>%
##  right_join(blutiadults, by = c("year", "site", "box"))
##bluti5 <- blutiphen %>%
## left_join(blutiadults, by = c("year", "site", "box"))
# Above there are some other options that I have rejected either because they don't keep the information that I'm interested in.

##bluti1 <- bluti1 %>% arrange(year, site, box)   # Let's rearrange the dataset again for better clarity when reading it
##head(bluti1)
################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################


# For now, I will only include in the database the shared cases between blutiphen and blutiadults because lack of data of either of them is problematic
bluti1 <- uqblutiphen %>%
  inner_join(uqblutiadults, by =c("year", "site", "box"))  # warning: many-to-many relationships due to duplicates in both blutiphen and blutiadults databases. For now, we will just keep duplicate rows, but we might have to delete them (especially the duplicates in blutiadults)
# once we remove the duplicates, the warning of many-to-many is gone
# There are some cases for which breeding success is -999. I don't exactly know what that means, but I will create a new bluti2 variable in which I don't include those cases
bluti2 <- bluti1[-which(bluti1$suc < 0),]
bluti2 <- bluti2 %>% relocate(ring, year, site, box, age, fed, cs, suc)
bluti2 <- bluti2 %>% arrange(year, site, box)

### Clutch swap treatment ###

# Between 2017 and 2019, a clutch-swap experiment took place. The results of these experiments may have an influence on suc, and we should account for this.
# First, we need to find how many and which cases in the bluti4 database were part of this experiment:
swaps <- as_tibble(read.csv("data/clutchswaps.csv"))
swaps
# I will have to change the name of either "origin.nest" or "destination.nest" to "box" so that I can compare
# Initially, which one I choose to change shouldn't matter as the experiment was an exchange.
swaps <- swaps %>% rename(box = origin.nest)
clutchswaps1 <- bluti1 %>% 
    semi_join(swaps, by = c("year", "site", "box"))
clutchswaps2 <- bluti2 %>% 
  semi_join(swaps, by = c("year", "site", "box"))

# According to this, 241 cases (or 240, when removing cases were suc < 0) out of a total of 1694 were involved in the clutch swap experiment that took place between 2017 and 2019.
# Ideally, we want to add a new variable in  our bluti4 database that accounts for this
# For now we will simply store all this cases in a different variable to bear it in mind
clutchswaps2 %>% 
  count(year) %>%
  filter(n > 1)  # Number of cases involved in clutch swipe experiment per year



### Exploring the dataset ###

unique(bluti2$year)  # Data from years 2014-2024
bluti2 %>%
  count(year) %>%
  filter(n > 1)  # Number of cases/recordings per years

length(unique(bluti2$ring))  # We have recordings of a total of 1150 different female birds across the years, 1134 when we remove cases where suc < 0

nr_birds2 <- bluti2 %>%
  count(ring)  # storing how many times we have recorded each female (including duplicates)
nr_birds2 %>% summarise(n = mean(n))  # On average, we have recorded 1.46 breeding attempts of each female. 
# Most females only breed one year (or we only have one recording on average of each female; maybe they've had more but we haven't noticed)

max(nr_birds2[, 2])  # the maximum number of breeding attempts recorded for the same female is 7...
nr_birds2[which(nr_birds2$n == max(nr_birds2[, 2])),"ring"]  # ...and it's of female S921907

attempt1 <- nr_birds2 %>% filter(n == 1)  # 811 females for which we have only 1 breeding attempt recorded
attempt2 <- nr_birds2 %>% filter(n == 2)  # 192 females for which we have 2 breeding attempts recorded
attempt3 <- nr_birds2 %>% filter(n == 3)  # 78 females for which we have 3 breeding attempts recorded
attempt4 <- nr_birds2 %>% filter(n == 4)  # 36 females for which we have 4 breeding attempts recorded
attempt5 <- nr_birds2 %>% filter(n == 5)  # 16 females for which we have 5 breeding attempts recorded
attempt6 <- nr_birds2 %>% filter(n == 6)  # 0 females for which we have 6 breeding attempts recorded
attempt7 <- nr_birds2 %>% filter(n == 7)  # 1 females for which we have 7 breeding attempts recorded

#bluti1$YEAR <- as.factor(bluti1$year)
#bluti1$site <- as.factor(bluti1$site)
str(bluti2)
bluti2$suc <- as.numeric(bluti2$suc)
hist(bluti2$suc, xlab = "Number of chicks successfully fledged")

bluti2 %>% summarise(suc = mean(suc))  # average number of successfully fledged chicks

bluti2 %>% 
  group_by(ring) %>%
  count(age) %>%
  filter(age == 6)
ages <- bluti2 %>% 
  group_by(ring) %>%
  count(age) 
age6 <- bluti2 %>% 
  group_by(ring) %>%
  count(age) %>%
  filter(age == 6)

### Filtering the number of females at age 6 that were also recorded at age 5 (and therefore, we definitely know their age) ###
# Find individuals recorded at age 5 (code is modified from code provided by ChatGPT)
birds_at_5 <- bluti2 %>%
  filter(age == 5) %>%
  select(ring) %>%
  distinct()

# Find individuals recorded at age 6 who were also recorded at age 5
birds_at_6_with_5 <- bluti2 %>%
  filter(age == 6) %>%
  semi_join(birds_at_5, by = "ring") %>%
  count(ring) 

birds_at_only_6 <- bluti2 %>%
  filter(age == 6) %>%
  anti_join(birds_at_5, by = "ring") %>%
  count(ring)   
# There are 410 females of which we have recordings only when they're age 6 and not before; these could be problematic to include in the model
which(bluti2$age==4)  # there are also 8 recordings of which age=4 (most likely age identification on the field was not possible)

bluti2_red <- bluti2 %>% anti_join(birds_at_only_6, by = "ring")  # smaller subdataset excluding birds first identified at age 6



### Trying to create age column that specifies age (in years old) of the bird at the recorded breeding attempt with Hannah's help ###

inds <- data.frame(ring = sort(unique(bluti2$ring)), firstcaught = tapply(bluti2$year, bluti2$ring, min), firstageclass = tapply(bluti2$age, bluti2$ring, min))  # define a new dataframe with each unique female, the year she was first caught and the age she was 
inds$hatchyear <- ifelse(inds$firstageclass==6, (inds$firstcaught - 2), (inds$firstcaught - 1))  # calculate an estimated hatch year based on the age class when first caught (assume age class 4 were yearlings) 
bluti2$hatchyear <- inds$hatchyear[match(bluti2$ring, inds$ring)]  # match hatch year back to original dataframe
bluti2$yo <- bluti2$year - bluti2$hatchyear  # calculate age


### Trying to create a "age at last breeding attempt" column ###

w <- tapply(bluti2$yo, bluti2$ring, max)
w <- as.data.frame(w)
w$rings <- rownames(w)
bluti2$w <- w$w[match(bluti2$ring, w$rings)]


blutidf <- bluti2 %>% select(ring, year, site, box, fed, cs, suc, hatchyear, age, yo, w)  # re-organising dataset) 

write.xlsx(blutidf, "data/blutidf.xlsx")  # final database


## The following code is not being used anymore ##

## Creating a new data frame (tibble) that summarises breeding attempts per identified individual per year

#br_attempts <- bluti2 %>% 
#  group_by(year) %>%
#  count(ring)

## Converting br_attempts into a individual-by-year matrix
#ID_by_year <- br_attempts %>% 
#  pivot_wider(names_from=year,values_from=c(n)) %>%
#  replace_na(list('2014'=0, '2015'=0, '2016'=0, '2017'=0, '2018'=0, '2019'=0, '2020'=0, '2021'=0, '2022'=0, '2023'=0, '2024'=0))
#ring <- ID_by_year$ring
#ID_by_year <- ID_by_year[,-1]
#ID_by_year$w <- rowSums(ID_by_year)  # here we have a column that has the number of breeding attempts recorded per
#ID_by_year$ring <- ring
#last_repr <- ID_by_year[,c(12,13)]



#### Creating a new column that specifies age (in years old) of the bird at the recorded breeding attempt ###

#bluti2 %>% count(ring,year) %>% filter(n>1)  # if I'm not wrong, this means that no bird in the dataset has laid eggs successfully twice in the same year, so each recording should correspond to one year in their life 

#years <- matrix(0, nrow=1, ncol=nrow(bluti2))  # here I create a matrix that has a single row and as many columns as rows has bluti3 (as cases are, after filtering NAs)
#y <- 0  # here I create a variable that I will use within the loop 
#for (i in 1:nrow(bluti2)) {
#  y <- 0  # reset the variable to 0 after every iteration
#  for (a in 1:i) {
#    if (bluti2[a, "ring"] == bluti2[i, "ring"]) {
#      y <- y + 1  # if the ring number from the iteration has been before, add the amount of times it has appeared so far
#    }
#  }
#  years[1,i] <- y  # store for each case, how many years we've recorded that bird up until the one in which that recording was made
#}

## Now, we can combine this with the informaton we have in the "age" column in bluti3 subdataset to estimate how old was each bird in each case:

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



# Now, I want to add a new column to my dataset that includes age of last breeding attempts
# To start, I'll add a column that represents the total number of breeding attempts (which, in this case, also coincides with the number of breeding years recorded)

#bluti2$w <- 0  # adding an empty column to the data frame that will store age at last breeding attempt

#for (a in 1:nrow(bluti2)) {
#  for (i in 1:nrow(last_repr)) {
#    if (bluti2[a,"ring"] == last_repr[i,"ring"]) {
#      bluti2[a,"w"] <- last_repr[i,"w"]
#    }
#  }
#}  # this loop specifies number of total breeding attempts recorded of each female bird to each of their cases in the dataframe


## Now, to create a column for age in years old

#bluti3 <- bluti2

#bluti3$y_old <- 0

#bluti3_age6 <- bluti3 %>%
#  filter(age==6)


#n <- 0
#for (i in 1:nrow(bluti3)) {
#  if (bluti3[i,"age"]==4) {
#    bluti3[i,"y_old"] <- 1
#  } else if (bluti3[i, "age"]==5) {
#    bluti3[i,"y_old"] <- 1
#  } else if (bluti3[i, "age"]==6) {
#    n <- 1
#    for (a in 1:i) {
#      if (bluti3[i,"ring"] == bluti3[a, "ring"] & bluti3[a,"age"] == 6) {
#        n <- n + 1
#      } 
#    }
#    bluti3[i,"y_old"] <- n
#  }
#}


#n <- 0
#for (i in 1:nrow(bluti3)) {
#  if (bluti3[i,"age"]==4) {
#    bluti3[i,"y_old"] <- 1
#  } else if (bluti3[i, "age"]==5) {
#    bluti3[i,"y_old"] <- 1
#  } else if (bluti3[i, "age"]==6) {
#    n <- 2
#    for (a in 1:i) {
#      if (bluti3[i,"ring"] == bluti3[a, "ring"] & bluti3[a,"age"] == 6) {
#        diff <- bluti3[i, "year"] - bluti3[i, "year"]
#        n <- n + diff
#      } 
#    }
#    bluti3[i,"y_old"] <- n
#  }
#}    # I have to go over this again; it's not working. Look ID_by_year to check if it's working with 0s in the data

# Would it be possible to create y_old based on ID_by_year instead of based on bluti2 or bluti3?


# This most likely won't work as there are some females that are recorded every other year (intermittently)

# Now, I need to change the column "w" (age at last breeding attempt)
#bluti3 <- bluti2
#x <- 0
#for (i in 1:nrow(bluti3)) {
#  if (bluti3[i, "age"] == 6 & !(bluti3[i,"ring"] %in% x)) {
#    bluti3[i, "w"] <- bluti3[i, "w"] + 1
#  }
#  x <- c(x, bluti3[i,"ring"])
#}  # This isn't working exactly as I would want it to
