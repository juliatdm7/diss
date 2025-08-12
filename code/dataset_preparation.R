
########################
### Data preparation ###
########################

# Loading necessary packages
library(dplyr)  
library(tidyverse)

# Loading original datasets
alladults <- read.csv("data/Adults_2025.csv")  # loading adult blue tits data as a tibble
as_tibble(alladults)  # visualising first rows of data
allbirdphen <- read.csv("data/Bird_Phenology_2025.csv")  # loading phenology data as a tibble (easier to manage)
as_tibble(allbirdphen)  # visualising first rows of data

# Selecting variables that will be useful for the project from the phenology data
# Since we aim to study breeding success trends in blue tit populations across age groups and across sites, we will extract those variables that may be useful to study breeding success: first egg lay date ("fed"), clutch size ("cs") and number of fledgelings ("suc")
blutiphen <- allbirdphen %>% filter(species == "bluti")  # first, we will select and extract cases that are of blue tits
blutiphen <- blutiphen %>% select(year, site, box, fed, cs, suc)  # we also select columns that will help us collate both databases into one by identifying individuals (i.e.: year, site and box)

# Selecting variables that will be useful for the project from the adult data
adults <- alladults %>% filter(season != "winter", sex == "F")  # we will remove all data coming from adults captured in the winter (as we may not have their corresponding breeding season data) and keep data from females only.

# After this filtering, we remove 2100 observations (of male birds and of winter recordings), and we have a total of 1828 recordings of female adults ringed and identified of which we should have breeding success data in blutiphen (or, at least, of most of them)
blutiadults <- adults %>% select(ring, year, site, box, age)  # finally, we select the columns that we will use

# Rearranging datasets in ascending order of years, site and nestboxes
level_order <- c("EDI", "RSY", "FOF", "BAD", "LVN", "DOW", "GLF", "SER", "MCH", "PTH", "STY", "BIR", "DUN", "BLG", "PIT", "KCK", "KCZ", "BLA", "CAL", "DNM", "DNC", "DNS", "DLW", "CRU", "NEW", "HWP", "INS", "FSH", "RTH", "AVI", "AVN", "CAR", "SLS", "TOM", "DAV", "ART", "MUN", "FOU", "ALN", "DEL", "TAI", "SPD", "OSP", "DOR")
blutiphen <- blutiphen %>% arrange(year, factor(site, levels = level_order), box) 
blutiadults <- blutiadults %>% arrange(year, factor(site, levels = level_order), box)  



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
  filter(n > 1)  # there seem to be 10 duplicates in blutiadults
# These duplicates are re-measurements of the same females' tarsus length and body mass but later on (1-5 days later).

blutiadults %>%
  count(ring, year, site, box) %>%
  filter(n > 1)  # In nestbox 7 of site SPD in year 2018 two females were caught for the same nest. What should we do about it?

# I'm particularly interested in removing duplicates so that they don't complicate calculating the age of each individual in each event we've recorded of them
# I'm not particularly worried about each case is removed for blutiadults as both will store the same information
# However, for blutiphen I am interested in storing the re-lays rather than the failed breeding attempts (ask Hannah confirmation about this).
# I'm gonna try to rearrange my dataset so that relays appear before failed attempts as the unique() function in dplyr will keep the first row it encounters.
# I will keep the first rows because those are the ones for which most likely we have information on the females.

blutiphen <- blutiphen %>% arrange(year, factor(site, levels = level_order), desc(suc), box)  # now, the first row in the duplicates should be the failed attempt
uqblutiphen <- distinct(blutiphen, year, site, box, .keep_all = T) 

uqblutiadults <- distinct(blutiadults, ring, year, site, box, .keep_all = T)

# Now, not all adults are caught (e.g. when broods fail before nestlings reach they 10, when one of the parents dies or abandons the nest, etc.), therefore there might be observations in `blutiphen` that do not have a corresponding match in the `blutiadults` database:
# I will use the function anti_join() to find it out:
no_adult_data <- uqblutiphen %>%
  anti_join(uqblutiadults, by = c("year", "site", "box"))  # there are 412 rows in  blutiphen for which we do not have data in blutiadults ( most likely, the female adult was not caught)
# We store these problematic cases in their separate rows in case we want to reach for them in the future

no_bs_data <- uqblutiadults %>%
  anti_join(uqblutiphen, by = c("year", "site", "box"))  # There are also two adult female birds for which there is no matching breeding data. These are exceptional cases in which females were found incubating empty nests. They will be removed.


# I will only include in the database the shared cases between blutiphen and blutiadults because lack of data of either of them is problematic
bluti1 <- uqblutiphen %>%
  inner_join(uqblutiadults, by =c("year", "site", "box"))  

# There are some cases for which breeding success is -999. These are cases were nests were predated and, instead of indicating 0 as fledgeling success, -999 was written down in case they needed to be excluded at some point.
# I will create a new bluti2 variable in which those cases show "NA" for fledgeling success
bluti2 <- bluti1
bluti2[which(bluti2$suc < 0),"suc"] <- NA
bluti2 <- bluti2[-which(bluti2$cs > 14),]  # There are only three cases in the entire dataset where cs > 14. 
# Two of them belong to the same nest box and the same year, but have different females. 
# My guess is that two females were laying eggs in the same nest box, and since we cannot really tear appart to whom do both fed and suc correspond (and they're different ages as well), then the right call is to remove this cases entirely.
# As for the last one, I'd argue that is probably a very similar case except that the other female was never caught. I would also remove the whole line (fed and suc included) for this reason
bluti2 <- bluti2 %>% relocate(ring, year, site, box, age, fed, cs, suc)

nrow(bluti2)  # 1677 observations without 2025 data, 1814 observations with 2025 data

length(unique(bluti2$ring))  # 1148 females without 2025 data, 1220 females with it

### Brief dataset exploration ###

unique(bluti2$year)  # Data from years 2014-2025
bluti2 %>%
  count(year) %>%
  filter(n > 1)  # Number of cases/recordings per years

length(unique(bluti2$ring))  # We have recordings of a total of 1220 different female birds across the years

nr_birds <- bluti2 %>%
  count(ring)  # storing how many times we have recorded each female (including duplicates)
mean(nr_birds$n)  # On average, we have recorded 1.486885 breeding attempts of each female. 
# Most females only breed one year (or we only have one recording on average of each female; maybe they've had more but we haven't noticed)

max(nr_birds[, 2])  # the maximum number of breeding attempts recorded for the same female is 8...
nr_birds[which(nr_birds$n == max(nr_birds[, 2])),"ring"]  # ...and it's of female S921907

str(bluti2)
bluti2$suc <- as.numeric(bluti2$suc)

# Number of birds first recorded as yearlings

# Yearlings fall within age category 5, but in some cases people on the field mark them as age category 4 by mistake:

age4 <- bluti2 %>% filter(age == 4)  # there are  8 recordings of which age=4, which are most likely yearlings

age5 <- bluti2 %>% 
  filter(age <= 5) %>%
  group_by(ring) %>%
  count(ring)  # 765 unique female birds were first identified as yearlings

nrow(age5)

age6 <- bluti2 %>% 
  filter(age == 6) %>%
  group_by(ring) %>%
  count(ring)  # 697 unique females were captured as 2+ year olds

only_age6 <- anti_join(x = age6, y = age5, by = "ring")  # 559 females were first identified as 2+ year olds (~37% of all females)

nrow(only_age6)

length(unique(only_age6$ring))


### Estimating birds' age ###

# Identifying ringed individuals that might have been ringed in the project sites

nestlings <- read.csv("data/NestlingsII.csv")
nestlings <- nestlings %>%
  select(ring, year, site, box) %>%
  rename(ringedyear = year) %>%
  arrange(ringedyear, site, box)

bluti3 <- bluti2
bluti3$ringedyear <- NA
ring_nr <- 0
for (i in 1:nrow(bluti3)) {
  if (bluti3[i,"ring"] %in% nestlings$ring) {
    ring_nr <- as.character(bluti3[i,"ring"])
    row <- which(nestlings$ring == ring_nr)
    bluti3[i,"ringedyear"] <- nestlings[row, "ringedyear"]
  }
}  
length(which(!is.na(bluti3$ringedyear)))  # There's only 32 (female) birds that hatched in the project and that were seen breeding in the nest boxes after that. 

# We know exactly the hatching year of 30 female birds. For the remaining ones (N = 1448), we will need to estimate their age based on how we aged them on the field.
# Birds aged 5 are 1 year old, while birds aged 6 are 2 or more years old.
# We will assume that when we first get a bird aged 6, the bird was 2 y.o. on that year.

inds <- data.frame(ring = sort(unique(bluti3$ring)), 
                   firstcaught = tapply(bluti3$year, bluti3$ring, min), 
                   firstageclass = tapply(bluti3$age, bluti3$ring, min))  # define a new dataframe with each unique female, the year she was first caught and the age she was 
inds$hatchyear <- ifelse(inds$firstageclass==6, (inds$firstcaught - 2), (inds$firstcaught - 1))  # calculate an estimated hatch year based on the age class when first caught (assume age class 4 were yearlings) 
bluti3$hatchyear <- inds$hatchyear[match(bluti3$ring, inds$ring)]  # match hatch year back to original dataframe
for (i in 1:nrow(bluti3)) {
  if (!is.na(bluti3[i,"ringedyear"])) {
    bluti3[i, "hatchyear"] <- bluti3[i, "ringedyear"]
  }
}
bluti3$yo <- bluti3$year - bluti3$hatchyear  # calculate age


### Trying to create a "age at last breeding attempt" column ###

w <- tapply(bluti3$yo, bluti3$ring, max)
w <- as.data.frame(w)
w$rings <- rownames(w)
bluti3$w <- w$w[match(bluti3$ring, w$rings)]

### Rearranging dataset ###

blutidf <- bluti3 %>% 
  select(ring, year, site, box, fed, cs, suc, hatchyear, ringedyear, age, yo, w) %>%
  arrange(year, factor(site, levels = level_order), box)


### Excluding observations within clutch swaps experiment ###

# Between 2017 and 2019, a clutch-swap experiment took place. The results of these experiments may have an influence on suc, and we should account for this.
# First, we need to find how many and which cases in the bluti4 database were part of this experiment:
swaps <- read.csv("data/clutchswaps.csv")
swaps
swaps <- swaps %>% rename(box = origin.nest)

# My aim is to search my current database to find the cases that took part in the experiment and switch their suc value into "NA", as it may have been impacted by the experiment and, therefore, obscure age effects from the actual parents and of the surrounding environment
# In order to do so, first I will assign a case number to each case (row) in my database:
blutidf$case <- seq(from=1, to=nrow(blutidf), by=1)

# Now, I'll create a data frame that will store the cases in my database included in the clutch swap experiments
clutchswaps <- blutidf %>% 
  semi_join(swaps, by = c("year", "site", "box"))

clutchswaps %>% 
  count(year) # Number of cases involved in clutch swipe experiment per year
# According to this, 234 out of a total of 1660 were involved in the clutch swap experiment that took place between 2017 and 2019.

clutchswaps %>% 
  count(ring)  # It seems that a total of 201 female birds were involved in the experiment

# Now, based on the case number assigned to each row in the blutidf database, I will switch suc value to NA whenever the case is found in the dataframe that stores cases included in the experiment

for (i in 1:nrow(blutidf)) {
  if (blutidf[i,"case"] %in% clutchswaps$case) {
    blutidf[i,"suc"] <- NA
  }  
}

# I think it would be interesting to add a column in which fledgeling success is a proportion (proportion of eggs laid that were successfully fledged). 
# This proportion will take the complete clutch into account, rather than the number of hatched eggs. However, it would be potentially more accurate to use the number of hatched eggs as base (maybe substracting the number of unhatched eggs found in V1 to the clutch size)

blutidf$suc_prop <- blutidf$suc/blutidf$cs

blutidf <- blutidf %>% relocate(suc_prop, .before=hatchyear)

### Creating a database for recordings of females that are only between 1 and 3 years old

blutidf_3yo <- blutidf[which(blutidf$yo <= 3),]
length(unique(blutidf_3yo$ring))  # We have 1219 females in total including 2025 data

#write.csv(blutidf, "data/blutidf_2025.csv")
#write.xlsx(blutidf, "data/blutidf_2025.xlsx") 

#write.csv(blutidf_3yo, "data/blutidf_3yo_2025.csv")
#write.xlsx(blutidf_3yo, "data/blutidf_3yo_2025.xlsx")  # final database (with observations where females are only up to 3 years old)

population <- blutidf_3yo %>% 
  count(year,yo) %>%
  pivot_wider(names_from=year,values_from=c(n))  # There are many less 1-year-olds in 2025 than in any other year

mean(blutidf$w)  # females live on average 2.496141 years old on this study system

#The number of observations of 3 years-old individuals per year is quite low overall, and it's almost always less than 20% of the population (2022 is the sole exception); birds aged 3 years old usually represent between 10 and 15% of the total of the (female) population.



### Creating separate data frames for each response variable ###

length(which(is.na(blutidf_3yo$fed)))  # There are 351 observations for which we have no first egg lay date recordings

blutidf_3yo[which(is.na(blutidf_3yo$fed)),]  

# Quite a few of these belong to sites that are visited less often (most sites in the Northern part of the transect, starting with HWP and until DOR, as well as sites in the southern transect that after a certain year started to receive visits less often: BAD, DLW and DUN) and, therefore, estimation of first egg lay date based on the number of eggs present in the nest box once its inspected is less accurate. 
# However, and potentially more relevant, most of these fed-lacking observations belong to the year 2020, year in which the sanitary emergency caused by the COVID-19 pandemic provided an obstacle to conduct fieldwork following the standard protocol.

fed_df <- blutidf_3yo %>% filter(!is.na(fed))

nrow(fed_df) # In the separate dataset for fed there are 1302 observations... (1233 without 2025 data)

length(unique(fed_df$ring)) #... of 1006 unique (female) bird rings (965 without 2025 data)

#write.csv(fed_df, "data/fed_df_2025.csv")

cs_df <- blutidf_3yo %>% filter(!is.na(cs))

nrow(cs_df)  # In the separate dataset for cs there are 1600 observations... (1488 without 2025 data)

length(unique(cs_df$ring))  #... of 1188 unique (female) bird rings (1117 without 2025 data)

#write.csv(cs_df, "data/cs_df_2025.csv")

suc_df <- blutidf_3yo %>% filter(!is.na(suc))

nrow(suc_df)  # In the separate dataset for suc there are 1413 observations... (1303 without 2025 data)

length(unique(suc_df$ring))  #... of 1079 unique (female) bird rings (1008 without 2025 data)

#write.csv(suc_df, "data/suc_df_2025.csv")


# Now, I need to match each observation in the dataset to its corresponding total foliage score value:

habitat_foliage_scores <- read.csv("data/Habitat_SiteII.csv")

fed_df$total_FS <- habitat_foliage_scores[match(fed_df$site, habitat_foliage_scores$Site),"Total"]

