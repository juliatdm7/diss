
########################
### Data exploration ###
########################

# Loading all necessary packages

library(dplyr)  
library(tidyverse)
library(openxlsx)
library(gridExtra)
library(vegan)
library(tidyr)
library(ggplot2)

# Loading datasets

alladults <- as_tibble(read.csv("data/AdultsII.csv"))  # loading complete adult blue tits data as a tibble
allbirdphen <- as_tibble(read.csv("data/Bird_PhenologyII.csv"))  # loading complete phenology data as a tibble (easier to manage)
blutiphen <- allbirdphen %>% filter(species == "bluti")  # first, we will select and extract cases that are of blue tits

blutidf <- read.csv("C:/Users/julia/OneDrive - University of Edinburgh/EEB_MSc_UEd/Dissertation/diss/data/blutidf.csv")
blutidf_3yo <- read.csv("C:/Users/julia/OneDrive - University of Edinburgh/EEB_MSc_UEd/Dissertation/diss/data/blutidf_3yo.csv")
sites <- read.csv("C:/Users/julia/OneDrive - University of Edinburgh/EEB_MSc_UEd/Dissertation/diss/data/site_detailsII.csv")
habitat <- read.csv("C:/Users/julia/OneDrive - University of Edinburgh/EEB_MSc_UEd/Dissertation/diss/data/Habitats.csv")  
habitat_foliage_scores <- read.csv("C:/Users/julia/OneDrive - University of Edinburgh/EEB_MSc_UEd/Dissertation/diss/data/Habitat_SiteII.csv")  
environment <- read.csv("data/environment.csv")

caterpillars <- read.csv("data/Lepidoptera_Abundance.csv")

# Checking for correlation between environmental variables

library(ggstatsplot)

remotes::install_github("easystats/correlation")

env <- environment[,3:21]

corrplot1 <- ggstatsplot::ggcorrmat(
  data = env,
  type = "parametric", # parametric for Pearson, nonparametric for Spearman's correlation
  colors = c("darkred", "white", "steelblue") # change default colors
)


# Checking for correlation between response variables

blutiphen <- blutiphen[-which(blutiphen$suc < 0),]

blutiphen$suc <- as.numeric(blutiphen$suc)

rv <- blutiphen[,c(11,13,15,21,37)]

corrplot2 <- ggstatsplot::ggcorrmat(
  data = rv,
  type = "parametric", # parametric for Pearson, nonparametric for Spearman's correlation
  colors = c("darkred", "white", "steelblue") # change default colors
)

# Caterpillar data
caterpillars <- caterpillars %>% 
  rename(year = Year) %>%
  rename(site = Site)

cater_birdphen <- caterpillars %>%
  inner_join(blutiphen, by =c("year", "site"))

cor.test(cater_birdphen$Number.present.total, cater_birdphen$fed, method = "pearson")  # I'm honestly not sure this makes sense, but I would like to do something like this

cor.test(cater_birdphen$Number.present.total, cater_birdphen$suc, method = "pearson")


# Male age (finding correlations with female age)

alladults <- alladults %>% filter(season != "winter")

nestlings <- as_tibble(read.csv("data/NestlingsII.csv"))
nestlings <- nestlings %>%
  select(ring, year, site, box) %>%
  rename(ringedyear = year) %>%
  arrange(ringedyear, site, box)

alladults$ringedyear <- NA
ring_nr <- 0
for (i in 1:nrow(alladults)) {
  if (alladults[i,"ring"] %in% nestlings$ring) {
    ring_nr <- as.character(alladults[i,"ring"])
    row <- which(nestlings$ring == ring_nr)
    alladults[i,"ringedyear"] <- nestlings[row, "ringedyear"]
  }
}


indsx <- data.frame(ring = sort(unique(alladults$ring)), firstcaught = tapply(alladults$year, alladults$ring, min), firstageclass = tapply(alladults$age, alladults$ring, min))  # defining a new dataframe with each unique bird, the year they were first caught and the age they were
indsx$hatchyear <- ifelse(indsx$firstageclass==6, (indsx$firstcaught - 2), (indsx$firstcaught - 1))  # calculating an estimated hatch year based on the age class when first caught (assume age class 4 were yearlings) 
alladults$hatchyear <- indsx$hatchyear[match(alladults$ring, indsx$ring)]  # match hatch year back to original dataframe
for (i in 1:nrow(alladults)) {
  if (!is.na(alladults[i,"ringedyear"])) {
    alladults[i, "hatchyear"] <- alladults[i, "ringedyear"]
  }
}
alladults$yo <- alladults$year - alladults$hatchyear  # calculating age in years old


### Trying to create a "age at last breeding attempt" column ###

w <- tapply(alladults$yo, alladults$ring, max)
w <- as.data.frame(w)
w$rings <- rownames(w)
alladults$w <- w$w[match(alladults$ring, w$rings)]

age_df <- alladults[,c(2,3,4,6,19)]

age_df <- age_df %>% 
  arrange(year, factor(site, levels = level_order), box)

dupes <- age_df %>% summarise(n = dplyr::n(), .by = c(year, site, box, sex)) %>% filter(n > 1L) 

age_df <- age_df %>% distinct(year, site, box, sex, .keep_all = T)  # keeping unique recordings (one of the duplicates is removed, I don't choose which one; definitely not the best course of action, but it's a place to start)

#box1 <- data.frame(year = NA, site = NA, box = NA)
#box2 <- data.frame(year = NA, site = NA, box = NA)

#for (i in age_df) {
#  box1[1,"year"] <- age_df[i, "year"]
#  box1[1,"site"] <- age_df[i, "site"]
#  box1[1,"box"] <- age_df[i, "box"]
  
#  for (a in dupes) {
#    box2[1,"year"] <- dupes[1, "year"]
#    box2[1,"site"] <- dupes[1, "site"]
#    box2[1,"box"] <- dupes[1, "box"]
    
#    if (box1[1,"year"] == box2[1,"year"] & box1[1,"site"] == box2[1,"site"] & box1[1,"box"] == box2[1,"box"]) {
#      age_df <- age_df[-i,] 
#    }
#  }
#}
  
age_wide <- age_df %>% pivot_wider(names_from=sex,values_from=c(yo))

age_wide <- age_wide %>% 
  rename(yo_F = F) %>%
  rename(yo_M = M)

cor.test(age_wide$yo_M, age_wide$yo_F, method = "pearson")  

# It appears that, when computing a Pearson correlation test (which I'm not sure it's the best), male age and female age of the same year, site and nest box (age of the female and the male withing the same pair) show a significant positive correlation (cor = 0.2558343, p-value < 2.2e-16)
# However, it's really not a 1 by 1 correlation nor is it particularly strong.

ggplot(age_wide, aes(x=yo_F, y=yo_M)) +
  geom_jitter(alpha = 0.25) +
  geom_smooth(method='lm', formula= y~x) +
  theme_bw() +
  labs(x = "Age of the female in a pair (y.o.)", y = "Age of the male in a pair (y.o.)") +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(margin = unit(c(1, 0, 0, 0), "mm")), 
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))  +
  scale_x_continuous(breaks = seq(1,7,1)) +
  scale_y_continuous(breaks = seq(1,7,1))
