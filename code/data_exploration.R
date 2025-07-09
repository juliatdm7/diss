
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

blutidf <- read.csv("C:/Users/julia/OneDrive - University of Edinburgh/EEB_MSc_UEd/Dissertation/diss/data/blutidf_II.csv")
blutidf_3yoX <- read.csv("C:/Users/julia/OneDrive - University of Edinburgh/EEB_MSc_UEd/Dissertation/diss/data/blutidf_3yo_II.csv")
sites <- read.csv("C:/Users/julia/OneDrive - University of Edinburgh/EEB_MSc_UEd/Dissertation/diss/data/site_detailsII.csv")
habitat <- read.csv("C:/Users/julia/OneDrive - University of Edinburgh/EEB_MSc_UEd/Dissertation/diss/data/Habitats.csv")  
habitat_foliage_scores <- read.csv("C:/Users/julia/OneDrive - University of Edinburgh/EEB_MSc_UEd/Dissertation/diss/data/Habitat_SiteII.csv")  
environment <- read.csv("data/environment.csv")

caterpillars <- read.csv("data/Lepidoptera_Abundance.csv")

bluti_3yo <- read.csv("data/blutidf_3yo_II.csv")
fed_df <- read.csv("data/fed_df_II.csv")
cs_df <- read.csv("data/cs_df_II.csv")
suc_df <- read.csv("data/suc_df.csv")

### Sample sizes of each breeding trait by age ### 

# 1. First egg lay date

fed_age <- fed_df %>% count(yo)

fed_uq_rings <- as.data.frame(unique(fed_df$ring)) %>% rename(rings = "unique(fed_df$ring)")

fed_uq_rings$w <- fed_df[match(fed_uq_rings$rings, fed_df$ring), "w"]

fed_w <- fed_uq_rings %>% count(w)

fed_age  # here we can see the number of observations per age group

fed_w  # and here we can see the number of observations per age-at-last-reproduction group

fedplot1 <- ggplot(fed_df, aes(x=yo)) +
  geom_freqpoly(colour="#248fc9", binwidth=1, linejoin = "round") +
  geom_point(stat="bin", aes(y=after_stat(count)), binwidth=1, colour="black", size = 2) +  
  geom_label(stat="count", label = fed_age$n, vjust=-0.5) + 
  theme_bw() +
  scale_x_continuous(breaks=seq(1,3,1), limits=c(1,3)) +
  scale_y_continuous(limits=c(0,800)) +
  labs(x = "Age (in years old)", y = "Observations (fed)", title = "Overall number of female recordings per age group") +
  theme( axis.title = element_text(size = 13), 
         axis.text = element_text(size = 11),
         axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
         axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

fedplot2 <- ggplot(fed_uq_rings, aes(x=w)) +
  geom_freqpoly(colour="#248fc9", binwidth=1, linejoin = "round") +
  geom_point(stat="bin", aes(y=after_stat(count)), binwidth=1, colour="black", size = 2) + 
  geom_label(stat="count", label = fed_w$n, vjust=-0.5) + 
  theme_bw() +
  scale_x_continuous(breaks=seq(1,7,1), limits=c(1,7)) +
  scale_y_continuous(limits=c(0,800)) +
  labs(x = "Age at last reproduction (w)", y = "Unique females (fed)", title = "Female longevity") +
  theme( axis.title = element_text(size = 13), 
         axis.text = element_text(size = 11),
         axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
         axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

grid.arrange(fedplot1, fedplot2, ncol=2, nrow = 1)


# 2. Clutch size

cs_age <- cs_df %>% count(yo)

cs_uq_rings <- as.data.frame(unique(cs_df$ring)) %>% rename(rings = "unique(cs_df$ring)")

cs_uq_rings$w <- cs_df[match(cs_uq_rings$rings, cs_df$ring), "w"]

cs_w <- cs_uq_rings %>% count(w)

cs_age  # here we can see the number of observations per age group

cs_w  # and here we can see the number of observations per age-at-last-reproduction group

csplot1 <- ggplot(cs_df, aes(x=yo)) +
  geom_freqpoly(colour="#248fc9", binwidth=1, linejoin = "round") +
  geom_point(stat="bin", aes(y=after_stat(count)), binwidth=1, colour="black", size = 2) +  
  geom_label(stat="count", label = cs_age$n, vjust=-0.5) + 
  theme_bw() +
  scale_x_continuous(breaks=seq(1,3,1), limits=c(1,3)) +
  scale_y_continuous(limits=c(0,800)) +
  labs(x = "Age (in years old)", y = "Observations (cs)", title = "Overall number of female recordings per age group") +
  theme( axis.title = element_text(size = 13), 
         axis.text = element_text(size = 11),
         axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
         axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

csplot2 <- ggplot(cs_uq_rings, aes(x=w)) +
  geom_freqpoly(colour="#248fc9", binwidth=1, linejoin = "round") +
  geom_point(stat="bin", aes(y=after_stat(count)), binwidth=1, colour="black", size = 2) + 
  geom_label(stat="count", label = cs_w$n, vjust=-0.5) + 
  theme_bw() +
  scale_x_continuous(breaks=seq(1,7,1), limits=c(1,7)) +
  scale_y_continuous(limits=c(0,800)) +
  labs(x = "Age at last reproduction (w)", y = "Unique females (cs)", title = "Female longevity") +
  theme( axis.title = element_text(size = 13), 
         axis.text = element_text(size = 11),
         axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
         axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

grid.arrange(csplot1, csplot2, ncol=2, nrow = 1)


# 3. Fledgeling success

suc_age <- suc_df %>% count(yo)

suc_uq_rings <- as.data.frame(unique(suc_df$ring)) %>% rename(rings = "unique(suc_df$ring)")

suc_uq_rings$w <- suc_df[match(suc_uq_rings$rings, suc_df$ring), "w"]

suc_w <- suc_uq_rings %>% count(w)

suc_age  # here we can see the number of observations per age group

suc_w  # and here we can see the number of observations per age-at-last-reproduction group

sucplot1 <- ggplot(suc_df, aes(x=yo)) +
  geom_freqpoly(colour="#248fc9", binwidth=1, linejoin = "round") +
  geom_point(stat="bin", aes(y=after_stat(count)), binwidth=1, colour="black", size = 2) +  
  geom_label(stat="count", label = suc_age$n, vjust=-0.5) + 
  theme_bw() +
  scale_x_continuous(breaks=seq(1,3,1), limits=c(1,3)) +
  scale_y_continuous(limits=c(0,800)) +
  labs(x = "Age (in years old)", y = "Observations (suc)", title = "Overall number of female recordings per age group") +
  theme( axis.title = element_text(size = 13), 
         axis.text = element_text(size = 11),
         axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
         axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

sucplot2 <- ggplot(suc_uq_rings, aes(x=w)) +
  geom_freqpoly(colour="#248fc9", binwidth=1, linejoin = "round") +
  geom_point(stat="bin", aes(y=after_stat(count)), binwidth=1, colour="black", size = 2) + 
  geom_label(stat="count", label = suc_w$n, vjust=-0.5) + 
  theme_bw() +
  scale_x_continuous(breaks=seq(1,7,1), limits=c(1,7)) +
  scale_y_continuous(limits=c(0,800)) +
  labs(x = "Age at last reproduction (w)", y = "Unique females (suc)", title = "Female longevity") +
  theme( axis.title = element_text(size = 13), 
         axis.text = element_text(size = 11),
         axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
         axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

grid.arrange(sucplot1, sucplot2, ncol=2, nrow = 1)


### Exploring breeding traits ###


## First egg lay date ##

# Overall first egg lay date 
ggplot(blutidf, aes(fed)) +
  geom_histogram(colour = "black", fill = "#248fc9", binwidth=1) +
  labs(x = "First egg lay date (1st Jan = 1)", y = "Frequency") +
  theme_bw() +
  scale_x_continuous(breaks=seq(90,200,5)) +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))


# Overall first egg lay date (up until 3 y.o.)  
ggplot(fed_df, aes(fed)) +
  geom_histogram(colour = "black", fill = "#248fc9", binwidth=1) +
  geom_vline(xintercept = mean(fed_df$fed), colour = "maroon", linewidth = 1.2, linetype = 2) +
  labs(x = "First egg lay date (1st Jan = 1)", y = "Frequency") +
  theme_bw() +
  scale_x_continuous(breaks=seq(90,200,5)) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,80)) +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))
#ggsave("figures/hist_fed_bluti3yo_II.png")  # at first glance, although it looks a bit shifted, first egg lay date appears to be more or less Normally distributed

qqnorm(blutidf_3yo$fed); qqline(blutidf_3yo$fed, col = 2)  # the observations mostly align with the diagonal that crosses through the first and third quantiles of the theoretical Normal distribution, except for the tails of the data distribution.

# Once we've looked at the trait distribution overall, we can take a look at how this distribution changes when we plot observations both by age and by age at last reproductive attempt:

# First egg lay date by age(up until 3 y.o.) 
ggplot(fed_df, aes(fed)) +
  geom_histogram(colour = "black", fill = "#248fc9", binwidth=1) +
  labs(x = "First egg lay date (1st Jan = 1)", y = "Frequency") +
  theme_bw() +
  facet_wrap(~ yo) +
  scale_x_continuous(breaks=seq(90,200,5)) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,40)) +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12, angle = 90))

# First egg lay date by age at last reproductive attempt 
ggplot(fed_df, aes(fed)) +
  geom_histogram(colour = "black", fill = "#248fc9", binwidth=1) +
  labs(x = "Number of chicks fledged successfully", y = "Frequency") +
  theme_bw() +
  facet_wrap(~ w) +
  scale_x_continuous(breaks=seq(90,200,5)) +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12, angle = 90)) 

# The main thing that catches one's attention is the decrease in frequency as age and age at last reproduction increases.

# We could also summarise the variable in terms of mean value(s), standard error(s), minimum and maximum value(s)...

mean(fed_df$fed)  # On average, females tend to start laying eggs on ordinal day 121 (~1st of May)

se <- function(x) {
  sd(x)/(sqrt(length(x)))
}  # this rudimentary function should be able to calculate the SE...

se(fed_df$fed)  # ...as shown here

fed_df %>% group_by(yo) %>% summarise_at(vars(fed), list(mean=mean, sd=sd, SE=se, min=min, max=max)) %>% as.data.frame()  # summarising the data to mean, standard deviation, min. value and max. value 

#  One can also get the impression that the mean increases as age increases, however the decrease between age groups is of barely 1-2 days on average (as we can see on the summary table above). As for the range of values, the distribution appears to be slightly narrower for observations at age 3 (y.o.) (the earlier recorded first egg lay date is 4 days later than for younger birds but the latest is at least 5 days earlier than the last first egg lay date observed in younger birds). 

# Let's plot mean +- SE over raw (noisy) data now.

# By site:

fed_summary <- fed_df %>%
  group_by(year) %>%
  summarise_at(vars(fed), list(mean=mean, SE=se)) %>% 
  as.data.frame()  # summarising first egg lay date observations by calculating its mean and standard deviation

ggplot() +
  geom_jitter(data=fed_df, aes(x=year, y=fed), size=2, alpha=0.25, colour = "#248fc9") +
  geom_errorbar(data=fed_summary, aes(x=year, y=mean, ymin=mean-SE, ymax=mean+SE)) +
  geom_point(data=fed_summary, aes(x=year, y=mean), colour = "black", size = 2, alpha = 0.8) +
  labs(x = "Year", y = "First egg lay date (1st Jan = 1)", title = "First egg lay date raw data and mean+sd") +
  theme_bw() +
  scale_x_continuous(breaks=seq(2014,2024,1)) +
  scale_y_continuous(breaks=seq(90,200,5)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))  # There's quite a lot of variation from one year to another, and we can clearly see that there are much fewer observations during 2020. This points clearly towards the need to include year random effects in the models.

# By site

fed_summary <- fed_df %>%
  group_by(site) %>%
  summarise_at(vars(fed), list(mean=mean, SE=se)) %>% 
  as.data.frame()  # summarising first egg lay date observations by calculating its mean and standard deviation

ggplot() +
  geom_jitter(data=fed_df, aes(x=site, y=fed), size=2, alpha=0.25, colour = "#248fc9") +
  geom_errorbar(data=fed_summary, aes(x=site, y=mean, ymin=mean-SE, ymax=mean+SE)) +
  geom_point(data=fed_summary, aes(x=site, y=mean), colour = "black", size = 2, alpha = 0.8) +
  labs(x = "Site", y = "First egg lay date (1st Jan = 1)", title = "First egg lay date raw data and mean+sd") +
  theme_bw() +
  scale_x_discrete(limits = level_order) +
  scale_y_continuous(breaks=seq(90,200,5)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 90),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))  # At first glance, there appears to be sites on which, on average, first egg lay date is earlier and where it appears to be later (also on average): there's quite a bit of site-to-site variation. On average, first egg lay date lies between day 109 and 131 across sites, with some sites having first egg lay date as early as day ~100 and as late as day ~146. To me, this suggest that there is are important differences between sites (as we initially expected), which also implies that site random effects should be included.

# By age

fed_summary <- fed_df %>%
  group_by(yo) %>%
  summarise_at(vars(fed), list(mean=mean, SE=se)) %>% 
  as.data.frame()

ggplot() +
  geom_jitter(data=fed_df, aes(x=yo, y=fed), size=2, alpha=0.25, colour = "#248fc9") +
  geom_errorbar(data=fed_summary, aes(x=yo, y=mean, ymin=mean-SE, ymax=mean+SE)) +
  geom_point(data=fed_summary, aes(x=yo, y=mean), colour = "black", size = 2, alpha = 0.8) +
  labs(x = "Age", y = "First egg lay date (1st Jan = 1)", title = "First egg lay date raw data and mean+sd") +
  theme_bw() +
  scale_y_continuous(breaks=seq(90,200,5)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))  # First egg lay date does, indeed, decrease as age increases

# By age at last reproductive event

fed_summary <- fed_df %>%
  group_by(w) %>%
  summarise_at(vars(fed), list(mean=mean, SE=se)) %>% 
  as.data.frame()

ggplot() +
  geom_jitter(data=fed_df, aes(x=w, y=fed), size=2, alpha=0.25, colour = "#248fc9") +
  geom_errorbar(data=fed_summary, aes(x=w, y=mean, ymin=mean-SE, ymax=mean+SE)) +
  geom_point(data=fed_summary, aes(x=w, y=mean), colour = "black", size = 2, alpha = 0.8) +
  labs(x = "Age at last reproductive event", y = "First egg lay date (1st Jan = 1)", title = "First egg lay date raw data and mean+sd") +
  theme_bw() +
  scale_y_continuous(breaks=seq(90,200,5)) +
  scale_x_continuous(breaks=seq(1,7,1)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))  # Here, we can see that between 1 and 3 years of longevity, first egg lay date is earlier. However, after that the trend is more irregular (since we have fewer observations)


## Clutch size ##

# Histogram of overall cs
ggplot(blutidf, aes(cs)) +
  geom_histogram(colour = "black", fill = "#248fc9", binwidth=1) +
  labs(x = "Clutch size", y = "Frequency") +
  theme_bw() +
  scale_x_continuous(breaks=seq(0,15,2)) +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))


# Histogram of overall cs (up until 3 y.o.)  
ggplot(cs_df, aes(cs)) +
  geom_histogram(colour = "black", fill = "#248fc9", binwidth=1) +
  geom_vline(xintercept = mean(cs_df$cs), colour = "maroon", linewidth = 1.2, linetype = 2) +
  labs(x = "Clutch size", y = "Frequency") +
  theme_bw() +
  scale_x_continuous(breaks=seq(0,15,1)) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,400)) +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))
#ggsave("figures/hist_cs_bluti3yo_II.png")

# The sampling distribution of `cs` appears to be more normally distributed than the sampling distribution of `fed`, which should make things easier:

qqnorm(cs_df$cs); qqline(cs_df$cs, col = 2)

# However, the Q-Q plot looks odd. 
# As with `fed`, the plot highlight that the data is count data, for which (if I'm not mistaken) the best distribution to model it would be a Poisson distribution, although Sanjana mentioned that this might not be necessary since it's been shown that the model outputs usually don't change as much. 
# Apart from that, it also looks like the distribution is light-tailed according to the shape of the Q-Qplot.

# Again, we can now take a look at how this distribution changes when we plot observations both by age and by age at last reproductive attempt:

# Clutch size by age(up until 3 y.o.) # 
ggplot(cs_df, aes(cs)) +
  geom_histogram(colour = "black", fill = "#248fc9", binwidth=1) +
  labs(x = "Clutch size", y = "Frequency") +
  theme_bw() +
  facet_wrap(~ yo) +
  scale_x_continuous(breaks=seq(0,15,1)) +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))

# Clutch size by age at last reproductive attempt #
ggplot(cs_df, aes(cs)) +
  geom_histogram(colour = "black", fill = "#248fc9", binwidth=1) +
  labs(x = "Number of chicks fledged successfully", y = "Frequency") +
  theme_bw() +
  facet_wrap(~ w) +
  scale_x_continuous(breaks=seq(0,13,1)) +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

# And we can summarise the variable:

cs_df %>% group_by(yo) %>% summarise_at(vars(cs), list(mean=mean, sd=sd, SE=se, min=min, max=max)) %>% as.data.frame()

# When we look at the mean clutch size per age group, we can see that, on average, it increases as age increases: it increases by 0.29 eggs (around 3% increase) from 1 y.o. to 2 y.o, but then it decreases between 2 y.o. and 3 y.o. by 0.05 eggs (0.6 % decrease). However, the minimum at age 3 is 4, whereas it's 2 and 1 for 2 y.o. and 1 y.o. respectively. The maximum decreases at 13 y.o. 

# Let's summarise by age at last reproductive attempt now:

cs_df %>% group_by(w) %>% summarise_at(vars(cs), list(mean=mean, sd=sd, SE=se, min=min, max=max)) %>% as.data.frame()  

# On average, clutch size also increases up until 7 y.o. at last reproductive attempt (although SE is greater). The maximum clutch size is found in birds that are estimated to reproduce only once or twice. 
# Without further insight, my first though is that birds that invest more energy and resources from their first reproductive attempt wear out sooner and die sooner, whereas birds that don't invest as much in each reproductive attempt that they undergo manage to save enough energy to live another year and, therefore, reproduce once more. 
# It's interesting though how clutch size still increases on average (until 7 y.o. at last reproductive attempt).

# Let's plot clutch size raw data...

#...by year:

bluti_cs_summary <- cs_df %>%
  group_by(year) %>%
  summarise_at(vars(cs), list(mean=mean, SE=se)) %>% 
  as.data.frame()  

ggplot() +
  geom_jitter(data=cs_df, aes(x=year, y=cs), size=2, alpha=0.25, colour = "#248fc9") +
  geom_errorbar(data=bluti_cs_summary, aes(x=year, y=mean, ymin=mean-SE, ymax=mean+SE)) +
  geom_point(data=bluti_cs_summary, aes(x=year, y=mean), colour = "black", size = 2, alpha = 0.8) +
  labs(x = "Year", y = "Clutch size", title = "Clutch size raw data and mean+sd") +
  theme_bw() +
  scale_x_continuous(breaks=seq(2014,2024,1)) +
  scale_y_continuous(breaks=seq(0,15,1)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"))) # Unlike with `fed`, average values remain quite close among years, with a few exceptions (e.g. 2015 and 2029).

#...by site:

bluti_cs_summary <- cs_df %>%
  group_by(site) %>%
  summarise_at(vars(cs), list(mean=mean, SE=se)) %>% 
  as.data.frame()  

ggplot(cs_df, aes(x=site, y=cs)) +
  geom_jitter(size=1.5, alpha=0.25, colour = "#248fc9") +
  geom_errorbar(data=bluti_cs_summary, aes(x=site, y=mean, ymin=mean-SE, ymax=mean+SE)) +
  geom_point(data=bluti_cs_summary, aes(x=site, y=mean), colour = "black", size = 2) +
  labs(x = "Sites", y = "Clutch size") +
  theme_bw() +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 90), 
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"))) +
  scale_x_discrete(limits = level_order) +
  scale_y_continuous(breaks=seq(0,15,1))  # However, there's quite a bit of variation between sites (although on average clutch size remains between ~7 and ~10 eggs)

# As with `fed` this suggests that, considering that sites differ in habitat, `cs` varies depending on the habitat and its quality (although it should also be taken into consideration that, according to Ally, birds stay more or less "faithful" to breeding sites, usually returning to the same site or to a nearby site year after year; as a matter of fact, the oldest recorded bird in the transect has always bred in nest box 1 in EDI site).


## Fledgeling success ##

# Histogram of overall fledgeling success
ggplot(blutidf, aes(suc)) +
  geom_histogram(colour = "black", fill = "#248fc9", binwidth=1) +
  labs(x = "Number of chicks fledged successfully", y = "Frequency") +
  theme_bw() +
  scale_x_continuous(breaks=seq(0,13,1)) +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))

# Histogram of verall fledgeling success (up until 3 y.o.)
ggplot(suc_df, aes(suc)) +
  geom_histogram(colour = "black", fill = "#a2deff", binwidth=1) +
  labs(x = "Number of chicks fledged successfully", y = "Frequency") +
  geom_vline(xintercept = mean(suc_df$suc), colour = "maroon", linewidth = 1.2, linetype = 2) +
  theme_bw() +
  scale_x_continuous(breaks=seq(0,13,1)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,200)) +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))  # The `suc` histogram clearly shows a spike in 0s. Sanjana suggested using a Hurdle model to deal with this.

#For fledgeling success, I was also interested in seeing the proportion of chicks that fledged (comparing to the initial clutch size). This proportion will take the complete clutch into account, rather than the number of hatched eggs. However, it would be potentially more accurate to use the number of hatched eggs as base (maybe substracting the number of unhatched eggs found in V1 to the clutch size), but for now I'll use clutch size.

ggplot(suc_df, aes(suc_prop)) +
  geom_histogram(colour = "black", fill = "#248fc9", binwidth=0.1) +
  labs(x = "Proportion of chicks successfully fledged", y = "Frequency") +
  theme_bw() +
  scale_x_continuous(breaks=seq(0,1,0.1)) +
  theme(axis.title = element_text(size = 14,), axis.text = element_text(size = 12))
#ggsave("figures/hist_suc_bluti3yo.png")

qqnorm(suc_df$suc); qqline(suc_df$suc, col = 2)

# Let's look at how this distribution changes when we plot observations both by age and by age at last reproductive attempt:

# Fledgeling success by age (up until 3 y.o.) 
ggplot(suc_df, aes(suc)) +
  geom_histogram(colour = "black", fill = "#a2deff", binwidth=1) +
  labs(x = "Number of chicks fledged successfully", y = "Frequency") +
  theme_bw() +
  facet_wrap(~ yo) +
  scale_x_continuous(breaks=seq(0,13,1)) +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))  # Something quite interesting and that pops up straight aways is how the spike in 0 is smaller as age increases (although overall frequency decreases as age increases, as in `fed` and in `cs`).

# Fledgeling success by age at last reproductive attempt 
ggplot(blutidf_3yo, aes(suc)) +
  geom_histogram(colour = "black", fill = "#248fc9", binwidth=1) +
  labs(x = "Number of chicks fledged successfully", y = "Frequency") +
  theme_bw() +
  facet_wrap(~ w) +
  scale_x_continuous(breaks=seq(0,13,1)) +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12)) #The value 0 seems to be always a bit too frequent (except for the bird that is 7 years old, where the sole value is 8 as the two previous recordings are NAs).

ggplot(suc_df, aes(suc_prop)) +
  geom_histogram(colour = "black", fill = "#248fc9", binwidth=0.1) +
  labs(x = "Proportion of chicks successfully fledged", y = "Frequency", title = "Proportion of successfuly fledged chicks per age (yo) group") +
  theme_bw() +
  facet_wrap(~ yo) +
  scale_x_continuous(breaks=seq(0,1,0.1)) +
  theme(axis.title = element_text(size = 14,), axis.text = element_text(size = 12)) # Interestingly enough, complete success is more frequent in yonger birds.

ggplot(suc_df, aes(suc_prop)) +
  geom_histogram(colour = "black", fill = "#248fc9", binwidth=0.1) +
  labs(x = "Proportion of chicks successfully fledged", y = "Frequency", title = "Proportion of successfuly fledged chicks per age-at-last-reproduction (w) group") +
  theme_bw() +
  facet_wrap(~ w) +
  scale_x_continuous(breaks=seq(0,1,0.1)) +
  theme(axis.title = element_text(size = 14,), axis.text = element_text(size = 12, angle = 30))

suc_df %>% group_by(yo) %>% summarise_at(vars(suc), list(mean=mean, sd=sd, SE=se, min=min, max=max)) %>% as.data.frame()

# When we look at the mean fledgeling success per age group, we can see that it increases ~8% from age 1 to age 2 but it then decreases from age 2 to age 3 (it decreases ~2%), although (once again) the SE is greater. The maximum number of successfully fledged chicks also decreases (which is somewhat expected given that we previously saw that the maximum clutch size is at ages 1 and 2).

# Looking now at raw data by...

# ...year:

bluti_suc_summary <- suc_df %>%
  group_by(year) %>%
  summarise_at(vars(suc), list(mean=mean, SE=se)) %>% 
  as.data.frame()  

ggplot() +
  geom_jitter(data=suc_df, aes(x=year, y=suc), size=2, alpha=0.25, colour = "#248fc9") +
  geom_errorbar(data=bluti_suc_summary, aes(x=year, y=mean, ymin=mean-SE, ymax=mean+SE)) +
  geom_point(data=bluti_suc_summary, aes(x=year, y=mean), colour = "black", size = 2, alpha = 0.8) +
  labs(x = "Year", y = "Fledgeling success", title = "Fledgeling success raw data and mean+sd") +
  theme_bw() +
  scale_x_continuous(breaks=seq(2014,2024,1)) +
  scale_y_continuous(breaks=seq(0,15,1)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))  # There's some variation between years, and what catches my eye the most is the extremely low fledgeling success in year 2024. However, this is expected as we know that last year was a particularly bad year. According to this, both 2015 and 2020 might have also been bad years, while it seems that 2014 has been the best year so far.

#...site:

bluti_suc_summary <- suc_df %>%
  group_by(site) %>%
  summarise_at(vars(suc), list(mean=mean, SE=se)) %>% 
  as.data.frame()  

ggplot(suc_df, aes(x=site, y=suc)) +
  geom_jitter(size=1.5, alpha=0.25, colour = "#248fc9") +
  geom_errorbar(data=bluti_suc_summary, aes(x=site, y=mean, ymin=mean-SE, ymax=mean+SE)) +
  geom_point(data=bluti_suc_summary, aes(x=site, y=mean), colour = "black", size = 2) +
  labs(x = "Sites", y = "Number of chicks successfully fledged") +
  theme_bw() +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 90), 
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"))) +
  scale_x_discrete(limits = level_order) +
  scale_y_continuous(breaks=seq(0,15,1))  # On average, there appears to be some variation, with sites in which fledgeling success is quite low on average (3-4 chicks) and sites in which fledgeling success is okay-ish (with not one site reaching 8 successfully fledged chicks on average)

### Blue tit phenology ###

# Average start of the breeding season

blutiphen$n1 <- as.numeric(blutiphen$n1)

mean(na.omit(blutiphen$n1))  # The breeding season starts on average on day 102 (~ April 2nd)

n1_df <- blutiphen[-which(is.na(blutiphen$n1)),]

n1_df %>%
  group_by(year) %>%
  summarise_at(vars(n1), list(mean=mean, min=min, max=max)) %>%
  as.data.frame()  # earliest start (on average) is on day 102 (April 12th)

# Average end of the breeding season

blutiphen$dfl <- as.numeric(blutiphen$dfl)

mean(na.omit(blutiphen$dfl))  # The breeding season ends on average on day 163 (~ June 12th)

dfl_df <- blutiphen[-which(is.na(blutiphen$dfl)),]

dfl_df %>%
  group_by(year) %>%
  summarise_at(vars(dfl), list(mean=mean, min=min, max=max)) %>%
  as.data.frame()

# Average duration of nest construction

blutiphen$nl <- as.numeric(blutiphen$nl)

mean(na.omit(blutiphen$nl - blutiphen$n1))  # This is on average how long it takes for female birds to finish their nest assuming that they finish the same day they lay the first egg and including pauses within that period of time.

# Pause between nest completion and egg laying

min(na.omit(blutiphen$cc - blutiphen$nl))

blutiphen$fed <- as.numeric(blutiphen$fed)

mean(na.omit(blutiphen$fed - blutiphen$nl))

max(na.omit(blutiphen$cc - blutiphen$nl))

# Mean first egg lay date

mean(fed_df$fed)


# Checking for correlation between environmental variables

library(ggstatsplot)

remotes::install_github("easystats/correlation")

# First, I will do a very rude all vs. all correlation:

env <- environment[,3:21]

corrplot1 <- ggstatsplot::ggcorrmat(
  data = env,
  type = "parametric", # parametric for Pearson, nonparametric for Spearman's correlation
  colors = c("darkred", "white", "steelblue") # change default colors
)


# Now, I would like to see how different oak-related variables correlate with each other:

oak_vars <- environment[,c(10, 11, 14, 20)]

ggstatsplot::ggcorrmat(
  data = oak_vars,
  type = "parametric", # parametric for Pearson, nonparametric for Spearman's correlation
  colors = c("darkred", "white", "steelblue") # change default colors
)  # Everything is highly positively correlated and significant, which I guess should be expected but it is still somewhat surprising to me (how much new information does oak foliage score provides in comparison to the other oak-related predictors?)

# Now, I would like to check how other trees (i.e. willow, birch and sycamore) variables correlate with each other

trees_bars <- environment[,c(12,13,19,21)]

ggstatsplot::ggcorrmat(
  data = trees_bars,
  type = "parametric", # parametric for Pearson, nonparametric for Spearman's correlation
  colors = c("darkred", "white", "steelblue") # change default colors
)  # Here correlations are still significant but not as strong (particularly, the correlations between the number of oaks, birches and sycamores combined and any kind of combined FS are not very strong: 0.37 and 0.49. I would like to interpret this like the foliage score is giving a bit more information, maybe)

# Once this is done, I will do correlations only with overall FS, individual FS and other variables:

predictors <- environment[,c(4,5,6,9,14,15,16,17,18,8)]

ggstatsplot::ggcorrmat(
  data = predictors,
  type = "parametric", # parametric for Pearson, nonparametric for Spearman's correlation
  colors = c("darkred", "white", "steelblue") # change default colors
)

# I'll go through all significant correlations:

### a) Latitude and site occupation are negatively correlated: as latitude increases, so does occupation (bearing in mind that the increase in latitude from the southernmost site to the northernmost site is 2º latitude). I'm not entirely sure about why.
#### a.1) Although it's non-significant, sycamore FS decreases as latitude increases, while birch and oak FS appear to increase (but not only is it non-significant, but also the correlation is quite weak).
#### a.2) I also think that it's interesting to note that correlation between latitude and elevation is 0 (which, having seen elevation vs. altitude, makes sense); they provide completely different information (I think)
### b) Elevation is negatively correlated with site occupation as well, but also with sycamore FS and overall FS.
### c) Site occupation is greater when sycamore foliage score is greater. Not entirely sure what to infer from this.
### d) Tree diversity is negatively correlated with birch FS. Although I'm not entirely sure how to interpret this, maybe I'd say that in sites with lower tree diversity, birch foliage happens to be greater (maybe there are more birches in number or, even if there are not that many birch trees, they're quite wide and provide quite a bit of foliage cover)
### e) Out of all FS predictors, only sycamore FS is positively correlated with total FS: seems like sycamore trees have greater foliage overall than any of the other species selected for the correlation (this is also expected as it's the most frequent tree taxa along the transect, as well as sycamores along the transect tend to offer quite a bit of foliage cover)



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
