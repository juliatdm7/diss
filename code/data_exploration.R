
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

blutidf <- read.csv("C:/Users/julia/OneDrive - University of Edinburgh/EEB_MSc_UEd/Dissertation/diss/data/blutidf_2025.csv")
blutidf_3yo <- read.csv("C:/Users/julia/OneDrive - University of Edinburgh/EEB_MSc_UEd/Dissertation/diss/data/blutidf_3yo_2025.csv")
sites <- read.csv("C:/Users/julia/OneDrive - University of Edinburgh/EEB_MSc_UEd/Dissertation/diss/data/site_detailsII.csv")
habitat <- read.csv("C:/Users/julia/OneDrive - University of Edinburgh/EEB_MSc_UEd/Dissertation/diss/data/Habitats.csv")  
habitat_foliage_scores <- read.csv("C:/Users/julia/OneDrive - University of Edinburgh/EEB_MSc_UEd/Dissertation/diss/data/Habitat_SiteII.csv")  
environment <- read.csv("data/environment_2025.csv")

caterpillars <- read.csv("data/Lepidoptera_Abundance.csv")


### Raw data and mean+se values for each breeding trait without capping observations based on age ###

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  datac <- plyr::rename(datac, c("mean" = measurevar))
  datac$se <- datac$sd / sqrt(datac$N)
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

## AGE ##

# First egg lay date

fed_df <- blutidf[which(!is.na(blutidf$fed)),]

nrow(fed_df)

length(unique(fed_df$ring))

#fed_summary <- fed_df %>%
#  group_by(yo) %>%
#  summarise_at(vars(fed), list(mean=mean, SE=summarySE)) %>% 
#  as.data.frame()

fed_summary <- summarySE(fed_df, measurevar='fed', groupvars='yo')

ggplot() +
  geom_jitter(data=fed_df, aes(x=yo, y=fed), size=2, alpha=0.25, colour = "#248fc9") +
  geom_errorbar(data=fed_summary, aes(x=yo, y=fed, ymin=fed-se, ymax=fed+se)) +
  geom_point(data=fed_summary, aes(x=yo, y=fed), colour = "black", size = 2, alpha = 0.8) +
  labs(x = "Age", y = "First egg lay date (1st Jan = 1)") +
  theme_bw() +
  scale_y_continuous(breaks=seq(90,200,5)) +
  scale_x_continuous(breaks=seq(1,7,1)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))
#ggsave("figures_2025/fed_alldataset_I.png")

# Clutch size

cs_df <- blutidf[which(!is.na(blutidf$cs)),]

nrow(cs_df)

length(unique(cs_df$ring))

#cs_summary <- cs_df %>%
#  group_by(yo) %>%
#  summarise_at(vars(cs), list(mean=mean, SE=se)) %>% 
#  as.data.frame()

cs_summary <- summarySE(cs_df, measurevar='cs', groupvars='yo')

ggplot() +
  geom_jitter(data=cs_df, aes(x=yo, y=cs), size=2, alpha=0.25, colour = "#248fc9") +
  geom_errorbar(data=cs_summary, aes(x=yo, y=cs, ymin=cs-se, ymax=cs+se)) +
  geom_point(data=cs_summary, aes(x=yo, y=cs), colour = "black", size = 2, alpha = 0.8) +
  labs(x = "Age", y = "Clutch size") +
  theme_bw() +
  scale_y_continuous(breaks=seq(1,15,2)) +
  scale_x_continuous(breaks=seq(1,7,1)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

#ggsave("figures_2025/cs_alldataset_I.png")

# Fledgeling success

suc_df <- blutidf[which(!is.na(blutidf$suc)),]

nrow(suc_df)

length(unique(suc_df$ring))

suc_summary <- summarySE(suc_df, measurevar='suc', groupvars='yo')

#suc_summary <- suc_df %>%
#  group_by(yo) %>%
#  summarise_at(vars(suc), list(mean=mean, SE=se)) %>% 
#  as.data.frame()

ggplot() +
  geom_jitter(data=suc_df, aes(x=yo, y=suc), size=2, alpha=0.25, colour = "#248fc9") +
  geom_errorbar(data=suc_summary, aes(x=yo, y=suc, ymin=suc-se, ymax=suc+se)) +
  geom_point(data=suc_summary, aes(x=yo, y=suc), colour = "black", size = 2, alpha = 0.8) +
  labs(x = "Age", y = "Number of successfully fledged nestlings") +
  theme_bw() +
  scale_y_continuous(breaks=seq(0,15,2)) +
  scale_x_continuous(breaks=seq(1,7,1)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

#ggsave("figures_2025/suc_alldataset_I.png")


## AGE AT LAST REPRODUCTION ##

# First egg lay date

#fed_summary <- fed_df %>%
#  group_by(w) %>%
#  summarise_at(vars(fed), list(mean=mean, SE=summarySE)) %>% 
#  as.data.frame()

fed_summary <- summarySE(fed_df, measurevar='fed', groupvars='w')

ggplot() +
  geom_jitter(data=fed_df, aes(x=w, y=fed), size=2, alpha=0.25, colour = "#248fc9") +
  geom_errorbar(data=fed_summary, aes(x=w, y=fed, ymin=fed-se, ymax=fed+se)) +
  geom_point(data=fed_summary, aes(x=w, y=fed), colour = "black", size = 2, alpha = 0.8) +
  labs(x = "Age at last reproduction", y = "First egg lay date (1st Jan = 1)") +
  theme_bw() +
  scale_y_continuous(breaks=seq(90,200,5)) +
  scale_x_continuous(breaks=seq(1,7,1)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))
#ggsave("figures_2025/fed_alldataset_II.png")

# Clutch size

#cs_summary <- cs_df %>%
#  group_by(w) %>%
#  summarise_at(vars(cs), list(mean=mean, SE=se)) %>% 
#  as.data.frame()

cs_summary <- summarySE(cs_df, measurevar='cs', groupvars='w')

ggplot() +
  geom_jitter(data=cs_df, aes(x=w, y=cs), size=2, alpha=0.25, colour = "#248fc9") +
  geom_errorbar(data=cs_summary, aes(x=w, y=cs, ymin=cs-se, ymax=cs+se)) +
  geom_point(data=cs_summary, aes(x=w, y=cs), colour = "black", size = 2, alpha = 0.8) +
  labs(x = "Age at last reproduction", y = "Clutch size") +
  theme_bw() +
  scale_y_continuous(breaks=seq(1,15,2)) +
  scale_x_continuous(breaks=seq(1,7,1)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

#ggsave("figures_2025/cs_alldataset_II.png")

# Fledgeling success

suc_summary <- summarySE(suc_df, measurevar='suc', groupvars='w')

#suc_summary <- suc_df %>%
#  group_by(w) %>%
#  summarise_at(vars(suc), list(mean=mean, SE=se)) %>% 
#  as.data.frame()

ggplot() +
  geom_jitter(data=suc_df, aes(x=w, y=suc), size=2, alpha=0.25, colour = "#248fc9") +
  geom_errorbar(data=suc_summary, aes(x=w, y=suc, ymin=suc-se, ymax=suc+se)) +
  geom_point(data=suc_summary, aes(x=w, y=suc), colour = "black", size = 2, alpha = 0.8) +
  labs(x = "Age at last reproduction", y = "Number of successfully fledged nestlings") +
  theme_bw() +
  scale_y_continuous(breaks=seq(0,15,2)) +
  scale_x_continuous(breaks=seq(1,7,1)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

#ggsave("figures_2025/suc_alldataset_II.png")



### Sample sizes of each breeding trait by age ### 


bluti_3yo <- read.csv("data/blutidf_3yo_2025.csv")
fed_df <- read.csv("data/fed_df_2025.csv")
cs_df <- read.csv("data/cs_df_2025.csv")
suc_df <- read.csv("data/suc_df_2025.csv")

# 1. First egg lay date

fed_age <- fed_df %>% dplyr::count(yo)

fed_uq_rings <- as.data.frame(unique(fed_df$ring)) %>% dplyr::rename(rings = "unique(fed_df$ring)")

fed_uq_rings$w <- fed_df[match(fed_uq_rings$rings, fed_df$ring), "w"]

fed_w <- fed_uq_rings %>% dplyr::count(w)

fed_age  # here we can see the number of observations per age group

fed_w  # and here we can see the number of observations per age-at-last-reproduction group

fedplot1 <- ggplot(fed_df, aes(x=yo)) +
  geom_freqpoly(colour="#248fc9", binwidth=1, linejoin = "round") +
  geom_point(stat="bin", aes(y=after_stat(count)), binwidth=1, colour="black", size = 2) +  
  geom_label(stat="count", label = fed_age$n, vjust=-0.5) + 
  theme_bw() +
  scale_x_continuous(breaks=seq(1,3,1), limits=c(1,3), expand = c(0.1,0)) +
  scale_y_continuous(limits=c(0,800)) +
  labs(x = "Age", y = "Observations", title = "Lay date (1st Jan = 1)", subtitle = "Recordings per age group") +
  theme( axis.title = element_text(size = 13), 
         axis.text = element_text(size = 11),
         axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
         axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))
#ggsave("figures_2025/fed_age_counts_I.png")

fedplot2 <- ggplot(fed_uq_rings, aes(x=w)) +
  geom_freqpoly(colour="#248fc9", binwidth=1, linejoin = "round") +
  geom_point(stat="bin", aes(y=after_stat(count)), binwidth=1, colour="black", size = 2) + 
  geom_label(stat="count", label = fed_w$n, vjust=-0.5) + 
  theme_bw() +
  scale_x_continuous(breaks=seq(1,8,1), limits=c(1,8), expand = c(0.1,0)) +
  scale_y_continuous(limits=c(0,600)) +
  labs(x = "ALR", y = "Unique females", subtitle = "Female longevity") +
  theme( axis.title = element_text(size = 13), 
         axis.text = element_text(size = 11),
         axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
         axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")),
         plot.margin = margin(0.8,0.2,0.2,0.2, "cm"))
#ggsave("figures_2025/fed_w_counts_I.png")

grid.arrange(fedplot1, fedplot2, ncol=2, nrow = 1)
#ggsave("figures_2025/fed_age_w.png")


# 2. Clutch size

cs_age <- cs_df %>% dplyr::count(yo)

cs_uq_rings <- as.data.frame(unique(cs_df$ring)) %>% dplyr::rename(rings = "unique(cs_df$ring)")

cs_uq_rings$w <- cs_df[match(cs_uq_rings$rings, cs_df$ring), "w"]

cs_w <- cs_uq_rings %>% dplyr::count(w)

cs_age  # here we can see the number of observations per age group

cs_w  # and here we can see the number of observations per age-at-last-reproduction group

csplot1 <- ggplot(cs_df, aes(x=yo)) +
  geom_freqpoly(colour="#248fc9", binwidth=1, linejoin = "round") +
  geom_point(stat="bin", aes(y=after_stat(count)), binwidth=1, colour="black", size = 2) +  
  geom_label(stat="count", label = cs_age$n, vjust=-0.5) + 
  theme_bw() +
  scale_x_continuous(breaks=seq(1,3,1), limits=c(1,3), expand = c(0.1,0)) +
  scale_y_continuous(limits=c(0,800), expand = c(0.2,2)) +
  labs(x = "Age", y = "Observations", title = "Clutch size", subtitle = "Recordings per age group") +
  theme( axis.title = element_text(size = 13), 
         axis.text = element_text(size = 11),
         axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
         axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))
#ggsave("figures_2025/cs_age_counts_I.png")

csplot2 <- ggplot(cs_uq_rings, aes(x=w)) +
  geom_freqpoly(colour="#248fc9", binwidth=1, linejoin = "round") +
  geom_point(stat="bin", aes(y=after_stat(count)), binwidth=1, colour="black", size = 2) + 
  geom_label(stat="count", label = cs_w$n, vjust=-0.5) + 
  theme_bw() +
  scale_x_continuous(breaks=seq(1,8,1), limits=c(1,8), expand = c(0.1,0)) +
  scale_y_continuous(limits=c(0,600)) +
  labs(x = "ALR", y = "Unique females", subtitle = "Female longevity") +
  theme( axis.title = element_text(size = 13), 
         axis.text = element_text(size = 11),
         axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
         axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")),
         plot.margin = margin(0.8,0.2,0.2,0.2, "cm"))
#ggsave("figures_2025/cs_age_w.png")

grid.arrange(csplot1, csplot2, ncol=2, nrow = 1)



# 3. Fledgeling success

suc_age <- suc_df %>% dplyr::count(yo)

suc_uq_rings <- as.data.frame(unique(suc_df$ring)) %>% dplyr::rename(rings = "unique(suc_df$ring)")

suc_uq_rings$w <- suc_df[match(suc_uq_rings$rings, suc_df$ring), "w"]

suc_w <- suc_uq_rings %>% dplyr::count(w)

suc_age  # here we can see the number of observations per age group

suc_w  # and here we can see the number of observations per age-at-last-reproduction group

sucplot1 <- ggplot(suc_df, aes(x=yo)) +
  geom_freqpoly(colour="#248fc9", binwidth=1, linejoin = "round") +
  geom_point(stat="bin", aes(y=after_stat(count)), binwidth=1, colour="black", size = 2) +  
  geom_label(stat="count", label = suc_age$n, vjust=-0.5) + 
  theme_bw() +
  scale_x_continuous(breaks=seq(1,3,1), limits=c(1,3), expand = c(0.1,0)) +
  scale_y_continuous(limits=c(0,800)) +
  labs(x = "Age", y = "Observations", title = "Fledgeling success", subtitle = "Recordings per age group") +
  theme( axis.title = element_text(size = 13), 
         axis.text = element_text(size = 11),
         axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
         axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))
#ggsave("figures_2025/suc_age_counts_I.png")

sucplot2 <- ggplot(suc_uq_rings, aes(x=w)) +
  geom_freqpoly(colour="#248fc9", binwidth=1, linejoin = "round") +
  geom_point(stat="bin", aes(y=after_stat(count)), binwidth=1, colour="black", size = 2) + 
  geom_label(stat="count", label = suc_w$n, vjust=-0.5) + 
  theme_bw() +
  scale_x_continuous(breaks=seq(1,8,1), limits=c(1,8), expand = c(0.1,0)) +
  scale_y_continuous(limits=c(0,600)) +
  labs(x = "ALR", y = "Unique females", subtitle = "Female longevity") +
  theme( axis.title = element_text(size = 13), 
         axis.text = element_text(size = 11),
         axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
         axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")),
         plot.margin = margin(0.8,0.2,0.2,0.2, "cm"))
#ggsave("figures_2025/suc_w_counts_I.png")

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
  scale_y_continuous(expand = c(0, 0), limits=c(0,90)) +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))
#ggsave("figures_2025/hist_fed_bluti3yo.png")  # at first glance, although it looks a bit shifted, first egg lay date appears to be more or less Normally distributed

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

#fed_summary <- fed_df %>%
#  group_by(year) %>%
#  summarise_at(vars(fed), list(mean=mean, SE=se)) %>% 
#  as.data.frame() 

fed_summary <- summarySE(fed_df, measurevar='fed', groupvars='year')

ggplot() +
  geom_jitter(data=fed_df, aes(x=year, y=fed), size=2, alpha=0.25, colour = "#248fc9") +
  geom_errorbar(data=fed_summary, aes(x=year, y=fed, ymin=fed-se, ymax=fed+se)) +
  geom_point(data=fed_summary, aes(x=year, y=fed), colour = "black", size = 2, alpha = 0.8) +
  labs(x = "Year", y = "First egg lay date (1st Jan = 1)") +
  theme_bw() +
  scale_x_continuous(breaks=seq(2014,2025,1)) +
  scale_y_continuous(breaks=seq(90,200,5)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 90),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))  # There's quite a lot of variation from one year to another, and we can clearly see that there are much fewer observations during 2020. This points clearly towards the need to include year random effects in the models.

# By site

#fed_summary <- fed_df %>%
#  group_by(site) %>%
#  summarise_at(vars(fed), list(mean=mean, SE=se)) %>% 
#  as.data.frame() 

fed_summary <- summarySE(fed_df, measurevar='fed', groupvars='site')

ggplot() +
  geom_jitter(data=fed_df, aes(x=site, y=fed), size=2, alpha=0.25, colour = "#248fc9") +
  geom_errorbar(data=fed_summary, aes(x=site, y=fed, ymin=fed-se, ymax=fed+se)) +
  geom_point(data=fed_summary, aes(x=site, y=fed), colour = "black", size = 2, alpha = 0.8) +
  labs(x = "Site", y = "First egg lay date (1st Jan = 1)") +
  theme_bw() +
  scale_x_discrete(limits = level_order) +
  scale_y_continuous(breaks=seq(90,200,5)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 90, size = 10, vjust =0.5),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))  # At first glance, there appears to be sites on which, on average, first egg lay date is earlier and where it appears to be later (also on average): there's quite a bit of site-to-site variation. On average, first egg lay date lies between day 109 and 131 across sites, with some sites having first egg lay date as early as day ~100 and as late as day ~146. To me, this suggest that there is are important differences between sites (as we initially expected), which also implies that site random effects should be included.

# By age

#fed_summary <- fed_df %>%
#  group_by(yo) %>%
#  summarise_at(vars(fed), list(mean=mean, SE=se)) %>% 
#  as.data.frame()

fed_summary <- summarySE(fed_df, measurevar='fed', groupvars='yo')

ggplot() +
  geom_jitter(data=fed_df, aes(x=yo, y=fed), size=2, alpha=0.25, colour = "#248fc9") +
  geom_errorbar(data=fed_summary, aes(x=yo, y=fed, ymin=fed-se, ymax=fed+se)) +
  geom_point(data=fed_summary, aes(x=yo, y=fed), colour = "black", size = 2, alpha = 0.8) +
  labs(x = "Age", y = "First egg lay date (1st Jan = 1)") +
  theme_bw() +
  scale_y_continuous(breaks=seq(90,200,5)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))  # First egg lay date does, indeed, decrease as age increases
#ggsave("figures_2025/fed_3yo_age_I.png")

# By age at last reproductive event

#fed_summary <- fed_df %>%
#  group_by(w) %>%
#  summarise_at(vars(fed), list(mean=mean, SE=se)) %>% 
#  as.data.frame()

fed_summary <- summarySE(fed_df, measurevar='fed', groupvars='w')

ggplot() +
  geom_jitter(data=fed_df, aes(x=w, y=fed), size=2, alpha=0.25, colour = "#248fc9") +
  geom_errorbar(data=fed_summary, aes(x=w, y=fed, ymin=fed-se, ymax=fed+se)) +
  geom_point(data=fed_summary, aes(x=w, y=fed), colour = "black", size = 2, alpha = 0.8) +
  labs(x = "Age at last reproductive event", y = "First egg lay date (1st Jan = 1)") +
  theme_bw() +
  scale_y_continuous(breaks=seq(90,200,5)) +
  scale_x_continuous(breaks=seq(1,8,1)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))  # Here, we can see that between 1 and 3 years of longevity, first egg lay date is earlier. However, after that the trend is more irregular (since we have fewer observations)
#ggsave("figures_2025/fed_3yo_ALR_I.png")

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
#ggsave("figures_2025/hist_cs_bluti3yo.png")

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

cs_summary <- summarySE(cs_df, measurevar='cs', groupvars='year')

ggplot() +
  geom_jitter(data=cs_df, aes(x=year, y=cs), size=2, alpha=0.25, colour = "#248fc9") +
  geom_errorbar(data=cs_summary, aes(x=year, y=cs, ymin=cs-se, ymax=cs+se)) +
  geom_point(data=cs_summary, aes(x=year, y=cs), colour = "black", size = 2, alpha = 0.8) +
  labs(x = "Year", y = "Clutch size") +
  theme_bw() +
  scale_x_continuous(breaks=seq(2014,2025,1)) +
  scale_y_continuous(breaks=seq(0,15,1)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 90), 
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"))) # Unlike with `fed`, average values remain quite close among years, with a few exceptions (e.g. 2015 and 2029).

#...by site:

bluti_cs_summary <- cs_df %>%
  group_by(site) %>%
  summarise_at(vars(cs), list(mean=mean, SE=se)) %>% 
  as.data.frame()  

cs_summary <- summarySE(cs_df, measurevar='cs', groupvars='site')

ggplot(cs_df, aes(x=site, y=cs)) +
  geom_jitter(size=1.5, alpha=0.25, colour = "#248fc9") +
  geom_errorbar(data=cs_summary, aes(x=site, y=cs, ymin=cs-se, ymax=cs+se)) +
  geom_point(data=cs_summary, aes(x=site, y=cs), colour = "black", size = 2) +
  labs(x = "Sites", y = "Clutch size") +
  theme_bw() +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 90, size = 11, vjust = 0.5), 
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"))) +
  scale_x_discrete(limits = level_order) +
  scale_y_continuous(breaks=seq(0,15,1))  # However, there's quite a bit of variation between sites (although on average clutch size remains between ~7 and ~10 eggs)

# As with `fed` this suggests that, considering that sites differ in habitat, `cs` varies depending on the habitat and its quality (although it should also be taken into consideration that, according to Ally, birds stay more or less "faithful" to breeding sites, usually returning to the same site or to a nearby site year after year; as a matter of fact, the oldest recorded bird in the transect has always bred in nest box 1 in EDI site).

# By age

#cs_summary <- cs_df %>%
#  group_by(yo) %>%
#  summarise_at(vars(cs), list(mean=mean, SE=se)) %>% 
#  as.data.frame()

cs_summary <- summarySE(cs_df, measurevar='cs', groupvars='yo')

ggplot() +
  geom_jitter(data=cs_df, aes(x=yo, y=cs), size=2, alpha=0.25, colour = "#248fc9") +
  geom_errorbar(data=cs_summary, aes(x=yo, y=cs, ymin=cs-se, ymax=cs+se)) +
  geom_point(data=cs_summary, aes(x=yo, y=cs), colour = "black", size = 2, alpha = 0.8) +
  labs(x = "Age", y = "Clutch size") +
  theme_bw() +
  scale_y_continuous(breaks=seq(1,15,2)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))  # Clutch size does, indeed, increase as age increases
#ggsave("figures_2025/cs_3yo_age_I.png")

# By age at last reproductive event

#cs_summary <- cs_df %>%
#  group_by(w) %>%
#  summarise_at(vars(cs), list(mean=mean, SE=se)) %>% 
#  as.data.frame()

cs_summary <- summarySE(cs_df, measurevar='cs', groupvars='w')

ggplot() +
  geom_jitter(data=cs_df, aes(x=w, y=cs), size=2, alpha=0.25, colour = "#248fc9") +
  geom_errorbar(data=cs_summary, aes(x=w, y=cs, ymin=cs-se, ymax=cs+se)) +
  geom_point(data=cs_summary, aes(x=w, y=cs), colour = "black", size = 2, alpha = 0.8) +
  labs(x = "Age at last reproductive event", y = "Clutch size") +
  theme_bw() +
  scale_y_continuous(breaks=seq(1,15,2)) +
  scale_x_continuous(breaks=seq(1,8,1)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))  # Here, we can see that between 1 and 3 years of longevity, first egg lay date is earlier. However, after that the trend is more irregular (since we have fewer observations)
#ggsave("figures_2025/cs_3yo_ALR_I.png")

## Fledgeling success ##

# Histogram of overall fledgeling success
ggplot(blutidf, aes(suc)) +
  geom_histogram(colour = "black", fill = "#248fc9", binwidth=1) +
  labs(x = "Number of chicks fledged successfully", y = "Frequency") +
  theme_bw() +
  scale_x_continuous(breaks=seq(0,13,1)) +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))

# Histogram of overall fledgeling success (up until 3 y.o.)
ggplot(suc_df, aes(suc)) +
  geom_histogram(colour = "black", fill = "#248fc9", binwidth=1) +
  labs(x = "Number of chicks fledged successfully", y = "Frequency") +
  geom_vline(xintercept = mean(suc_df$suc), colour = "maroon", linewidth = 1.2, linetype = 2) +
  theme_bw() +
  scale_x_continuous(breaks=seq(0,13,1)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,200)) +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))  # The `suc` histogram clearly shows a spike in 0s. Sanjana suggested using a Hurdle model to deal with this.
#ggsave("figures_2025/hist_suc_bluti3yo.png")

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
  geom_histogram(colour = "black", fill = "#248fc9", binwidth=1) +
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

suc_summary <- summarySE(suc_df, measurevar='suc', groupvars='year')

ggplot() +
  geom_jitter(data=suc_df, aes(x=year, y=suc), size=2, alpha=0.25, colour = "#248fc9") +
  geom_errorbar(data=suc_summary, aes(x=year, y=suc, ymin=suc-se, ymax=suc+se)) +
  geom_point(data=suc_summary, aes(x=year, y=suc), colour = "black", size = 2, alpha = 0.8) +
  labs(x = "Year", y = "Number of fledgelings") +
  theme_bw() +
  scale_x_continuous(breaks=seq(2014,2025,1)) +
  scale_y_continuous(breaks=seq(0,15,1)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(size = 11, angle = 90),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))  # There's some variation between years, and what catches my eye the most is the extremely low fledgeling success in year 2024. However, this is expected as we know that last year was a particularly bad year. According to this, both 2015 and 2020 might have also been bad years, while it seems that 2014 has been the best year so far.

#...site:

bluti_suc_summary <- suc_df %>%
  group_by(site) %>%
  summarise_at(vars(suc), list(mean=mean, SE=se)) %>% 
  as.data.frame()

suc_summary <- summarySE(suc_df, measurevar='suc', groupvars='site')

ggplot(suc_df, aes(x=site, y=suc)) +
  geom_jitter(size=1.5, alpha=0.25, colour = "#248fc9") +
  geom_errorbar(data=suc_summary, aes(x=site, y=suc, ymin=suc-se, ymax=suc+se)) +
  geom_point(data=suc_summary, aes(x=site, y=suc), colour = "black", size = 2) +
  labs(x = "Sites", y = "Number of fledgelings") +
  theme_bw() +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 90, size = 11, vjust = 0.5),  
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"))) +
  scale_x_discrete(limits = level_order) +
  scale_y_continuous(breaks=seq(0,15,1))  # On average, there appears to be some variation, with sites in which fledgeling success is quite low on average (3-4 chicks) and sites in which fledgeling success is okay-ish (with not one site reaching 8 successfully fledged chicks on average)

# By age

#suc_summary <- suc_df %>%
#  group_by(yo) %>%
#  summarise_at(vars(suc), list(mean=mean, SE=se)) %>% 
#  as.data.frame()

suc_summary <- summarySE(suc_df, measurevar='suc', groupvars='yo')

ggplot() +
  geom_jitter(data=suc_df, aes(x=yo, y=suc), size=2, alpha=0.25, colour = "#248fc9") +
  geom_errorbar(data=suc_summary, aes(x=yo, y=suc, ymin=suc-se, ymax=suc+se)) +
  geom_point(data=suc_summary, aes(x=yo, y=suc), colour = "black", size = 2, alpha = 0.8) +
  labs(x = "Age", y = "Number of successfully fledged chicks") +
  theme_bw() +
  scale_y_continuous(breaks=seq(1,15,2)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))  # Clutch size does, indeed, increase as age increases
#ggsave("figures_2025/suc_3yo_age_I.png")

# By age at last reproductive event

#suc_summary <- suc_df %>%
#  group_by(w) %>%
#  summarise_at(vars(suc), list(mean=mean, SE=se)) %>% 
#  as.data.frame()

suc_summary <- summarySE(suc_df, measurevar='suc', groupvars='w')

ggplot() +
  geom_jitter(data=suc_df, aes(x=w, y=suc), size=2, alpha=0.25, colour = "#248fc9") +
  geom_errorbar(data=suc_summary, aes(x=w, y=suc, ymin=suc-se, ymax=suc+se)) +
  geom_point(data=suc_summary, aes(x=w, y=suc), colour = "black", size = 2, alpha = 0.8) +
  labs(x = "Age at last reproductive event", y = "Number of successfully fledged chicks") +
  theme_bw() +
  scale_y_continuous(breaks=seq(1,15,2)) +
  scale_x_continuous(breaks=seq(1,7,1)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))  # Here, we can see that between 1 and 3 years of longevity, first egg lay date is earlier. However, after that the trend is more irregular (since we have fewer observations)
#ggsave("figures_2025/suc_3yo_ALR_I.png")



### Identifying selective appearance ###

age5 <- bluti2 %>% 
  filter(age <= 5) %>%
  group_by(ring) %>%
  count(ring)  # 765 unique female birds were first identified as yearlings

nrow(age5)

age6 <- bluti2 %>% 
  filter(age == 6) %>%
  group_by(ring) %>%
  count(ring)  # 697 unique females were captured as 2+ year olds

only_age6 <- anti_join(x = age6, y = age5, by = "ring")

nrow(only_age6)

known_age <- blutidf_3yo[which(!is.na(blutidf_3yo$ringedyear)),]

birdies <- 0
for (i in 1:nrow(birds_at_only_6)) {
  if (only_age6[i,"ring"] %in% known_age$ring)
    birdies <- birdies + 1
}

birdies # we know for certain the age of 20 birds first identified at age category 6

for (i in 1:nrow(birds_at_only_6)) {
  if (only_age6[i,"ring"] %in% known_age$ring) {
    only_age6 <- only_age6[-i,]
  }
}  

# For each separate dataset:

birds_at_5_fed <- fed_df %>%
  filter(age <= 5) %>%
  select(ring) %>%
  distinct()

birds_at_6_with_5_fed <- fed_df %>%
  filter(age == 6) %>%
  semi_join(birds_at_5_fed, by = "ring") %>%
  count(ring) 

birds_at_only_6_fed <- fed_df %>%
  filter(age == 6) %>%
  anti_join(birds_at_5_fed, by = "ring") %>%
  count(ring)

for (i in 1:nrow(birds_at_only_6_fed)) {
  if (birds_at_only_6_fed[i,"ring"] %in% known_age$ring) {
    birds_at_only_6_fed <- birds_at_only_6_fed[-i,]
  }
}

nrow(birds_at_only_6_fed)  # 401 birds of unknown age (could be 2 years old or above) out of 1006 (~40%)


# Clutch size

birds_at_5_cs <- cs_df %>%
  filter(age <= 5) %>%
  select(ring) %>%
  distinct()

birds_at_6_with_5_cs <- cs_df %>%
  filter(age == 6) %>%
  semi_join(birds_at_5_cs, by = "ring") %>%
  count(ring) 

birds_at_only_6_cs <- cs_df %>%
  filter(age == 6) %>%
  anti_join(birds_at_5_cs, by = "ring") %>%
  count(ring)

for (i in 1:nrow(birds_at_only_6_cs)) {
  if (birds_at_only_6_cs[i,"ring"] %in% known_age$ring) {
    birds_at_only_6_cs <- birds_at_only_6_cs[-i,]
  }
}

nrow(birds_at_only_6_cs)  # 440 birds of unknown age (could be 2 years old or above) out of 1188 (~37%)

# Fledgeling success

birds_at_5_suc <- suc_df %>%
  filter(age <= 5) %>%
  select(ring) %>%
  distinct()  # 656 birds first identified as yearlings 

birds_at_6_with_5_suc <- suc_df %>%
  filter(age == 6) %>%
  semi_join(birds_at_5_suc, by = "ring") %>%
  count(ring) 

birds_at_only_6_suc <- suc_df %>%
  filter(age == 6) %>%
  anti_join(birds_at_5_suc, by = "ring") %>%
  count(ring)

for (i in 1:nrow(birds_at_only_6_suc)) {
  if (birds_at_only_6_suc[i,"ring"] %in% known_age$ring) {
    birds_at_only_6_suc <- birds_at_only_6_suc[-i,]
  }
}

nrow(birds_at_only_6_suc)  # 415 birds of unknown age (could be 2 years old or above) out of 1079 (~38%)


## First egg lay date

fed_red <- fed_df %>% filter(yo <= 2)

fed_rings <- data.frame(ring = unique(fed_red$ring), caught = rep(0, length(unique(fed_red$ring))))

fed_age5 <- fed_red %>% filter(age == 5)

fed_age6 <- fed_red %>% filter(age == 6)

for (i in 1:nrow(fed_rings)) {
  if (!(fed_rings[i,"ring"] %in% fed_age5$ring) & (fed_rings[i,"ring"] %in% fed_age6$ring)) {
    fed_rings[i,"caught"] <- "only 6"
  } else if ((fed_rings[i,"ring"] %in% fed_age5$ring) & !(fed_rings[i,"ring"] %in% fed_age6$ring)) {
    fed_rings[i,"caught"] <- "only 5"
  } else if ((fed_rings[i,"ring"] %in% fed_age5$ring) & (fed_rings[i,"ring"] %in% fed_age6$ring)) {
    fed_rings[i,"caught"] <- "5 & 6"
  }
}

fed_rings %>% dplyr::count(caught)  # according to this, and if I'm not mistaken, 125 birds were caught both when they were 1 and 2 y.o. while 454 birds were only caught when they were 1 y.o. (age 5) and 361 were only caught when they were 2 y.o. (age 6)

fed_red$caught <- fed_rings[match(fed_red$ring, fed_rings$ring),"caught"]  # assigning a category to each of the observations in the reduced dataset

fed_red[which(fed_red$caught == "only 5"),"caught"] <- NA

se <- function(x) {
  sd(x)/(sqrt(length(x)))
}

fed_red_summary <- fed_red %>%
  group_by(caught) %>%
  summarise_at(vars(fed), list(mean=mean, SE=se)) %>% 
  as.data.frame()

ggplot() +
  geom_jitter(data=fed_red, aes(x=caught, y=fed), size=2, alpha=0.25, colour = "#248fc9") +
  geom_errorbar(data=fed_red_summary, aes(x=caught, y=mean, ymin=mean-SE, ymax=mean+SE)) +
  geom_point(data=fed_red_summary, aes(x=caught, y=mean), colour = "black", size = 2, alpha = 0.8) +
  labs(x = NULL, y = "First egg lay date (1st Jan = 1)", title = "Differences in fed between birds caught once or twice") +
  theme_bw() +
  scale_y_continuous(breaks=seq(110,135,5), limits = c(110, 135)) +  # I chopped some observations to be able to see better the SE
  scale_x_discrete(limits = c("only 6", "5 & 6")) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

# At first glance, mean +- SE overlaps for birds caught only at age 6 (2 y.o.) and birds caught at both age 5 and 6 (1 & 2 y.o.)


# Clutch size

cs_red <- cs_df %>% filter(yo <= 2)

cs_rings <- data.frame(ring = unique(cs_red$ring), caught = rep(0, length(unique(cs_red$ring))))

cs_age5 <- cs_red %>% filter(age == 5)

cs_age6 <- cs_red %>% filter(age == 6)

for (i in 1:nrow(cs_rings)) {
  if (!(cs_rings[i,"ring"] %in% cs_age5$ring) & (cs_rings[i,"ring"] %in% cs_age6$ring)) {
    cs_rings[i,"caught"] <- "only 6"
  } else if ((cs_rings[i,"ring"] %in% cs_age5$ring) & !(cs_rings[i,"ring"] %in% cs_age6$ring)) {
    cs_rings[i,"caught"] <- "only 5"
  } else {
    cs_rings[i,"caught"] <- "5 & 6"
  }
}

cs_rings %>% dplyr::count(caught)  # according to this, and for clutch size, 185 birds were caught both when they were 1 and 2 y.o. while 517 birds were only caught when they were 1 y.o. (age 5) and 396 were only caught when they were 2 y.o. (age 6). Unlike with fed, this might be a problem

cs_red$caught <- cs_rings[match(cs_red$ring, cs_rings$ring),"caught"]  # assigning a category to each of the observations in the reduced dataset

cs_red[which(cs_red$caught == "only 5"),"caught"] <- NA

se <- function(x) {
  sd(x)/(sqrt(length(x)))
}

cs_red_summary <- cs_red %>%
  group_by(caught) %>%
  summarise_at(vars(cs), list(mean=mean, SE=se)) %>% 
  as.data.frame()

ggplot() +
  geom_jitter(data=cs_red, aes(x=caught, y=cs), size=2, alpha=0.25, colour = "#248fc9") +
  geom_errorbar(data=cs_red_summary, aes(x=caught, y=mean, ymin=mean-SE, ymax=mean+SE)) +
  geom_point(data=cs_red_summary, aes(x=caught, y=mean), colour = "black", size = 2, alpha = 0.8) +
  labs(x = NULL, y = "Clutch size", title = "Differences in cs between birds caught once or twice") +
  theme_bw() +
  scale_y_continuous(breaks=seq(0,15,2)) +  
  scale_x_discrete(limits = c("only 6", "5 & 6")) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

# At first glance, clutch size appears to be slightly higher (on average) for birds caught at both 1 y.o. and 2 y.o., but it might not be significant

# Fledgeling success

suc_red <- suc_df %>% filter(yo <= 2)

suc_rings <- data.frame(ring = unique(suc_red$ring), caught = rep(0, length(unique(suc_red$ring))))

suc_age5 <- suc_red %>% filter(age == 5)

suc_age6 <- suc_red %>% filter(age == 6)

for (i in 1:nrow(suc_rings)) {
  if (!(suc_rings[i,"ring"] %in% suc_age5$ring) & (suc_rings[i,"ring"] %in% suc_age6$ring)) {
    suc_rings[i,"caught"] <- "only 6"
  } else if ((suc_rings[i,"ring"] %in% suc_age5$ring) & !(suc_rings[i,"ring"] %in% suc_age6$ring)) {
    suc_rings[i,"caught"] <- "only 5"
  } else {
    suc_rings[i,"caught"] <- "5 & 6"
  }
}

suc_rings %>% count(caught)  # according to this, and for clutch size, 185 birds were caught both when they were 1 and 2 y.o. while 517 birds were only caught when they were 1 y.o. (age 5) and 396 were only caught when they were 2 y.o. (age 6). Unlike with fed, this might be a problem

suc_red$caught <- suc_rings[match(suc_red$ring, suc_rings$ring),"caught"]  # assigning a category to each of the observations in the reduced dataset

suc_red[which(suc_red$caught == "only 5"),"caught"] <- NA

se <- function(x) {
  sd(x)/(sqrt(length(x)))
}

suc_red_summary <- suc_red %>%
  group_by(caught) %>%
  summarise_at(vars(suc), list(mean=mean, SE=se)) %>% 
  as.data.frame()

ggplot() +
  geom_jitter(data=suc_red, aes(x=caught, y=suc), size=2, alpha=0.25, colour = "#248fc9") +
  geom_errorbar(data=suc_red_summary, aes(x=caught, y=mean, ymin=mean-SE, ymax=mean+SE)) +
  geom_point(data=suc_red_summary, aes(x=caught, y=mean), colour = "black", size = 2, alpha = 0.8) +
  labs(x = NULL, y = "Fledgeling success", title = "Differences in suc between birds caught once or twice") +
  theme_bw() +
  scale_y_continuous(breaks=seq(0,15,2)) +  
  scale_x_discrete(limits = c("only 6", "5 & 6")) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

# At first glance, it looks like there might be a significant difference in fledgeling success between individuals caught only at 2 y.o. vs. individuals caught at both 1 and 2 y.o.


### Breeding traits vs. environmental predictors ###

suc_df$suc <- as.numeric(suc_df$suc)

## Total foliage score ##

# First egg lay date

fed_df$total_FS <- habitat_foliage_scores[match(fed_df$site, habitat_foliage_scores$Site),"Total"]

fedplot1 <- ggplot(fed_df, aes(x=total_FS, y=fed, col=as.factor(yo))) +
  geom_point(alpha=0.75) +
  theme_bw() +
  labs(x = "Total foliage score", y = "First egg lay date (1 = 1st Jan)") +
  scale_y_continuous(breaks=seq(95,150,5)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(margin = unit(c(1, 0, 0, 0), "mm")), 
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

# Clutch size

cs_df$total_FS <- habitat_foliage_scores[match(cs_df$site, habitat_foliage_scores$Site),"Total"]

csplot1 <- ggplot(cs_df, aes(x=total_FS, y=cs, col=as.factor(yo))) +
  geom_point(alpha=0.75) +
  theme_bw() +
  labs(x = "Total foliage score", y = "Clutch size") +
  scale_y_continuous(breaks=seq(1,15,2)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(margin = unit(c(1, 0, 0, 0), "mm")), 
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

# Fledgeling success

suc_df$total_FS <- habitat_foliage_scores[match(suc_df$site, habitat_foliage_scores$Site),"Total"]

sucplot1 <- ggplot(suc_df, aes(x=total_FS, y=suc, col=as.factor(yo))) +
  geom_point(alpha=0.75) +
  theme_bw() +
  labs(x = "Total foliage score", y = "Number of successfully fledged chicks") +
  scale_y_continuous(breaks=seq(0,15,2)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(margin = unit(c(1, 0, 0, 0), "mm")), 
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

## Proportion of oak and birch foliage score ##

# First egg lay date

fed_df$oak_birch_FS <- environment[match(fed_df$site, environment$sites),"oak_birch_FS_prop"]

fedplot2 <- ggplot(fed_df, aes(x=oak_birch_FS, y=fed, col=as.factor(yo))) +
  geom_point(alpha=0.75) +
  theme_bw() +
  labs(x = "Proportion of oak and birch foliage score", y = "First egg lay date (1 = 1st Jan)") +
  scale_y_continuous(breaks=seq(95,150,5)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(margin = unit(c(1, 0, 0, 0), "mm")), 
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

# Clutch size

cs_df$oak_birch_FS <- environment[match(cs_df$site, environment$sites),"oak_birch_FS_prop"]

csplot2 <- ggplot(cs_df, aes(x=oak_birch_FS, y=cs, col=as.factor(yo))) +
  geom_point(alpha=0.75) +
  theme_bw() +
  labs(x = "Proportion of oak and birch foliage score", y = "Clutch size") +
  scale_y_continuous(breaks=seq(1,15,2)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(margin = unit(c(1, 0, 0, 0), "mm")), 
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

# Fledgeling success

suc_df$oak_birch_FS <- environment[match(suc_df$site, environment$sites),"oak_birch_FS_prop"]

sucplot2 <- ggplot(suc_df, aes(x=oak_birch_FS, y=suc, col=as.factor(yo))) +
  geom_point(alpha=0.75) +
  theme_bw() +
  labs(x = "Proportion of oak and birch foliage score", y = "Number of successfully fledged chicks") +
  scale_y_continuous(breaks=seq(1,15,2)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(margin = unit(c(1, 0, 0, 0), "mm")), 
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

## Proportion of oak, birch and foliage score ##

# First egg lay date

fed_df$oak_birch_sycamore_FS <- environment[match(fed_df$site, environment$sites),"oak_birch_sycamore_FS_prop"]

fedplot3 <- ggplot(fed_df, aes(x=oak_birch_sycamore_FS, y=fed, col=as.factor(yo))) +
  geom_point(alpha=0.75) +
  theme_bw() +
  labs(x = "Proportion of oak, birch and sycamore foliage score", y = "First egg lay date (1 = 1st Jan)") +
  scale_y_continuous(breaks=seq(95,150,5)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(margin = unit(c(1, 0, 0, 0), "mm")), 
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

# Clutch size

cs_df$oak_birch_sycamore_FS <- environment[match(cs_df$site, environment$sites),"oak_birch_sycamore_FS_prop"]

csplot3 <- ggplot(cs_df, aes(x=oak_birch_sycamore_FS, y=cs, col=as.factor(yo))) +
  geom_point(alpha=0.75) +
  theme_bw() +
  labs(x = "Proportion of oak and birch foliage score", y = "Clutch size") +
  scale_y_continuous(breaks=seq(1,15,2)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(margin = unit(c(1, 0, 0, 0), "mm")), 
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

# Fledgeling success

suc_df$oak_birch_sycamore_FS <- environment[match(suc_df$site, environment$sites),"oak_birch_sycamore_FS_prop"]

sucplot3 <- ggplot(suc_df, aes(x=oak_birch_sycamore_FS, y=suc, col=as.factor(yo))) +
  geom_point(alpha=0.75) +
  theme_bw() +
  labs(x = "Proportion of oak and birch foliage score", y = "Number of successfully fledged chicks") +
  scale_y_continuous(breaks=seq(1,15,2)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(margin = unit(c(1, 0, 0, 0), "mm")), 
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

grid.arrange(fedplot1,fedplot2,fedplot3,csplot1,csplot2,csplot3,sucplot1,sucplot2,sucplot3, ncol=3, nrow=3)


# Checking for correlation between environmental variables

library(ggstatsplot)

#remotes::install_github("easystats/correlation")

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

predictors <- environment[,c(4,5,10,11,17,19,20,21)]

pairs(predictors)

ggstatsplot::ggcorrmat(
  data = predictors,
  type = "nonparametric", # parametric for Pearson, nonparametric for Spearman's correlation
  colors = c("darkred", "white", "steelblue") # change default colors
)

cor.test(environment$tree_diversity_simpson, environment$total_FS, method = "spearman")

cor.test(environment$tree_diversity_simpson, environment$total_FS, method = "spearman")

# I'll go through all significant correlations:

### a) Latitude and site occupation are negatively correlated: as latitude increases, so does occupation (bearing in mind that the increase in latitude from the southernmost site to the northernmost site is 2º latitude). I'm not entirely sure about why.
#### a.1) Although it's non-significant, sycamore FS decreases as latitude increases, while birch and oak FS appear to increase (but not only is it non-significant, but also the correlation is quite weak).
#### a.2) I also think that it's interesting to note that correlation between latitude and elevation is 0 (which, having seen elevation vs. altitude, makes sense); they provide completely different information (I think)
### b) Elevation is negatively correlated with site occupation as well, but also with sycamore FS and overall FS.
### c) Site occupation is greater when sycamore foliage score is greater. Not entirely sure what to infer from this.
### d) Tree diversity is negatively correlated with birch FS. Although I'm not entirely sure how to interpret this, maybe I'd say that in sites with lower tree diversity, birch foliage happens to be greater (maybe there are more birches in number or, even if there are not that many birch trees, they're quite wide and provide quite a bit of foliage cover)
### e) Out of all FS predictors, only sycamore FS is positively correlated with total FS: seems like sycamore trees have greater foliage overall than any of the other species selected for the correlation (this is also expected as it's the most frequent tree taxa along the transect, as well as sycamores along the transect tend to offer quite a bit of foliage cover)


# Checking how correlated overall FS and all deciduous trees FS are:

cor.test(environment$all_decid_FS, environment$total_FS, method = "spearman")

ggplot(environment, aes(x=total_FS, y=all_decid_FS)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x) +
  theme_bw() +
  labs(x = "Total foliage score", y = "Total foliage score of all deciduous taxa") +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(margin = unit(c(1, 0, 0, 0), "mm")), 
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"))) 

# Checking how correlated overall FS and oak, birch and sycamore trees FS are:

cor.test(environment$total_FS, environment$oak_birch_sycamore_FS, method = "spearman")

ggplot(environment, aes(x=total_FS, y=oak_birch_sycamore_FS)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x) +
  theme_bw() +
  labs(x = "Total foliage score", y = "Oak, birch and sycamore foliage score") +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(margin = unit(c(1, 0, 0, 0), "mm")), 
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"))) 


# Checking for correlation between response variables

blutiphen[which(blutiphen$suc < 0),"suc"] <- NA

blutiphen$suc <- as.numeric(blutiphen$suc)

rv <- blutiphen[,c(4,5,6)]

corrplot2 <- ggstatsplot::ggcorrmat(
  data = rv,
  type = "nonparametric", # parametric for Pearson, nonparametric for Spearman's correlation
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

cor.test(age_wide$yo_M, age_wide$yo_F, method = "spearman", exact = F)  

# It appears that, when computing a Pearson correlation test (which I'm not sure it's the best), male age and female age of the same year, site and nest box (age of the female and the male withing the same pair) show a significant positive correlation (cor = 0.2558343, p-value < 2.2e-16)
# However, it's really not a 1 by 1 correlation nor is it particularly strong.

ggplot(age_wide, aes(x=yo_F, y=yo_M)) +
  geom_jitter(alpha = 0.25) +
  geom_smooth(method='lm', formula= y~x, col="red") +
  theme_bw() +
  labs(x = "Age of the female in a pair (y.o.)", y = "Age of the male in a pair (y.o.)") +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(margin = unit(c(1, 0, 0, 0), "mm")), 
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))  +
  scale_x_continuous(breaks = seq(1,7,1)) +
  scale_y_continuous(breaks = seq(1,7,1))
#ggsave("figures_2025/male_female_age_corr.png")

