########################
### PROOF OF CONCEPT ###
########################

library(dplyr)
library(ggplot2)
library(gridExtra)
blutidf <- read.csv("data/blutidf.csv")
blutidf_3yo <- read.csv("data/blutidf_3yo.csv")


#### Visualisation of sample sizes and cases ####



### Number of breeding attempts per female ###
unique_birds <- blutidf %>%
  count(ring)  # unique birds
ggplot(unique_birds, aes(n)) +
  geom_freqpoly(colour="#248fc9", binwidth=1, linejoin = "round") +
  geom_point(stat="bin", aes(y=after_stat(count)), binwidth=1, colour="black", size = 2) +  # I would like to add a label with the frequencies
  theme_bw() +
  scale_x_continuous(breaks=seq(1,7,1), limits = c(1,7)) +
  labs(x = "Number of breeding attempts", y = "Number of females", title = "Number of breeding events attempted by female blue tits") +
  theme( axis.title = element_text(size = 13), 
         axis.text = element_text(size = 11),
         axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
         axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

### Number of recorded breeding attempts per age group ###
# Histogram
ggplot(blutidf, aes(x=yo)) +
  geom_histogram(colour = "black", fill = "#248fc9", binwidth=1) + 
  theme_bw() +
  scale_x_continuous(breaks=seq(1,7,1)) +
  labs(x = "Age", y = "Number of females", title = "Overall number of female recordings per age group") +
  theme( axis.title = element_text(size = 13), 
         axis.text = element_text(size = 11),
         axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
         axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

# Labeled line with points
birds_per_age <- blutidf %>% count(yo)
ggplot(blutidf, aes(x=yo)) +
  geom_freqpoly(colour="#248fc9", binwidth=1, linejoin = "round") +
  geom_point(stat="bin", aes(y=after_stat(count)), binwidth=1, colour="black", size = 2) +  # I would like to add a label with the frequencies
  geom_label(stat="count", label = birds_per_age$n, vjust=-0.5) + 
  theme_bw() +
  scale_x_continuous(breaks=seq(1,7,1), limits=c(1,7)) +
  scale_y_continuous(limits=c(0,820)) +
  labs(x = "Age", y = "Number of females", title = "Overall number of female recordings per age group") +
  theme( axis.title = element_text(size = 13), 
         axis.text = element_text(size = 11),
         axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
         axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

## Estimated age at last reproductive event of each female ##
birds_longev <- w %>% count(w)
ggplot(w, aes(x=w)) +
  geom_freqpoly(colour="#248fc9", binwidth=1, linejoin = "round") +
  geom_point(stat="bin", aes(y=after_stat(count)), binwidth=1, colour="black", size = 2) +  # I would like to add a label with the frequencies
  geom_label(stat="count", label = birds_longev$n, vjust=-0.5) + 
  theme_bw() +
  scale_x_continuous(breaks=seq(1,7,1), limits=c(1,7)) +
  scale_y_continuous(limits=c(0,650)) +
  labs(x = "Age at last recorded reproductive event", y = "Number of females", title = "What is the longevity estimated per female?") +
  theme( axis.title = element_text(size = 13), 
         axis.text = element_text(size = 11),
         axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
         axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))
  

bluti2$suc <- as.numeric(bluti2$suc)

### Distribution of the breeding trait variables ###

# Overall fledgeling success #
ggplot(blutidf, aes(suc)) +
  geom_histogram(colour = "black", fill = "#248fc9", binwidth=1) +
  labs(x = "Number of chicks fledged successfully", y = "Frequency") +
  theme_bw() +
  scale_x_continuous(breaks=seq(0,13,1)) +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))

# Overall fledgeling success (up until 3 y.o.)#
ggplot(blutidf_3yo, aes(suc)) +
  geom_histogram(colour = "black", fill = "#248fc9", binwidth=1) +
  labs(x = "Number of chicks fledged successfully", y = "Frequency") +
  theme_bw() +
  scale_x_continuous(breaks=seq(0,13,1)) +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))

# Fledgeling success by age (up until 3 y.o.) #
ggplot(blutidf_3yo, aes(suc)) +
  geom_histogram(colour = "black", fill = "#248fc9", binwidth=1) +
  labs(x = "Number of chicks fledged successfully", y = "Frequency") +
  theme_bw() +
  facet_wrap(~ yo) +
  scale_x_continuous(breaks=seq(0,13,1)) +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

# Fledgeling success by age at last reproductive attempt #
ggplot(blutidf_3yo, aes(suc)) +
  geom_histogram(colour = "black", fill = "#248fc9", binwidth=1) +
  labs(x = "Number of chicks fledged successfully", y = "Frequency") +
  theme_bw() +
  facet_wrap(~ w) +
  scale_x_continuous(breaks=seq(0,13,1)) +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 
  
# Overall clutch size # 
ggplot(blutidf, aes(cs)) +
  geom_histogram(colour = "black", fill = "#248fc9", binwidth=1) +
  labs(x = "Clutch size", y = "Frequency") +
  theme_bw() +
  scale_x_continuous(breaks=seq(0,15,1)) +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))

# Overall clutch size (up until 3 y.o.) # 
ggplot(blutidf_3yo, aes(cs)) +
  geom_histogram(colour = "black", fill = "#248fc9", binwidth=1) +
  labs(x = "Clutch size", y = "Frequency") +
  theme_bw() +
  scale_x_continuous(breaks=seq(0,15,1)) +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))

# Clutch size by age(up until 3 y.o.) # 
ggplot(blutidf_3yo, aes(cs)) +
  geom_histogram(colour = "black", fill = "#248fc9", binwidth=1) +
  labs(x = "Clutch size", y = "Frequency") +
  theme_bw() +
  facet_wrap(~ w) +
  scale_x_continuous(breaks=seq(0,15,1)) +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))

# Clutch size by age at last reproductive attempt #
ggplot(blutidf_3yo, aes(cs)) +
  geom_histogram(colour = "black", fill = "#248fc9", binwidth=1) +
  labs(x = "Number of chicks fledged successfully", y = "Frequency") +
  theme_bw() +
  facet_wrap(~ w) +
  scale_x_continuous(breaks=seq(0,13,1)) +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

# Overall first egg lay date # 
ggplot(blutidf, aes(fed)) +
  geom_histogram(colour = "black", fill = "#248fc9", binwidth=1) +
  labs(x = "First egg lay date (1st Jan = 1)", y = "Frequency") +
  theme_bw() +
  scale_x_continuous(breaks=seq(90,200,5)) +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))

# Overall first egg lay date (up until 3 y.o.) # 
ggplot(blutidf_3yo, aes(fed)) +
  geom_histogram(colour = "black", fill = "#248fc9", binwidth=1) +
  labs(x = "First egg lay date (1st Jan = 1)", y = "Frequency") +
  theme_bw() +
  scale_x_continuous(breaks=seq(90,200,5)) +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))

# First egg lay date by age(up until 3 y.o.) # 
ggplot(blutidf_3yo, aes(fed)) +
  geom_histogram(colour = "black", fill = "#248fc9", binwidth=1) +
  labs(x = "First egg lay date (1st Jan = 1)", y = "Frequency") +
  theme_bw() +
  facet_wrap(~ yo) +
  scale_x_continuous(breaks=seq(90,200,5)) +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))

# Fledgeling success by age at last reproductive attempt #
ggplot(blutidf_3yo, aes(fed)) +
  geom_histogram(colour = "black", fill = "#248fc9", binwidth=1) +
  labs(x = "Number of chicks fledged successfully", y = "Frequency") +
  theme_bw() +
  facet_wrap(~ w) +
  scale_x_continuous(breaks=seq(0,13,1)) +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 


### Breeding traits per year (raw data) ###

# Suc per year
ggplot(blutidf_3yo, aes(x=year, y=suc)) +
  geom_jitter(size=2, alpha=0.25, colour = "#248fc9") +
  labs(x = "Years", y = "Number of chicks fledged successfully") +
  theme_bw() +
  scale_x_continuous(breaks=seq(2014,2024,1)) +
  scale_y_continuous(breaks=seq(0,16,1), limits = c(0, 16)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))


# Fed per year
ggplot(bluti2, aes(x=year, y=fed)) +
  geom_jitter(size=2, alpha=0.25, colour = "#248fc9") +
  labs(x = "Years", y = "First egg lay date (1st Jan = 1)") +
  theme_bw() +
  scale_x_continuous(breaks=seq(2014,2024,1)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))
#  pay attention to 2020

# Cs per year
ggplot(bluti2, aes(x=year, y=cs)) +
  geom_jitter(size=2, alpha=0.25, colour = "#248fc9") +
  labs(x = "Years", y = "Clutch size") +
  theme_bw() +
  scale_x_continuous(breaks=seq(2014,2024,1)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))



### Raw data per age group ###

# Now, I'll create new dataframes that will include the mean values for each breeding trait for each age group (years old) and their corresponding standard deviation
# I will overlap this metrics to a raw data layer with a bit of noise added using geom_jitter()

## For fledgeling success
bluti_suc_summary <- blutidf_3yo %>%
  group_by(yo) %>%
  na.omit(bluti_suc_summary) %>%
  summarise_at(vars(suc), list(mean=mean, sd=sd)) %>% 
  as.data.frame()
## For lay date of first egg
bluti_fed_summary <- blutidf_3yo %>%
  group_by(yo) %>%
  na.omit(bluti_fed_summary) %>%
  summarise_at(vars(fed), list(mean=mean, sd=sd)) %>% 
  as.data.frame()
## For clutch size
bluti_cs_summary <- blutidf_3yo %>%
  group_by(yo) %>%
  na.omit(bluti_cs_summary) %>%
  summarise_at(vars(cs), list(mean=mean, sd=sd)) %>% 
  as.data.frame()

# Suc per age (noisy raw data plus mean+sd)
grid.arrange(plot1, plot2, ..., ncol=3, nrow = 3)
plot1 <- ggplot() +
  geom_jitter(data=blutidf_3yo, aes(x=yo, y=suc), size=2, alpha=0.25, colour = "#248fc9") +
  geom_errorbar(data=bluti_suc_summary, aes(x=yo, y=mean, ymin=mean-sd, ymax=mean+sd)) +
  geom_point(data=bluti_suc_summary, aes(x=yo, y=mean), colour = "black", size = 3) +
  labs(x = "Bird age", y = "Number of chicks successfully fledged", title = "Number of chicks successfully fledged raw data and mean+sd") +
  theme_bw() +
  scale_x_continuous(breaks=seq(1,3,1)) +
  scale_y_continuous(breaks=seq(1,15,1)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))
#ggsave("figures/rawdata_suc_ggplotI.png")

# Fed per age (noisy raw data plus mean+sd)
plot2 <- ggplot() +
  geom_jitter(data=blutidf_3yo, aes(x=yo, y=fed), size=2, alpha=0.25, colour = "#248fc9") +
  geom_errorbar(data=bluti_fed_summary, aes(x=yo, y=mean, ymin=mean-sd, ymax=mean+sd)) +
  geom_point(data=bluti_fed_summary, aes(x=yo, y=mean), colour = "black", size = 3) +
  labs(x = "Bird age", y = "First egg lay date (1st Jan = 1)", title = "First egg lay date raw data and mean+sd") +
  theme_bw() +
  scale_x_continuous(breaks=seq(1,3,1)) +
  scale_y_continuous(breaks=seq(90,200,5)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))
#ggsave("figures/rawdata_fed_ggplotI.png")

# Cs per age (noisy raw data plus mean+sd)
plot3 <- ggplot() +
  geom_jitter(data=blutidf_3yo, aes(x=yo, y=cs), size=2, alpha=0.25, colour = "#248fc9") +
  geom_errorbar(data=bluti_cs_summary, aes(x=yo, y=mean, ymin=mean-sd, ymax=mean+sd)) +
  geom_point(data=bluti_cs_summary, aes(x=yo, y=mean), colour = "black", size = 3) +
  labs(x = "Bird age", y = "Clutch size", title = "Clutch size raw data and mean+sd") +
  theme_bw() +
  scale_x_continuous(breaks=seq(1,3,1)) +
  scale_y_continuous(breaks=seq(1,15,2)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))
#ggsave("figures/rawdata_cs_ggplotI.png")

grid.arrange(plot1, plot2, plot3, ncol=3, nrow = 1)

### Age-specific patterns of breeding traits ###

# Violin plot for fledgeling success
ggplot(bluti2, aes(x=yo, y=suc, group=yo)) +
  geom_violin(aes(fill=factor(yo), colour = "black"),stat = "ydensity", position = "dodge", colour = "black", scale = "count", adjust=0.7) +
  labs(x = "Age of female parent", y = "Number of chicks successfully fledged") +
  theme_bw() +
  guides(fill=guide_legend(title="Age in years old")) +
  scale_x_continuous(breaks=seq(1,7,1))+
  scale_y_continuous(breaks=seq(1,14,1))

# Violin plot for lay date of first egg
ggplot(bluti2, aes(x=yo, y=fed, group=yo)) +
  geom_violin(aes(fill=factor(yo), colour = "black"),stat = "ydensity", position = "dodge", colour = "black", scale = "count", adjust=0.7) +
  labs(x = "Age of female parent", y = "First egg lay date (ordinal numbers)") +
  theme_bw() +
  guides(fill=guide_legend(title="Age in years old"))

# Violin plot for clutch size
ggplot(bluti2, aes(x=yo, y=cs, group=yo)) +
  geom_violin(aes(fill=factor(yo), colour = "black"),stat = "ydensity", position = "dodge", colour = "black", scale = "count", adjust=0.7) +
  labs(x = "Age of female parent", y = "Clutch size") +
  theme_bw() +
  guides(fill=guide_legend(title="Age in years old")) +
  scale_x_continuous(breaks=seq(1,7,1)) +
  scale_y_continuous(breaks=seq(0,14,1))


# I'll do the same, but with the reduced subdataset (excluding birds identified only at 6 years old)

## For fledgeling success
bluti2_red_suc_summary <- bluti2_red %>%
  group_by(y_old) %>%
  summarise_at(vars(suc), list(mean=mean, sd=sd)) %>% 
  as.data.frame()
## For lay date of first egg
bluti2_red_fed_summary <- bluti2_red %>%
  group_by(y_old) %>%
  na.omit(bluti2_red_fed_summary) %>%
  summarise_at(vars(fed), list(mean=mean, sd=sd)) %>% 
  as.data.frame()
## For clutch size
bluti2_red_cs_summary <- bluti2_red %>%
  group_by(y_old) %>%
  na.omit(bluti2_red_cs_summary) %>%
  summarise_at(vars(cs), list(mean=mean, sd=sd)) %>% 
  as.data.frame()

# Scatter plot of fledgeling success per age group
ggplot(bluti2, aes(x=yo, y=fed, fill=factor(yo))) +
  geom_point(aes(fill=yo, alpha = 0.01)) +
  labs(x = "Age of female parent", y = "First egg lay date (ordinal numbers)") +
  theme_bw() +
  scale_x_continuous(breaks=seq(1,7,1)) 

# Barplot with SD bars for fledgeling success
ggplot(bluti2_suc_summary, aes(x=yo, y=mean, fill = factor(yo))) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd)) +
  geom_col(colour = "black") +
  labs(x = "Age of female parent", y = "Number of chicks successfully fledged") +
  theme_bw() +
  guides(fill="none") +
  scale_x_continuous(breaks=seq(1,7,1))+
  scale_y_continuous(breaks=seq(0,14,1), expand = expansion(mult = c(0, .1)))

# Barplot with SD bars for first egg lay date
ggplot(bluti2_fed_summary, aes(x=yo, y=mean, fill = factor(yo))) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd)) +
  geom_col(colour = "black") +
  labs(x = "Age of female parent", y = "Lay date of first egg (1 = Jan 1st)") +
  theme_bw() +
  guides(fill="none") +
  scale_x_continuous(breaks=seq(1,7,1)) +
  scale_y_continuous(breaks=seq(0,200,10), expand = expansion(mult = c(0, .1)))

# Mean +- SD for first egg lay date (complete dataset)
ggplot(bluti2_fed_summary, aes(x=yo, y=mean, fill = factor(yo))) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd)) +
  geom_point(colour = "black", size = 2) +
  labs(x = "Age of female parent", y = "Lay date of first egg (1 = Jan 1st)", title="Complete dataset") +
  theme_bw() +
  guides(fill="none") +
  scale_x_continuous(breaks=seq(1,7,1)) +
  scale_y_continuous(breaks=seq(0,200,10)) +
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 11),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

# Mean +- SD for first egg lay date (reduced dataset)
ggplot(bluti2_red_fed_summary, aes(x=yo, y=mean, fill = factor(yo))) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd)) +
  geom_point(colour = "black", size = 2) +
  labs(x = "Age of female parent", y = "Lay date of first egg (1 = Jan 1st)", title="Reduced dataset") +
  theme_bw() +
  guides(fill="none") +
  scale_x_continuous(breaks=seq(1,7,1)) +
  scale_y_continuous(breaks=seq(0,200,10))

# Barplot with SD bars for clutch size
ggplot(bluti2_cs_summary, aes(x=yo, y=mean, fill = factor(yo))) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd)) +
  geom_col(colour = "black") +
  labs(x = "Age of female parent", y = "Clutch size") +
  theme_bw() +
  guides(fill="none") +
  scale_x_continuous(breaks=seq(1,7,1)) +
  scale_y_continuous(breaks=seq(0,12,1),expand = expansion(mult = c(0, .1)))


# Geom_smooth() for fledgeling success (whole dataset)
ggplot(bluti2_suc_summary, aes(x=yo, y=mean)) +
  geom_point(colour = "black", size = 3) +
  geom_smooth(linewidth=0.8, linetype = 2, col="#248fc9") +
  labs(x = "Age of female parent", y = "Number of chicks successfully fledged", title="Complete dataset") +
  theme_bw() +
  scale_x_continuous(breaks=seq(1,7,1))+
  scale_y_continuous(breaks=seq(0,14,1)) +
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 11),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))
ggsave("figures/suc_trend_geomsmooth_complete_new_I.png")

# Geom_smooth() for fledgeling success (reduced subdataset)
ggplot(bluti2_red_suc_summary, aes(x=y_old, y=mean)) +
  geom_point(colour = "black", size = 3) +
  geom_smooth(linewidth=0.8, linetype = 2, col="#248fc9") +
  labs(x = "Age of female parent", y = "Number of chicks successfully fledged", title="Reduced dataset") +
  theme_bw() +
  scale_x_continuous(breaks=seq(1,7,1)) +
  scale_y_continuous(breaks=seq(0,14,1)) +
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 11),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

# Geom_smooth() for lay date of first egg (whole dataset)
ggplot(bluti2_fed_summary, aes(x=yo, y=mean)) +
  geom_point(colour = "black", size = 3) +
  geom_smooth(linewidth=0.8, linetype = 2, col="#248fc9") +
  labs(x = "Age of female parent", y = "Lay date of first egg (1 = Jan 1st)", title="Complete dataset") +
  theme_bw() +
  scale_x_continuous(breaks=seq(1,7,1)) +
  scale_y_continuous(breaks=seq(100,140,5)) +
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 11),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"))) # for some reason, in this graph my y-axis values are gone
ggsave("figures/fed_trend_geomsmooth_complete_new_I.png")

# Geom_smooth() for lay date of first egg (reduced dataset)
ggplot(bluti2_red_fed_summary, aes(x=y_old, y=mean)) +
  geom_point(colour = "black", size = 3) +
  geom_smooth(linewidth=0.8, linetype = 2, col="#248fc9") +
  labs(x = "Age of female parent", y = "Lay date of first egg (1 = Jan 1st)", title="Reduced dataset") +
  theme_bw() +
  scale_x_continuous(breaks=seq(1,7,1)) +
  scale_y_continuous(breaks=seq(0,14,1))  # for some reason, in this graph my y-axis values are gone

# Geom_smooth() for clutch size (complete dataset)
ggplot(bluti2_cs_summary, aes(x=yo, y=mean)) +
  geom_point(colour = "black", size = 3) +
  geom_smooth(linewidth=0.8, linetype = 2, col="#248fc9") +
  labs(x = "Age of female parent", y = "Clutch size", title="Complete dataset") +
  theme_bw() +
  scale_x_continuous(breaks=seq(1,7,1)) +
  scale_y_continuous(breaks=seq(0,14,1)) +
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 11),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))
ggsave("figures/cs_trend_geomsmooth_complete_new_I.png")

# Geom_smooth() for clutch size (reduced dataset)
ggplot(bluti2_red_cs_summary, aes(x=y_old, y=mean)) +
  geom_point(colour = "black", size = 3) +
  geom_smooth(linewidth=0.8, linetype = 2, col="#248fc9") +
  labs(x = "Age of female parent", y = "Clutch size", title="Reduced dataset") +
  theme_bw() +
  scale_x_continuous(breaks=seq(1,7,1))+
  scale_y_continuous(breaks=seq(0,14,1))


### Distinct birds across years and sites ###

# Distinct rings:
bluti2_distinctring <- bluti2 %>% 
  distinct(ring, .keep_all = T)  # I don't think this makes much sense to plot as females can move between sites from one year to the next one and this could would obscure that

# Distinct birds across sites
ggplot(bluti2_distinctring,aes(x=site, fill=site)) +
  geom_bar() +
  theme_bw() +
  guides(fill="none") +
  labs(x="Sites", y="Number of distinct females") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title = element_text(size = 13), 
        axis.text = element_text(size = 11),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"))) +
  scale_y_continuous(breaks=seq(0,60,5), expand=expansion(mult=c(0,0.1))) 

# RECORDINGS (not distinct birds) per site and classifying by age (in years old). Complete dataset.
ggplot(blutidf,aes(x=site, fill=factor(yo))) +
  geom_bar() +
  theme_bw() +
  guides(fill=guide_legend(title="Age in years old")) +
  labs(x="Sites", y="Number of breeding attempts recorded", title="Complete dataset") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_continuous(breaks=seq(0,100,5), expand=expansion(mult=c(0,0.1)))

# RECORDINGS (not distinct birds) per site and classifying by age (in years old). Reduced dataset.
ggplot(bluti2_red,aes(x=site, fill=factor(y_old))) +
  geom_bar() +
  theme_bw() +
  guides(fill=guide_legend(title="Age in years old")) +
  labs(x="Sites", y="Number of breeding attempts recorded", title="Reduced dataset") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_continuous(breaks=seq(0,100,5), expand=expansion(mult=c(0,0.1)))

# Distinct birds across years
ggplot(bluti2_distinctring,aes(x=year, fill=factor(year))) +
  geom_bar() +
  theme_bw() +
  theme(axis.text.x = element_text(vjust = 0.5),
        axis.title = element_text(size = 13), 
        axis.text = element_text(size = 11),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"))) +
  guides(fill="none") +
  labs(x="Years", y="Number of distinct females") +
  scale_x_continuous(breaks=seq(2014,2024,1)) +
  scale_y_continuous(breaks=seq(0,180,25), expand=expansion(mult=c(0,0.1)))

# RECORDINGS (not distinct birds) per year and classifying by age (in years old). Complete dataset.
ggplot(bluti2,aes(x=year, fill=factor(y_old))) +
  geom_bar() +
  theme_bw() +
  guides(fill=guide_legend(title="Age in years old")) +
  labs(x="Sites", y="Number of recordings", title="Complete dataset") +
  scale_x_continuous(breaks=seq(2014,2024,1)) +
  scale_y_continuous(breaks=seq(0,300,20), expand=expansion(mult=c(0,0.1)))

# RECORDINGS (not distinct birds) per site and classifying by age (in years old). Reduced dataset.
ggplot(bluti2_red,aes(x=year, fill=factor(y_old))) +
  geom_bar() +
  theme_bw() +
  guides(fill=guide_legend(title="Age in years old")) +
  labs(x="Sites", y="Number of breeding attempts recorded", title="Reduced dataset") +
  scale_x_continuous(breaks=seq(2014,2024,1)) +
  scale_y_continuous(breaks=seq(0,300,20), expand=expansion(mult=c(0,0.1)))



### Site and environment ###



# Nestboxes per site
sites <- read.csv("data/site_detailsII.csv")

reorder(df_ex$name_age, df_ex$age, FUN = mean)

ggplot(sites, aes(x=site, y=Current.Boxes, fill = site)) +
  geom_col(colour = "black") +
  labs(x = "Sites of study", y = "Number of nestboxes") +
  theme_bw() +
  guides(fill="none") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_continuous(breaks=seq(0,10,1), expand = expansion(mult = c(0, .1)))

# Average occupancy per site

ggplot(environment, aes(x=reorder(sites, latitude), y=occupancy)) +
  geom_point() +
  theme_bw() +
  labs(x = "Sites of study", y = "Average occupancy rate (2014-2021)") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        axis.title = element_text(size = 13), 
        axis.text = element_text(size = 11),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

# Elevation per site

ggplot(environment, aes(x=reorder(sites, latitude), y=elevation)) +
  geom_point() +
  theme_bw() +
  labs(x = "Sites of study", y = "Average site elevation") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        axis.title = element_text(size = 13), 
        axis.text = element_text(size = 11),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

# Absolute number of oaks per site

ggplot(environment, aes(x=reorder(sites, latitude), y=TOTAL_oak)) +
  geom_point() +
  theme_bw() +
  labs(x = "Sites of study", y = "Absolute number of oaks per site") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        axis.title = element_text(size = 13), 
        axis.text = element_text(size = 11),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))


#########################################
### Breeding traits vs. foliage score ###
#########################################

# First egg lay date

fed_df <- blutidf_3yo %>% filter(!is.na(fed))

fed_df$total_fs <- habitat_foliage_scores[match(fed_df$site, habitat_foliage_scores$Site),"Total"]

ggplot(fed_df, aes(x=total_fs, y=fed, col=as.factor(year))) +
  geom_point(alpha=0.25) +
  theme_bw() +
  labs(x = "Total foliage score", y = "First egg lay date (1 = 1st Jan)") +
  guides(col=guide_legend(title="Year")) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(margin = unit(c(1, 0, 0, 0), "mm")), 
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"))) 


# Clutch size

