########################
### PROOF OF CONCEPT ###
########################

library(ggplot2)


x <- c(1:7)  # Number of breeding attempts
y <- c(811, 192, 78, 36, 16, 0, 1)  # I need to change this so that it directly references the database
brattempts_age <- data.frame(x, y)

# Number of females for which we have re
plot(brattempts_age, type = "b", xlab="Number of breeding attempts recorded", ylab="Number of females")
ggplot(data=brattempts_age, aes(x=x, y=y)) +
  geom_point(size = 2) +
  geom_line(colour="#248fc9") +
  theme_bw() +
  labs(x="Number of breeding attempts", y="Number of females") +
  scale_x_continuous(breaks=seq(1,7,1)) +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))
  

bluti2$suc <- as.numeric(bluti2$suc)

### Distribution of the breeding trait variables ###

# Fledgeling success
ggplot(bluti2, aes(suc)) +
  geom_histogram(colour = "black", fill = "#248fc9", binwidth=1) +
  labs(x = "Number of chicks fledged successfully", y = "Frequency") +
  theme_bw() +
  scale_x_continuous(breaks=seq(0,13,1)) +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))

# Lay date of first egg
ggplot(bluti2, aes(fed)) +
  geom_histogram(colour = "black", fill = "#59B7EB", binwidth=1) +
  labs(x = "Lay date of first egg (1 = Jan 1st)", y = "Frequency") +
  theme_bw() +
  scale_x_continuous(breaks=seq(100,150,5)) +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))

# Clutch size
ggplot(bluti2, aes(cs)) +
  geom_histogram(colour = "black", fill = "#A2DEFF", binwidth=1) +
  labs(x = "Clutch size", y = "Frequency") +
  theme_bw() +
  scale_x_continuous(breaks=seq(0,23,2)) +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))

### Age-specific patterns of breeding traits ###

# Violin plot for fledgeling success
ggplot(bluti2, aes(x=y_old, y=suc, group=y_old)) +
  geom_violin(aes(fill=factor(y_old), colour = "black"),stat = "ydensity", position = "dodge", colour = "black", scale = "count", adjust=0.7) +
  labs(x = "Age of female parent", y = "Number of chicks successfully fledged") +
  theme_bw() +
  guides(fill=guide_legend(title="Age in years old")) +
  scale_x_continuous(breaks=seq(1,7,1))+
  scale_y_continuous(breaks=seq(1,14,1))

# Violin plot for lay date of first egg
ggplot(bluti2, aes(x=y_old, y=fed, group=y_old)) +
  geom_violin(aes(fill=factor(y_old), colour = "black"),stat = "ydensity", position = "dodge", colour = "black", scale = "count", adjust=0.7) +
  labs(x = "Age of female parent", y = "First egg lay date (ordinal numbers)") +
  theme_bw() +
  guides(fill=guide_legend(title="Age in years old"))

# Violin plot for clutch size
ggplot(bluti2, aes(x=y_old, y=cs, group=y_old)) +
  geom_violin(aes(fill=factor(y_old), colour = "black"),stat = "ydensity", position = "dodge", colour = "black", scale = "count", adjust=0.7) +
  labs(x = "Age of female parent", y = "Clutch size") +
  theme_bw() +
  guides(fill=guide_legend(title="Age in years old")) +
  scale_x_continuous(breaks=seq(1,7,1)) +
  scale_y_continuous(breaks=seq(0,14,1))

# Now, I'll create new dataframes that will include the mean values for each breeding trait for each age group (years old) and their corresponding standard deviation

## For fledgeling success
bluti2_suc_summary <- bluti2 %>%
  group_by(y_old) %>%
  summarise_at(vars(suc), list(mean=mean, sd=sd)) %>% 
  as.data.frame()
## For lay date of first egg
bluti2_fed_summary <- bluti2 %>%
  group_by(y_old) %>%
  na.omit(bluti2_fed_summary) %>%
  summarise_at(vars(fed), list(mean=mean, sd=sd)) %>% 
  as.data.frame()
## For clutch size
bluti2_cs_summary <- bluti2 %>%
  group_by(y_old) %>%
  na.omit(bluti2_cs_summary) %>%
  summarise_at(vars(cs), list(mean=mean, sd=sd)) %>% 
  as.data.frame()

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
ggplot(bluti2, aes(x=y_old, y=fed, fill=factor(w))) +
  geom_point(aes(fill=w, alpha = 0.01)) +
  labs(x = "Age of female parent", y = "First egg lay date (ordinal numbers)") +
  theme_bw() +
  scale_x_continuous(breaks=seq(1,7,1)) 

# Barplot with SD bars for fledgeling success
ggplot(bluti2_suc_summary, aes(x=y_old, y=mean, fill = factor(y_old))) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd)) +
  geom_col(colour = "black") +
  labs(x = "Age of female parent", y = "Number of chicks successfully fledged") +
  theme_bw() +
  guides(fill="none") +
  scale_x_continuous(breaks=seq(1,7,1))+
  scale_y_continuous(breaks=seq(0,14,1), expand = expansion(mult = c(0, .1)))

# Barplot with SD bars for first egg lay date
ggplot(bluti2_fed_summary, aes(x=y_old, y=mean, fill = factor(y_old))) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd)) +
  geom_col(colour = "black") +
  labs(x = "Age of female parent", y = "Lay date of first egg (1 = Jan 1st)") +
  theme_bw() +
  guides(fill="none") +
  scale_x_continuous(breaks=seq(1,7,1)) +
  scale_y_continuous(breaks=seq(0,200,10), expand = expansion(mult = c(0, .1)))

# Mean +- SD for first egg lay date (complete dataset)
ggplot(bluti2_fed_summary, aes(x=y_old, y=mean, fill = factor(y_old))) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd)) +
  geom_point(colour = "black", size = 2) +
  labs(x = "Age of female parent", y = "Lay date of first egg (1 = Jan 1st)", title="Complete dataset") +
  theme_bw() +
  guides(fill="none") +
  scale_x_continuous(breaks=seq(1,7,1)) +
  scale_y_continuous(breaks=seq(0,200,10))

# Mean +- SD for first egg lay date (reduced dataset)
ggplot(bluti2_red_fed_summary, aes(x=y_old, y=mean, fill = factor(y_old))) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd)) +
  geom_point(colour = "black", size = 2) +
  labs(x = "Age of female parent", y = "Lay date of first egg (1 = Jan 1st)", title="Reduced dataset") +
  theme_bw() +
  guides(fill="none") +
  scale_x_continuous(breaks=seq(1,7,1)) +
  scale_y_continuous(breaks=seq(0,200,10))

# Barplot with SD bars for clutch size
ggplot(bluti2_cs_summary, aes(x=y_old, y=mean, fill = factor(y_old))) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd)) +
  geom_col(colour = "black") +
  labs(x = "Age of female parent", y = "Clutch size") +
  theme_bw() +
  guides(fill="none") +
  scale_x_continuous(breaks=seq(1,7,1)) +
  scale_y_continuous(breaks=seq(0,12,1),expand = expansion(mult = c(0, .1)))


# Geom_smooth() for fledgeling success (whole dataset)
ggplot(bluti2_suc_summary, aes(x=y_old, y=mean)) +
  geom_point(colour = "black", size = 3) +
  geom_smooth(linewidth=0.8, linetype = 2, col="#248fc9") +
  labs(x = "Age of female parent", y = "Number of chicks successfully fledged", title="Complete dataset") +
  theme_bw() +
  scale_x_continuous(breaks=seq(1,7,1))+
  scale_y_continuous(breaks=seq(0,14,1))

# Geom_smooth() for fledgeling success (reduced subdataset)
ggplot(bluti2_red_suc_summary, aes(x=y_old, y=mean)) +
  geom_point(colour = "black", size = 3) +
  geom_smooth(linewidth=0.8, linetype = 2, col="#248fc9") +
  labs(x = "Age of female parent", y = "Number of chicks successfully fledged", title="Reduced dataset") +
  theme_bw() +
  scale_x_continuous(breaks=seq(1,7,1))+
  scale_y_continuous(breaks=seq(0,14,1))

# Geom_smooth() for lay date of first egg (whole dataset)
ggplot(bluti2_fed_summary, aes(x=y_old, y=mean)) +
  geom_point(colour = "black", size = 3) +
  geom_smooth(linewidth=0.8, linetype = 2, col="#248fc9") +
  labs(x = "Age of female parent", y = "Lay date of first egg (1 = Jan 1st)", title="Complete dataset") +
  theme_bw() +
  scale_x_continuous(breaks=seq(1,7,1))+
  scale_y_continuous(breaks=seq(0,14,1))  # for some reason, in this graph my y-axis values are gone

# Geom_smooth() for lay date of first egg (reduced dataset)
ggplot(bluti2_red_fed_summary, aes(x=y_old, y=mean)) +
  geom_point(colour = "black", size = 3) +
  geom_smooth(linewidth=0.8, linetype = 2, col="#248fc9") +
  labs(x = "Age of female parent", y = "Lay date of first egg (1 = Jan 1st)", title="Reduced dataset") +
  theme_bw() +
  scale_x_continuous(breaks=seq(1,7,1))+
  scale_y_continuous(breaks=seq(0,14,1))  # for some reason, in this graph my y-axis values are gone

# Geom_smooth() for clutch size (complete dataset)
ggplot(bluti2_cs_summary, aes(x=y_old, y=mean)) +
  geom_point(colour = "black", size = 3) +
  geom_smooth(linewidth=0.8, linetype = 2, col="#248fc9") +
  labs(x = "Age of female parent", y = "Clutch size", title="Complete dataset") +
  theme_bw() +
  scale_x_continuous(breaks=seq(1,7,1))+
  scale_y_continuous(breaks=seq(0,14,1))

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
  distinct(ring) 

# Distinct birds across sites
ggplot(bluti2_distinctring,aes(x=site, fill=site)) +
  geom_bar() +
  theme_bw() +
  guides(fill="none") +
  labs(x="Sites", y="Number of distinct birds") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_continuous(breaks=seq(0,60,5), expand=expansion(mult=c(0,0.1)))

# RECORDINGS (not distinct birds) per site and classifying by age (in years old). Complete dataset.
ggplot(bluti2,aes(x=site, fill=factor(y_old))) +
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
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1)) +
  theme_bw() +
  guides(fill="none") +
  labs(x="Years", y="Number of distinct birds") +
  scale_x_continuous(breaks=seq(2014,2024,1)) +
  scale_y_continuous(breaks=seq(0,180,25), expand=expansion(mult=c(0,0.1)))

# RECORDINGS (not distinct birds) per year and classifying by age (in years old). Complete dataset.
ggplot(bluti2,aes(x=year, fill=factor(y_old))) +
  geom_bar() +
  theme_bw() +
  guides(fill=guide_legend(title="Age in years old")) +
  labs(x="Sites", y="Number of breeding attempts recorded", title="Complete dataset") +
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


# Nestboxes per site
sites <- read.csv("data/site_detailsII.csv")

ggplot(sites, aes(x=site, y=Current.Boxes, fill = site)) +
  geom_col(colour = "black") +
  labs(x = "Sites of study", y = "Number of nestboxes") +
  theme_bw() +
  guides(fill="none") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_continuous(breaks=seq(0,10,1), expand = expansion(mult = c(0, .1)))
