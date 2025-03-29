########################
### PROOF OF CONCEPT ###
########################

library(ggplot2)


x <- c(1:7)  # Number of breeding attempts
y <- c(811, 192, 78, 36, 16, 0, 1)  # I need to change this so that it directly references the database
brattempts_age <- data.frame(x, y)

# Number of females for which we have re
plot(brattempts_age, type = "b", xlab="Number of breeding attempts recorded", ylab="Number of females")

bluti2$suc <- as.numeric(bluti2$suc)

### Distribution of the breeding trait variables ###

# Fledgeling success
ggplot(bluti2, aes(suc)) +
  geom_histogram(colour = "black", fill = "#248fc9", binwidth=1) +
  labs(x = "Nr of chicks fledged successfully", y = "Frequency") +
  theme_bw() +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))

# Lay date of first egg
ggplot(bluti2, aes(fed)) +
  geom_histogram(colour = "black", fill = "#248fc9", binwidth=1) +
  labs(x = "Lay date of first egg (1 = Jan 1st)", y = "Frequency") +
  theme_bw() +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))

# Clutch size
ggplot(bluti2, aes(cs)) +
  geom_histogram(colour = "black", fill = "#248fc9", binwidth=1) +
  labs(x = "Clutch size", y = "Frequency") +
  theme_bw() +
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

# Mean +- SD for first egg lay date
ggplot(bluti2_fed_summary, aes(x=y_old, y=mean, fill = factor(y_old))) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd)) +
  geom_point(colour = "black", size = 2) +
  labs(x = "Age of female parent", y = "Lay date of first egg (1 = Jan 1st)") +
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
  distinct(ring,.keep_all=T)

# Distinct birds across sites
ggplot(bluti2_distinctring,aes(x=site, fill=site)) +
  geom_bar() +
  theme_bw() +
  guides(fill="none") +
  labs(x="Sites", y="Number of distinct birds") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_continuous(breaks=seq(0,60,5), expand=expansion(mult=c(0,0.1)))

# Distinct birds across years
ggplot(bluti2_distinctring,aes(x=year, fill=factor(year))) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1)) +
  theme_bw() +
  guides(fill="none") +
  labs(x="Years", y="Number of distinct birds") +
  scale_x_continuous(breaks=seq(2014,2024,1)) +
  scale_y_continuous(breaks=seq(0,180,25), expand=expansion(mult=c(0,0.1)))

### Map of study sites ###### Map of study_oldy sites ###

install.packages("terra",dependencies=TRUE,repos="https://cloud.r-project.org")
library(terra)
install.packages("geodata",dependencies=TRUE,repos="https://cloud.r-project.org")
library(geodata)
library(UKFE)
UKOutline

wrld <- world(path=".")
plot(wrld, xlim=c(-7,-2), ylim=c(55,59), col="light grey", border="black", xlab = "Longitude", ylab= "Latitude")
sites <- read.csv("data/site_details.csv")
points(sites$Mean.Long, sites$Mean.Lat, col = "blue", pch = 20)

UKmap <- map_data("world", region="UK")
ggplot(data = UKmap, aes(x = long, y = lat, group = group)) + 
  geom_polygon(colour="black", fill="lightgray") +
  theme_bw()

ggplot(UKmap, aes(map_id = region)) +
  geom_map(data = UKmap, map = UKmap,
           aes(x = long, y = lat, map_id = region))

ggplot(UK.sf) +
  geom_sf()


#### I would like to use the following code though ####
# First, we need to load packages that we will be using:

install.packages("mapdata")
library(mapdata)
library(maps)
library(sf)
# Now, we load the data we will be working with

bird.dat.tot <- read.csv("data/atlas_open_data_files/distributions.csv")
coords.dat <- read.csv("data/atlas_open_data_files/grid_square_coordinates_lookup.csv")

bird.dat.tot <- bird.dat.tot[which(bird.dat.tot$resolution==10),]
coords.dat <- coords.dat[which(coords.dat$resolution==10),]

map(database="world",regions="UK")

UK <- map(database="world",regions="UK") # here we visualise the UK outline
points(sites$Mean.Long, sites$Mean.Lat) # here we superimpose the coordinates in coords.dat to this outline

UK.sf <- st_as_sf(UK) # here we're storing the poligon of the outline of UK into a "Spatial Feature" (sf), which is useful to work with GIS on R
UK.sf <- st_cast(UK.sf, "POLYGON") # once saved as a sf, we change the geometry into a "polygon" (or rather, we make sure that its geometry is identified as a polygon)
plot(st_geometry(UK.sf)) # we use st_geometry() to make sure that we're plotting only the outline and not a filling. In this particular case, this makes it more efficient and we need less computer power to get the same thing. 
points(coords.dat)

coords.sf <- st_as_sf(coords.dat[,1:2],coords = c("long", "lat"), crs = st_crs(UK.sf)) # we're transforming the data in coords.dat into sf

coords.dat.island.ind <- st_intersects(UK.sf,coords.sf)[[15]] # we extract all the points that intersect with the 15th polygon: main island.
coords.dat.island <- coords.dat[coords.dat.island.ind,] # we select the rows in coords.dat that correspond to those points

# And we visualise the output:

plot(st_geometry(UK.sf))
points(coords.dat.island)
#####################################################################################################################################################################################################################################

rec_per_site <- bluti2 %>% 
  group_by(site) %>%
  count(site)
barplot(rec_per_site$n ~ rec_per_site$site, horiz= T)


ggplot(bluti2, aes(year,fed,col=site)) +
  geom_point() +
  xlab("Year") +
  ylab("Fledgeling success") +
  theme_bw()


###########  
### MAP ###
###########

install.packages("spDataLarge", repos = "https://geocompr.r-universe.dev")

library(sf)
library(terra)
library(dplyr)
library(spData)
library(spDataLarge)

install.packages("tmap", repos = c("https://r-tmap.r-universe.dev",
                                   "https://cloud.r-project.org"))
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package

nz_elev = rast(system.file("raster/nz_elev.tif", package = "spDataLarge"))
