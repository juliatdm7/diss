########################
### PROOF OF CONCEPT ###
########################

x <- c(1:8)  # Number of breeding attempts
y <- c(805, 197, 78, 36, 17, 0, 0, 1)  # I need to change this so that it directly references the database
brattempts_age <- data.frame(x, y)

plot(brattempts_age, type = "b", xlab="Number of breeding attempts recorded", ylab="Number of females")

bluti2$suc <- as.numeric(bluti2$suc)
ggplot(bluti2, aes(suc)) +
  geom_histogram(colour = "black", fill = "#248fc9", binwidth=1) +
  labs(x = "Nr of chicks fledged successfully", y = "Frequency") +
  theme_bw() +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))


### Map of study sites ###

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
