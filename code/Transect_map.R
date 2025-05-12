#########################
### Transect map code ###
#########################

# WARNING. This code has been modified from Megan Stamp's code.

library(sf) # general spatial manipulation
library(giscoR) # for the shape of Scotland
library(tidyverse) 


# Scotland as a spatial object
scotland <- giscoR::gisco_get_nuts(nuts_id = 'UKM',
                                   resolution = '01') 

sites <- read.csv("data/site_detailsII.csv") %>% 
  st_as_sf(coords = c("Mean.Long", "Mean.Lat"), crs = st_crs(scotland) ) #crs = 4326


sites$lat <- c(55.977, 56.02237, 56.05684, 56.12171, 56.1746, 56.16253, 56.30158, 56.35109, 56.35749, 56.42266, 56.47694, 56.53707, 56.57103, 56.64949, 56.681, 56.73079, 56.73073, 56.76043, 56.77057, 56.79876, 56.81636, 56.83151, 56.9207, 56.99202, 57.05359, 57.10939, 57.07229, 57.11714, 57.14681, 57.18645, 57.21044, 57.28932, 57.2966, 57.32509, 57.41388, 57.5084, 57.5528, 57.6398, 57.69372, 57.72381, 57.79614, 57.87125, 57.8776, 57.8853)

ggplot() +
  geom_sf(data = scotland, fill = "lightgray", alpha = 0.5, linewidth = 0.35, colour="black") + # borders of Scotland
  geom_sf(data = sites, pch = 16, size = 2, color = "#248fc9", alpha = 0.7) + #Transect sites
  theme_bw() +  # Gridlines and latitude as axes
  labs(x="Longitude", y="Latitude") +
  coord_sf(xlim = c(-8, -1), ylim = c(54.6, 59.55)) 
ggsave("figures/sitesMap_III.png")


