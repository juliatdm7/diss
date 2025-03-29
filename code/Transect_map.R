library(sf) # general spatial manipulation
library(giscoR) # for the shape of Scotland
library(tidyverse) 


# Scotland as a spatial object
scotland <- giscoR::gisco_get_nuts(nuts_id = 'UKM',
                                   resolution = '01') 

sites <- read.csv("C:/Users/s1413233/Dropbox/master_data/site/site_details.csv") %>% 
  st_as_sf(coords = c("Mean.Long", "Mean.Lat"), crs = st_crs(scotland) ) #crs = 4326

sites_map <- 
  ggplot() +
  geom_sf(data = scotland, fill = "#99B379", alpha = 0.5, linewidth = 1) + # borders of Scotland
  geom_sf(data = sites, pch = 16, size = 2, color = "#e76f51") + #Transect sites
  theme_minimal() # Gridlines and latitude as axes
  #theme_void() #No gridlines or axes



