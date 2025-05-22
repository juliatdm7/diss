###############################
### Environmental predictor ###
###############################

library(dplyr)
library(tidyr)


#### Tree composition ####

habitat_foliage_sores <- read.csv("data/Habitat_SiteII.csv")
habitat <- read.csv("data/Habitats.csv")

# 1. Absolute number of oaks per site

oak_sites <- habitat %>% select(site, nestbox, l_oak, m_oak, s_oak)
oak_sites <- oak_sites %>% replace_na(list(l_oak = 0, m_oak = 0, s_oak = 0))



oak_sites$TOTAL_oak <- 0
for (i in 1:nrow(oak_sites)) {
  oak_sites[i,"TOTAL_oak"] <- sum(c(oak_sites[i,"s_oak"], oak_sites[i,"m_oak"], oak_sites[i,"l_oak"]))
}


### Elevation ###

sites <- read.csv("data/site_detailsII.csv")
blutidf <- read.csv("data/blutidf.csv")
blutidf_3yo <- read.csv("data/blutidf_3yo.csv")

site_elevation <- sites %>% select(site, Mean.Elev)
blutidf$elevation <- site_elevation$Mean.Elev[match(blutidf$site, site_elevation$site)] 
blutidf_3yo$elevation <- site_elevation$Mean.Elev[match(blutidf_3yo$site, site_elevation$site)] 

