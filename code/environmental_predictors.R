###############################
### Environmental predictor ###
###############################

library(dplyr)
library(tidyr)

### First, loading blue tit databases:

blutidf <- read.csv("data/blutidf.csv")
blutidf_3yo <- read.csv("data/blutidf_3yo.csv")


### Creating a data frame that will store all environmental variables of potential interest:

sites <- read.csv("data/site_detailsII.csv")

environment <- data.frame(sites = sites$site, longitude = sites$Mean.Long, latitude = sites$Mean.Lat, elevation = sites$Mean.Elev)


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

for (i in 1:nrow(environment)) {
  environment[i,"TOTAL_oak"] <- sum(oak_sites[which(oak_sites$site ==  environment[i,"sites"]),"TOTAL_oak"])
}


# 2. Proportion of oaks per site 

habitat_details <- habitat[,c(1,2,3,4)]

habitat <- habitat[,-c(1,2,3,4)]

habitat <- habitat %>% mutate_all(~na_if(., 0))

habitat <- habitat %>% replace_na(list(X6_alder=0, m_alder=0, s_alder=0, l_ash=0, m_ash=0, s_ash=0, m_aspen=0, s_aspen=0, l_beech=0, m_beech=0, s_beech=0, X21_birch=0, X6_birch=0, m_birch=0, s_birch=0, X21_blackthorn=0, X6_cherry=0, m_cherry=0, s_cherry=0, m_chestnut=0, s_chestnut=0, l_conifer=0, m_conifer=0, s_conifer=0, X6_elder=0, s_elder=0, l_elm=0, m_elm=0, s_elm=0, s_hawthorn=0, X21_hazel=0, X6_hazel=0, s_hazel=0, X21_holly=0, X6_holly=0, m_holly=0, s_holly=0, m_lime=0, s_lime=0, l_oak=0, m_oak=0, s_oak=0, l_pine=0, m_pine=0, s_pine=0))

habitat <- habitat[,-50]

habitat <- habitat %>% replace_na(list(X6_alder=0, m_alder=0, s_alder=0, l_ash=0, m_ash=0, s_ash=0, m_aspen=0, s_aspen=0, l_beech=0, m_beech=0, s_beech=0, X21_birch=0, X6_birch=0, m_birch=0, s_birch=0, X21_blackthorn=0, X6_cherry=0, m_cherry=0, s_cherry=0, m_chestnut=0, s_chestnut=0, l_conifer=0, m_conifer=0, s_conifer=0, X6_elder=0, s_elder=0, l_elm=0, m_elm=0, s_elm=0, s_hawthorn=0, X21_hazel=0, X6_hazel=0, s_hazel=0, X21_holly=0, X6_holly=0, m_holly=0, s_holly=0, m_lime=0, s_lime=0, l_oak=0, m_oak=0, s_oak=0, l_pine=0, m_pine=0, s_pine=0))

trees_per_box <- data.frame(sites = habitat$site, box = habitat$nestbox, alder = rep(0, nrow(habitat)), ask = rep(0, nrow(habitat)), aspen = rep(0, nrow(habitat)), alder = rep(0, nrow(habitat)), beech = rep(0, nrow(habitat)), birch = rep(0, nrow(habitat)), blackthorn = rep(0, nrow(habitat)), cherry = rep(0, nrow(habitat)), chestnut = rep(0, nrow(habitat)), conifer = rep(0, nrow(habitat)), elder = rep(0, nrow(habitat)), elm = rep(0, nrow(habitat)), hawthorn = rep(0, nrow(habitat)), hazel = rep(0, nrow(habitat)), holly = rep(0, nrow(habitat)), lime = rep(0, nrow(habitat)), oak = rep(0, nrow(habitat)), pine = rep(0, nrow(habitat)))

trees_per_box$alder <- sum(na.omit(c(habitat[,"X6_alder"], habitat[,"m_alder"], habitat[,"s_alder"])))

trees <- data.frame(sites = sites$site, alder = rep(0, nrow(sites)), ask = rep(0, nrow(sites)), aspen = rep(0, nrow(sites)), alder = rep(0, nrow(sites)), beech = rep(0, nrow(sites)), birch = rep(0, nrow(sites)), blackthorn = rep(0, nrow(sites)), cherry = rep(0, nrow(sites)), chestnut = rep(0, nrow(sites)), conifer = rep(0, nrow(sites)), elder = rep(0, nrow(sites)), elm = rep(0, nrow(sites)), hawthorn = rep(0, nrow(sites)), hazel = rep(0, nrow(sites)), holly = rep(0, nrow(sites)), lime = rep(0, nrow(sites)), oak = rep(0, nrow(sites)), pine = rep(0, nrow(sites)))


### Elevation ###

sites <- read.csv("data/site_detailsII.csv")
blutidf <- read.csv("data/blutidf.csv")
blutidf_3yo <- read.csv("data/blutidf_3yo.csv")

site_elevation <- sites %>% select(site, Mean.Elev)
blutidf$elevation <- site_elevation$Mean.Elev[match(blutidf$site, site_elevation$site)] 
blutidf_3yo$elevation <- site_elevation$Mean.Elev[match(blutidf_3yo$site, site_elevation$site)] 



