###############################
### Environmental predictor ###
###############################

library(dplyr)
library(tidyr)

### First, loading blue tit databases:

blutidf <- read.csv("data/blutidf_2025.csv")
blutidf_3yo <- read.csv("data/blutidf_3yo_2025.csv")
site_occupancy <- read.csv("data/site_occupancy.csv")


### Creating a data frame that will store all environmental variables of potential interest:

sites <- read.csv("data/site_detailsII.csv")

environment <- data.frame(sites = sites$site, longitude = sites$Mean.Long, latitude = sites$Mean.Lat, elevation = sites$Mean.Elev)

### Elevation ###

sites <- read.csv("data/site_detailsII.csv")

site_elevation <- sites %>% select(site, Mean.Elev)
blutidf$elevation <- site_elevation$Mean.Elev[match(blutidf$site, site_elevation$site)] 
blutidf_3yo$elevation <- site_elevation$Mean.Elev[match(blutidf_3yo$site, site_elevation$site)] 

#### Tree composition ####

habitat_foliage_scores <- read.csv("data/Habitat_SiteII.csv")
habitat <- read.csv("data/HabitatsII.csv")

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

habitat <- habitat %>% replace(is.na(.), 0)  # replacing all NAs for 0s

habitat <- cbind(habitat_details, habitat)

# Creating a dataframe summarising the number of trees per genus recorded at each nestbox, ignoring their girth
trees_per_box <- data.frame(sites = habitat$site, box = habitat$nestbox, alder = rep(0, nrow(habitat)), ash = rep(0, nrow(habitat)), aspen = rep(0, nrow(habitat)), beech = rep(0, nrow(habitat)), birch = rep(0, nrow(habitat)), blackthorn = rep(0, nrow(habitat)), cherry = rep(0, nrow(habitat)), chestnut = rep(0, nrow(habitat)), conifer = rep(0, nrow(habitat)), elder = rep(0, nrow(habitat)), elm = rep(0, nrow(habitat)), hawthorn = rep(0, nrow(habitat)), hazel = rep(0, nrow(habitat)), holly = rep(0, nrow(habitat)), juniper = rep(0, nrow(habitat)), lime = rep(0, nrow(habitat)), oak = rep(0, nrow(habitat)), pine = rep(0, nrow(habitat)), rose = rep(0, nrow(habitat)), rowan = rep(0, nrow(habitat)), sycamore = rep(0, nrow(habitat)), whitebeam = rep(0, nrow(habitat)), willow = rep(0, nrow(habitat)), yew = rep(0, nrow(habitat)), other = rep(0, nrow(habitat)))

trees_per_box$alder <- habitat$X6_alder + habitat$m_alder + habitat$s_alder  # calculating the number of each tree genus by adding recordings of the same tree genus of different girth
trees_per_box$ash <- habitat$l_ash + habitat$m_ash + habitat$s_ash
trees_per_box$aspen <- habitat$m_aspen + habitat$s_aspen
trees_per_box$beech <- habitat$l_beech + habitat$m_beech + habitat$s_beech + habitat$X6_beech
trees_per_box$birch <- habitat$l_birch + habitat$m_birch + habitat$s_birch + habitat$X21_birch + habitat$X6_birch
trees_per_box$blackthorn <- habitat$z_blackthorn + habitat$X21_blackthorn
trees_per_box$cherry <- habitat$l_cherry + habitat$m_cherry + habitat$s_cherry + habitat$z_cherry + habitat$X6_cherry
trees_per_box$chestnut <- habitat$m_chestnut + habitat$s_chestnut
trees_per_box$conifer <- habitat$l_conifer + habitat$m_conifer + habitat$s_conifer
trees_per_box$elder <- habitat$s_elder + habitat$X6_elder
trees_per_box$elm <- habitat$l_elm + habitat$m_elm + habitat$s_elm
trees_per_box$hawthorn <- habitat$s_hawthorn
trees_per_box$hazel <- habitat$s_hazel + habitat$X6_hazel + habitat$X21_hazel
trees_per_box$holly <- habitat$m_holly + habitat$s_holly + habitat$X6_holly + habitat$X21_holly
trees_per_box$juniper <- habitat$s_juniper + habitat$X6_juniper
trees_per_box$lime <- habitat$m_lime + habitat$s_lime
trees_per_box$oak <- habitat$l_oak + habitat$m_oak + habitat$s_oak
trees_per_box$pine <- habitat$l_pine + habitat$m_pine + habitat$s_pine
trees_per_box$rose <- habitat$X6_rose 
trees_per_box$rowan <- habitat$m_rowan + habitat$s_rowan + habitat$X6_rowan
trees_per_box$sycamore <- habitat$s_sycamore + habitat$l_sycamore + habitat$m_sycamore + habitat$X6_sycamore
trees_per_box$whitebeam <- habitat$m_whitebeam + habitat$s_whitebeam
trees_per_box$willow <- habitat$l_willow + habitat$m_willow + habitat$s_willow + habitat$z_willow + habitat$X6_willow + habitat$X21_willow
trees_per_box$yew <- habitat$m_yew + habitat$s_yew
trees_per_box$other <- habitat$s_other + habitat$z_other


# Calculating the total number of trees recorded at each nest box, irrespective of their genus
trees_per_box$total_trees <- trees_per_box$alder + trees_per_box$ash + trees_per_box$aspen + trees_per_box$beech + trees_per_box$birch + trees_per_box$blackthorn + trees_per_box$cherry + trees_per_box$chestnut + trees_per_box$conifer + trees_per_box$elder + trees_per_box$elm + trees_per_box$hawthorn + trees_per_box$hazel + trees_per_box$holly + trees_per_box$juniper + trees_per_box$lime + trees_per_box$oak + trees_per_box$pine + trees_per_box$rose + trees_per_box$rowan + trees_per_box$sycamore + trees_per_box$whitebeam + trees_per_box$willow + trees_per_box$yew + trees_per_box$other

# Creating a dataframe to store all trees per site rather than per nest box
trees <- data.frame(sites = sites$site, alder = rep(0, nrow(sites)), ash = rep(0, nrow(sites)), aspen = rep(0, nrow(sites)), beech = rep(0, nrow(sites)), birch = rep(0, nrow(sites)), blackthorn = rep(0, nrow(sites)), cherry = rep(0, nrow(sites)), chestnut = rep(0, nrow(sites)), conifer = rep(0, nrow(sites)), elder = rep(0, nrow(sites)), elm = rep(0, nrow(sites)), hawthorn = rep(0, nrow(sites)), hazel = rep(0, nrow(sites)), holly = rep(0, nrow(sites)), juniper = rep(0, nrow(sites)), lime = rep(0, nrow(sites)), oak = rep(0, nrow(sites)), pine = rep(0, nrow(sites)), rose = rep(0, nrow(sites)), rowan = rep(0, nrow(sites)), sycamore = rep(0, nrow(sites)), whitebeam = rep(0, nrow(sites)), willow = rep(0, nrow(sites)), yew = rep(0, nrow(sites)), other = rep(0, nrow(sites)))


for (i in 1:nrow(trees)) {
  trees[i,"alder"] <- sum(trees_per_box[which(trees_per_box$sites ==  trees[i,"sites"]),"alder"])
}  # adding all alders at all nest boxes per site

for (i in 1:nrow(trees)) {
  trees[i,"ash"] <- sum(trees_per_box[which(trees_per_box$sites ==  trees[i,"sites"]),"ash"])
}  # ash

for (i in 1:nrow(trees)) {
  trees[i,"aspen"] <- sum(trees_per_box[which(trees_per_box$sites ==  trees[i,"sites"]),"aspen"])
}  # aspen

for (i in 1:nrow(trees)) {
  trees[i,"beech"] <- sum(trees_per_box[which(trees_per_box$sites ==  trees[i,"sites"]),"beech"])
}  # beech

for (i in 1:nrow(trees)) {
  trees[i,"birch"] <- sum(trees_per_box[which(trees_per_box$sites ==  trees[i,"sites"]),"birch"])
}  # birch

for (i in 1:nrow(trees)) {
  trees[i,"cherry"] <- sum(trees_per_box[which(trees_per_box$sites ==  trees[i,"sites"]),"cherry"])
}  # cherry

for (i in 1:nrow(trees)) {
  trees[i,"blackthorn"] <- sum(trees_per_box[which(trees_per_box$sites ==  trees[i,"sites"]),"blackthorn"])
}  # blackthorn

for (i in 1:nrow(trees)) {
  trees[i,"chestnut"] <- sum(trees_per_box[which(trees_per_box$sites ==  trees[i,"sites"]),"chestnut"])
}  # chestnut

for (i in 1:nrow(trees)) {
  trees[i,"conifer"] <- sum(trees_per_box[which(trees_per_box$sites ==  trees[i,"sites"]),"conifer"])
}  # conifer

for (i in 1:nrow(trees)) {
  trees[i,"elder"] <- sum(trees_per_box[which(trees_per_box$sites ==  trees[i,"sites"]),"elder"])
}  # elder

for (i in 1:nrow(trees)) {
  trees[i,"elm"] <- sum(trees_per_box[which(trees_per_box$sites ==  trees[i,"sites"]),"elm"])
}  # elm

for (i in 1:nrow(trees)) {
  trees[i,"hawthorn"] <- sum(trees_per_box[which(trees_per_box$sites ==  trees[i,"sites"]),"hawthorn"])
}  # hawthorn

for (i in 1:nrow(trees)) {
  trees[i,"hazel"] <- sum(trees_per_box[which(trees_per_box$sites ==  trees[i,"sites"]),"hazel"])
}  # hazel

for (i in 1:nrow(trees)) {
  trees[i,"holly"] <- sum(trees_per_box[which(trees_per_box$sites ==  trees[i,"sites"]),"holly"])
}  # holly

for (i in 1:nrow(trees)) {
  trees[i,"juniper"] <- sum(trees_per_box[which(trees_per_box$sites ==  trees[i,"sites"]),"juniper"])
}  # juniper

for (i in 1:nrow(trees)) {
  trees[i,"lime"] <- sum(trees_per_box[which(trees_per_box$sites ==  trees[i,"sites"]),"lime"])
}  # lime

for (i in 1:nrow(trees)) {
  trees[i,"oak"] <- sum(trees_per_box[which(trees_per_box$sites ==  trees[i,"sites"]),"oak"])
}  # oak

for (i in 1:nrow(trees)) {
  trees[i,"pine"] <- sum(trees_per_box[which(trees_per_box$sites ==  trees[i,"sites"]),"pine"])
}  # pine

for (i in 1:nrow(trees)) {
  trees[i,"rose"] <- sum(trees_per_box[which(trees_per_box$sites ==  trees[i,"sites"]),"rose"])
}  # rose

for (i in 1:nrow(trees)) {
  trees[i,"rowan"] <- sum(trees_per_box[which(trees_per_box$sites ==  trees[i,"sites"]),"rowan"])
}  # rowan

for (i in 1:nrow(trees)) {
  trees[i,"sycamore"] <- sum(trees_per_box[which(trees_per_box$sites ==  trees[i,"sites"]),"sycamore"])
}  # sycamore

for (i in 1:nrow(trees)) {
  trees[i,"whitebeam"] <- sum(trees_per_box[which(trees_per_box$sites ==  trees[i,"sites"]),"whitebeam"])
}  # whitebeam

for (i in 1:nrow(trees)) {
  trees[i,"willow"] <- sum(trees_per_box[which(trees_per_box$sites ==  trees[i,"sites"]),"willow"])
}  # willow

for (i in 1:nrow(trees)) {
  trees[i,"yew"] <- sum(trees_per_box[which(trees_per_box$sites ==  trees[i,"sites"]),"yew"])
}  # yew

for (i in 1:nrow(trees)) {
  trees[i,"other"] <- sum(trees_per_box[which(trees_per_box$sites ==  trees[i,"sites"]),"other"])
}  # other


trees$total_trees <- trees$alder + trees$ash + trees$aspen + trees$beech + trees$birch + trees$blackthorn + trees$cherry + trees$chestnut + trees$conifer + trees$elder + trees$elm + trees$hawthorn + trees$hazel + trees$holly + trees$juniper +trees$lime + trees$oak + trees$pine + trees$rose + trees$rowan + trees$sycamore + trees$whitebeam + trees$willow + trees$yew + trees$other 

trees$oak_proportion <- trees$oak/trees$total_trees

#trees <- trees %>% relocate(oak_proportion, .before = pine)

environment$proportion_oak <- trees$oak_proportion

# 3. Absolute number of oak and birch (combined)

trees$oak_birch <- trees$birch + trees$oak

environment$oak_birch <- trees$oak_birch

# 4. Proportion of oak and birch

trees$proportion_oak_birch <- trees$oak_birch/trees$total_trees

environment$proportion_oak_birch <- trees$proportion_oak_birch

# 5. Absolute number of oak, sycamore and birch (combined)

trees$oak_birch_sycamore <- trees$birch + trees$oak + trees$sycamore

environment$oak_birch_sycamore <- trees$oak_birch_sycamore

# 6. Proportion of oak, sycamore and birch

trees$proportion_oak_birch_sycamore <- trees$oak_birch_sycamore/trees$total_trees

environment$proportion_oak_birch_sycamore <- trees$proportion_oak_birch_sycamore

# 7. Tree diversity (at genus level)

library(vegan)

trees <- trees %>% relocate(c(conifer, other), .after = yew)

# I will be using the Simpson's Index to calculate tree diversity

tree.simp <- diversity(trees[,2:24],index = "simpson")  # I'm excluding the columns "other" and "conifers" because I don't exactly know what taxa they comprise and whether said taxa (or some of it) are already present in other columns (f.e. pine trees)

environment$tree_diversity_simpson <- tree.simp

# 8. Total foliage score 

level_order <- c("EDI", "RSY", "FOF", "BAD", "LVN", "DOW", "GLF", "SER", "MCH", "PTH", "STY", "BIR", "DUN", "BLG", "PIT", "KCK", "KCZ", "BLA", "CAL", "DNM", "DNC", "DNS", "DLW", "CRU", "NEW", "HWP", "INS", "FSH", "RTH", "AVI", "AVN", "CAR", "SLS", "TOM", "DAV", "ART", "MUN", "FOU", "ALN", "DEL", "TAI", "SPD", "OSP", "DOR")
habitat_foliage_scores <- habitat_foliage_scores %>% arrange(factor(Site, levels = level_order))

fs <- habitat_foliage_scores[,1:12]  # selecting columns with absolute foliage scores

score <- fs %>% pivot_longer(cols=c(Alder_FS, Ash_FS, Aspen_FS, Beech_FS, Birch_FS, Elm_FS, Oak_FS, Sycamore_FS, Willow_FS, Conifer_FS, OthDecid_FS), names_to = "foliage_score")

ggplot(score, aes(x=Site, y=value, fill = foliage_score)) +
  geom_col(colour="black") +
  theme_bw() +
  labs(x = "Sites", y = "Foliage scores") +
  guides(fill=guide_legend(title="Foliage score")) +
  scale_fill_hue(labels = c("Alder", "Ash", "Aspen", "Beech", "Birch", "Conifer", "Elm", "Oak", "Other deciduous", "Sycamore", "Willow")) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, margin = unit(c(3, 0, 0, 0), "mm")), 
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

environment$total_FS <- habitat_foliage_scores[match(environment$sites, habitat_foliage_scores$Site), "Total"]

# 9. Oak foliage score

environment$oak_FS <- habitat_foliage_scores[match(environment$sites, habitat_foliage_scores$Site),"Oak_FS"]

# 10. Birch foliage score

environment$birch_FS <- habitat_foliage_scores[match(environment$sites, habitat_foliage_scores$Site),"Birch_FS"]

# 11. Sycamore foliage score

environment$sycamore_FS <- habitat_foliage_scores[match(environment$sites, habitat_foliage_scores$Site),"Sycamore_FS"]

# 12. Willow foliage score

environment$willow_FS <- habitat_foliage_scores[match(environment$sites, habitat_foliage_scores$Site),"Willow_FS"]

# 13. Beech foliage score

environment$beech_FS <- habitat_foliage_scores[match(environment$sites, habitat_foliage_scores$Site),"Beech_FS"]

# 14. Oak, sycamore and birch foliage score

oak_birch_sycamore_FS <- habitat_foliage_scores %>% 
  select(c(Site,Oak_FS,Birch_FS,Sycamore_FS)) %>%
  mutate(sum = Oak_FS + Birch_FS + Sycamore_FS)

environment$oak_birch_sycamore_FS <- oak_birch_sycamore_FS[match(environment$sites, oak_birch_sycamore_FS$Site),"sum"]

# 15. Oak and birch foliage score

oak_birch_FS <- habitat_foliage_scores %>% 
  select(c(Site,Oak_FS,Birch_FS)) %>%
  mutate(sum = Oak_FS + Birch_FS)

environment$oak_birch_FS <- oak_birch_FS[match(environment$sites, oak_birch_FS$Site),"sum"]

# 16. Oak foliage score (proportion)

environment$oak_FS_prop <- habitat_foliage_scores[match(environment$sites, habitat_foliage_scores$Site), "Oak_prop"]

# 17. Oak, sycamore and birch foliage score (proportion)

oak_birch_sycamore_FS_prop <- habitat_foliage_scores %>% 
  select(c(Site,Oak_prop,Birch_prop,Sycamore_prop)) %>%
  mutate(sum = Oak_prop + Birch_prop + Sycamore_prop)

environment$oak_birch_sycamore_FS_prop <- oak_birch_sycamore_FS_prop[match(environment$sites, oak_birch_sycamore_FS_prop$Site),"sum"]


# 18. Oak and birch foliage score (proportion)

oak_birch_FS_prop <- habitat_foliage_scores %>% 
  select(c(Site,Oak_prop,Birch_prop,Sycamore_prop)) %>%
  mutate(sum = Oak_prop + Birch_prop)

environment$oak_birch_FS_prop <- oak_birch_FS_prop[match(environment$sites, oak_birch_FS_prop$Site),"sum"]

# 19. Deciduous trees foliage score:

environment$alder_FS <- habitat_foliage_scores[match(environment$sites, habitat_foliage_scores$Site),"Alder_FS"]
environment$ash_FS <- habitat_foliage_scores[match(environment$sites, habitat_foliage_scores$Site),"Ash_FS"]
environment$aspen_FS <- habitat_foliage_scores[match(environment$sites, habitat_foliage_scores$Site),"Aspen_FS"]
environment$elm_FS <- habitat_foliage_scores[match(environment$sites, habitat_foliage_scores$Site),"Elm_FS"]
environment$OthDecid_FS <- habitat_foliage_scores[match(environment$sites, habitat_foliage_scores$Site),"OthDecid_FS"]

environment <- environment %>% 
  mutate(all_decid_FS = alder_FS + ash_FS + aspen_FS + beech_FS + birch_FS + elm_FS + oak_FS + sycamore_FS + willow_FS + OthDecid_FS)

