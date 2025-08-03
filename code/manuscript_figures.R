##########################
### Manuscript figures ###
##########################

# Loading packages

library(ggeffects)
library(gridExtra)
library(ggplot2)
library(dplyr)
library(tidyr)

# Loading data sets

blutidf <- read.csv("data/blutidf_2025.csv")
blutidf_3yo <- read.csv("data/blutidf_3yo_2025.csv")
fed_df <- read.csv("data/fed_df_2025.csv")
cs_df <- read.csv("data/cs_df_2025.csv")
suc_df <- read.csv("data/suc_df_2025.csv")
habitat_foliage_scores <- read.csv("data/Habitat_SiteII.csv")
environment <- read.csv("data/environment_II.csv")

# Functions used in the plots

se <- function(x) {
  sd(x)/(sqrt(length(x)))
}

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

### Figures in the main text ###


# Figure 4

habitat_foliage_scores <- habitat_foliage_scores %>% arrange(factor(Site, levels = level_order))

fs <- habitat_foliage_scores[,1:12]  # selecting columns with absolute foliage scores

score <- fs %>% pivot_longer(cols=c(Alder_FS, Ash_FS, Aspen_FS, Beech_FS, Birch_FS, Elm_FS, Oak_FS, Sycamore_FS, Willow_FS, Conifer_FS, OthDecid_FS), names_to = "foliage_score")

tree_colours <- c("maroon", "orange", "gold", "lightgreen", "darkgreen", "skyblue", "darkblue", "#CBC3E3", "purple", "violet", "lightpink")

ggplot(score, aes(x=Site, y=value, fill = foliage_score)) +
  geom_col(colour="black") +
  theme_bw() +
  labs(x = "Sites", y = "Foliage score") +
  guides(fill=guide_legend(title="Foliage score")) +
  scale_fill_hue(labels = c("Alder", "Ash", "Aspen", "Beech", "Birch", "Conifer", "Elm", "Oak", "Other deciduous", "Sycamore", "Willow")) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 90, margin = unit(c(1, 0, 0, 0), "mm"), size = 10, vjust = 0.5), 
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")),
        plot.margin = margin(0.7,0.5,0.3,0.5, "cm")) +
  scale_x_discrete(limits = level_order) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 120)) +
  scale_fill_manual(values = tree_colours, labels = c("Alder", "Ash", "Aspen", "Beech", "Birch", "Conifer", "Elm", "Oak", "Other deciduous", "Sycamore", "Willow"))


# Figure 5

blutidf_summary <- blutidf %>%
  group_by(yo) %>%
  na.exclude(fed) %>%
  summarise_at(vars(fed), list(mean=mean, SE=se)) %>% 
  as.data.frame()

plot1 <- ggplot() +
  geom_jitter(data=blutidf, aes(x=yo, y=fed), size=1.8, alpha=0.25, colour = "#248fc9") +
  geom_errorbar(data=blutidf_summary, aes(x=yo, y=mean, ymin=mean-SE, ymax=mean+SE), linewidth = 0.6) +
  geom_point(data=blutidf_summary, aes(x=yo, y=mean), colour = "black", size = 2, alpha = 0.8) +
  labs(x = "Age", y = "First egg lay date (1st Jan = 1)") +
  theme_bw() +
  scale_x_continuous(breaks=seq(1,8,1)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

blutidf_summary <- blutidf %>%
  group_by(yo) %>%
  na.exclude(cs) %>%
  summarise_at(vars(cs), list(mean=mean, SE=se)) %>% 
  as.data.frame()

plot2 <- ggplot() +
  geom_jitter(data=blutidf, aes(x=yo, y=cs), size=1.8, alpha=0.25, colour = "#248fc9") +
  geom_errorbar(data=blutidf_summary, aes(x=yo, y=mean, ymin=mean-SE, ymax=mean+SE), linewidth = 0.6) +
  geom_point(data=blutidf_summary, aes(x=yo, y=mean), colour = "black", size = 2, alpha = 0.8) +
  labs(x = "Age", y = "Clutch size") +
  theme_bw() +
  scale_y_continuous(breaks=seq(1,15,2)) +
  scale_x_continuous(breaks=seq(1,8,1)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

blutidf_summary <- blutidf %>%
  group_by(yo) %>%
  na.exclude(suc) %>%
  summarise_at(vars(suc), list(mean=mean, SE=se)) %>% 
  as.data.frame()

plot3 <- ggplot() +
  geom_jitter(data=blutidf, aes(x=yo, y=suc), size=1.8, alpha=0.25, colour = "#248fc9") +
  geom_errorbar(data=blutidf_summary, aes(x=yo, y=mean, ymin=mean-SE, ymax=mean+SE), linewidth = 0.6) +
  geom_point(data=blutidf_summary, aes(x=yo, y=mean), colour = "black", size = 2, alpha = 0.8) +
  labs(x = "Age", y = "Number of fledgelings") +
  theme_bw() +
  scale_y_continuous(breaks=seq(0,15,2)) +
  scale_x_continuous(breaks=seq(1,8,1)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

blutidf_summary <- blutidf %>%
  group_by(w) %>%
  na.exclude(fed) %>%
  summarise_at(vars(fed), list(mean=mean, SE=se)) %>% 
  as.data.frame()

plot4 <- ggplot() +
  geom_jitter(data=blutidf, aes(x=w, y=fed), size=1.8, alpha=0.25, colour = "#248fc9") +
  geom_errorbar(data=blutidf_summary, aes(x=w, y=mean, ymin=mean-SE, ymax=mean+SE), linewidth = 0.6) +
  geom_point(data=blutidf_summary, aes(x=w, y=mean), colour = "black", size = 2, alpha = 0.8) +
  labs(x = "Age", y = "First egg lay date (1st Jan = 1)") +
  theme_bw() +
  scale_x_continuous(breaks=seq(1,8,1)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

blutidf_summary <- blutidf %>%
  group_by(w) %>%
  na.exclude(cs) %>%
  summarise_at(vars(cs), list(mean=mean, SE=se)) %>% 
  as.data.frame()

plot5 <- ggplot() +
  geom_jitter(data=blutidf, aes(x=w, y=cs), size=1.8, alpha=0.25, colour = "#248fc9") +
  geom_errorbar(data=blutidf_summary, aes(x=w, y=mean, ymin=mean-SE, ymax=mean+SE), linewidth = 0.6) +
  geom_point(data=blutidf_summary, aes(x=w, y=mean), colour = "black", size = 2, alpha = 0.8) +
  labs(x = "Age", y = "Clutch size") +
  theme_bw() +
  scale_y_continuous(breaks=seq(1,15,2)) +
  scale_x_continuous(breaks=seq(1,8,1)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

blutidf_summary <- blutidf %>%
  group_by(w) %>%
  na.exclude(suc) %>%
  summarise_at(vars(suc), list(mean=mean, SE=se)) %>% 
  as.data.frame()

plot6 <- ggplot() +
  geom_jitter(data=blutidf, aes(x=w, y=suc), size=1.8, alpha=0.25, colour = "#248fc9") +
  geom_errorbar(data=blutidf_summary, aes(x=w, y=mean, ymin=mean-SE, ymax=mean+SE), linewidth = 0.6) +
  geom_point(data=blutidf_summary, aes(x=w, y=mean), colour = "black", size = 2, alpha = 0.8) +
  labs(x = "Age", y = "Number of fledgelings") +
  theme_bw() +
  scale_y_continuous(breaks=seq(0,15,2)) +
  scale_x_continuous(breaks=seq(1,8,1)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol = 3, nrow = 2)


# Figure 6

## 1. First egg lay date

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


## 2. Clutch size

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



## 3. Fledgeling success

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
  labs(x = "Age", y = "Observations", title = "Number of fledgelings", subtitle = "Recordings per age group") +
  theme( axis.title = element_text(size = 13), 
         axis.text = element_text(size = 11),
         axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
         axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))


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



# Figure 7

fed_summary <- summarySE(fed_df, measurevar='fed', groupvars='yo')

pred_fed_1 <- plot(ggpredict(fed_model, terms = "yo"), show_title = F, colors = c("#248fc9")) +
  geom_jitter(data=fed_df, aes(x=yo, y=fed), size=1.8, alpha=0.25, colour = "#838282") +
  geom_linerange(data=fed_summary, aes(x=yo, y=fed, ymin=fed-ci, ymax=fed+ci), linewidth = 0.6) +
  geom_point(data=fed_summary, aes(x=yo, y=fed), colour = "black", size = 2, alpha = 1) + 
  labs(x = "Age", y = "First egg lay date (1st Jan = 1)") +
  theme_bw() +
  scale_y_continuous(breaks=seq(100,150,10), limits = c(100,140)) +
  scale_x_continuous(breaks=seq(1,3,1)) +
  theme(axis.title = element_text(size = 12), 
        axis.text = element_text(size = 10),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))  # showing data

fed_summary <- summarySE(fed_df, measurevar='fed', groupvars='w')

pred_fed_2 <- plot(ggpredict(fed_model, terms = "w"), show_title = F, colors = "#248fc9") + 
  geom_jitter(data=fed_df, aes(x=w, y=fed), size=1.8, alpha=0.25, colour = "#838282") +
  geom_linerange(data=fed_summary, aes(x=w, y=fed, ymin=fed-ci, ymax=fed+ci), linewidth = 0.6, colour = "black") +
  geom_point(data=fed_summary, aes(x=w, y=fed), colour = "black", size = 2, alpha = 1) + 
  labs(x = "Age at last reproduction", y = "First egg lay date (1st Jan = 1)") +
  theme_bw() +
  scale_y_continuous(breaks=seq(100,150,10), limits = c(100,140)) +
  scale_x_continuous(breaks=seq(1,8,1)) +
  theme(axis.title = element_text(size = 12), 
        axis.text = element_text(size = 10),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

fed_summary <- summarySE(fed_df, measurevar='fed', groupvars='yo')

pred_fed_4 <- plot(ggpredict(fed_model, terms = c("yo", "total_FS [24.9, 114.5]")), show_data = T, jitter = T, show_title = F, colors = c("red", "#11a821")) +
  labs(x = "Age", y = "First egg lay date (1st Jan = 1)") +
  theme_bw() +
  scale_y_continuous(breaks=seq(90,150,5), limits = c(100, 140)) +
  scale_x_continuous(breaks=seq(1,3,1)) +
  guides(col="none") +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8))  # slight different in slope but, again, it's important to remember that said slopes were NOT significant 

qq <- ggplot_build(pred_fed_4)

qq$data[[1]]$colour <- "#838282"

plot(ggplot_gtable(qq))

pred_fed_5 <- plot(ggpredict(fed_model, terms = c("w", "total_FS [24.9, 114.5]")), show_title = F, show_data = T, jitter = T, colors = c("red", "#11a821")) + # when I don't plot the data, the function "zooms in"
  labs(x = "Age at last reproduction", y = "First egg lay date (1st Jan = 1)") +
  theme_bw() +
  scale_y_continuous(breaks=seq(90,150,10), limits = c(100,140)) +
  scale_x_continuous(breaks=seq(1,8,1)) +
  guides(col="none") +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8))  # this clearly shows that we have a lot of uncertainty as ALR increases

qq <- ggplot_build(pred_fed_5)

qq$data[[1]]$colour <- "#838282"

plot(ggplot_gtable(qq))



# Figure 8

cs_summary <- summarySE(cs_df, measurevar='cs', groupvars='yo')

pred_cs_1 <- plot(ggpredict(cs_model_III, terms = "yo"), show_title = F, colors = "#248fc9") + 
  geom_jitter(data=cs_df, aes(x=yo, y=cs), size=1.8, alpha=0.25, colour = "#838282") +
  geom_linerange(data=cs_summary, aes(x=yo, y=cs, ymin=cs-ci, ymax=cs+ci), linewidth = 0.6, colour = "black") +
  geom_point(data=cs_summary, aes(x=yo, y=cs), colour = "black", size = 2, alpha = 1) + 
  labs(x = "Age", y = "Clutch size") +
  theme_bw() +
  scale_y_continuous(breaks=seq(1,15,2)) +
  scale_x_continuous(breaks=seq(1,3,1)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

cs_summary <- summarySE(cs_df, measurevar='cs', groupvars='w')

pred_cs_2 <- plot(ggpredict(cs_model, terms = "w"), show_title = F, colors = "#248fc9") + 
  geom_jitter(data=cs_df, aes(x=w, y=cs), size=1.8, alpha=0.25, colour = "#838282") +
  geom_linerange(data=cs_summary, aes(x=w, y=cs, ymin=cs-ci, ymax=cs+ci), linewidth = 0.6, colour = "black") +
  geom_point(data=cs_summary, aes(x=w, y=cs), colour = "black", size = 2, alpha = 1) + 
  labs(x = "Age at last reproduction", y = "Clutch size") +
  theme_bw() +
  scale_y_continuous(breaks = seq(1,15,2)) +
  scale_x_continuous(breaks=seq(1,8,1)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

cs_summary <- summarySE(cs_df, measurevar='cs', groupvars='yo')

pred_cs_4 <- plot(ggpredict(cs_model, terms = c("yo", "total_FS [24.925, 114.5156]")), show_title = F, show_data = T, jitter = T, colors = c("red", "#11a821")) + 
  labs(x = "Age", y = "Clutch size") +
  theme_bw() +
  scale_y_continuous(breaks=seq(1,14,1), limits = c(2,13)) +
  scale_x_continuous(breaks=seq(1,3,1)) +
  guides(col="none") +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))  # slight different in slope but, again, it's important to remember that said slopes were NOT significant 

qq <- ggplot_build(pred_cs_4)

qq$data[[1]]$colour <- "#838282"

plot(ggplot_gtable(qq))

cs_summary <- summarySE(cs_df, measurevar='cs', groupvars='w')

pred_cs_5 <- plot(ggpredict(cs_model, terms = c("w", "total_FS [24.925, 114.5156]")), show_title = F, show_data = T, jitter = T, colors = c("red", "#11a821")) + 
  labs(x = "Age at last reproduction", y = "Clutch size") +
  theme_bw() +
  scale_y_continuous(breaks=seq(1,15,2), limits = c(2,13)) +
  scale_x_continuous(breaks=seq(1,8,1)) +
  guides(col="none") +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))  # this clearly shows that we have a lot of uncertainty as ALR increases

qq <- ggplot_build(pred_cs_5)

qq$data[[1]]$colour <- "#838282"

plot(ggplot_gtable(qq))

grid.arrange(pred_cs_1, pred_cs_3, pred_cs_4, pred_cs_5, ncol=2, nrow = 2)



# Figure 9

suc_summary <- summarySE(suc_df, measurevar='suc', groupvars='yo')

pred_suc_1 <- plot(ggpredict(suc_model, terms = "yo"), show_title = F, colors = "#248fc9") + 
  geom_jitter(data=suc_df, aes(x=yo, y=suc), size=1.8, alpha=0.25, colour = "#838282") +
  geom_linerange(data=suc_summary, aes(x=yo, y=suc, ymin=suc-ci, ymax=suc+ci), linewidth = 0.6, colour = "black") +
  geom_point(data=suc_summary, aes(x=yo, y=suc), colour = "black", size = 2, alpha = 1) + 
  labs(x = "Age", y = "Number of fledgelings") +
  theme_bw() +
  scale_y_continuous(breaks = seq(0,15,2)) +
  scale_x_continuous(breaks=seq(1,3,1)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))  # not showing data

suc_summary <- summarySE(suc_df, measurevar='suc', groupvars='w')

pred_suc_2 <- plot(ggpredict(suc_model, terms = "w"), show_title = F, colors = "#248fc9") + 
  geom_jitter(data=suc_df, aes(x=w, y=suc), size=1.8, alpha=0.25, colour = "#838282") +
  geom_linerange(data=suc_summary, aes(x=w, y=suc, ymin=suc-ci, ymax=suc+ci), linewidth = 0.6, colour = "black") +
  geom_point(data=suc_summary, aes(x=w, y=suc), colour = "black", size = 2, alpha = 1) + 
  labs(x = "Age at last reproduction", y = "Number of fledgelings") +
  theme_bw() +
  scale_y_continuous(breaks = seq(0,14,2)) +
  scale_x_continuous(breaks=seq(1,8,1)) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

suc_summary <- summarySE(suc_df, measurevar='suc', groupvars='yo')

pred_suc_4 <- plot(ggpredict(suc_model, terms = c("yo", "total_FS [24.925, 114.5156]")), show_title = F, show_data = T, jitter = T, colors = c("red", "#11a821")) + 
  labs(x = "Age", y = "Number of fledgelings") +
  theme_bw() +
  scale_y_continuous(breaks=seq(0,15,2), limits = c(0,12)) +
  scale_x_continuous(breaks=seq(1,8,1)) +
  guides(col="none") +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

qq <- ggplot_build(pred_suc_4)

qq$data[[1]]$colour <- "#838282"

plot(ggplot_gtable(qq))

suc_summary <- summarySE(suc_df, measurevar='suc', groupvars='w')

pred_suc_5 <- plot(ggpredict(suc_model, terms = c("w", "total_FS [24.925, 114.5156]")), show_title = F, show_data = T, jitter = T, colors = c("red", "#11a821")) + 
  labs(x = "Age at last reproduction", y = "Number of fledgelings") +
  theme_bw() +
  scale_y_continuous(breaks=seq(0,15,2), limits = c(0,12)) +
  scale_x_continuous(breaks=seq(1,8,1)) +
  guides(col="none") +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))

qq <- ggplot_build(pred_suc_5)

qq$data[[1]]$colour <- "#838282"

plot(ggplot_gtable(qq))



# Figure 10

fed_summary <- summarySE(fed_df, measurevar='fed', groupvars='total_FS')

pred_fed_3 <- plot(ggpredict(fed_model, terms = "total_FS"), show_title = F, colors = "#248fc9") + 
  geom_jitter(data=fed_df, aes(x=total_FS, y=fed), size=1.8, alpha=0.25, colour = "#838282") +
  geom_linerange(data=fed_summary, aes(x=total_FS, y=fed, ymin=fed-ci, ymax=fed+ci), linewidth = 0.6, colour = "black") +
  geom_point(data=fed_summary, aes(x=total_FS, y=fed), colour = "black", size = 2, alpha = 1) + 
  labs(x = "Foliage score", y = "First egg lay date (1st Jan = 1)") +
  theme_bw() +
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 11),
        axis.title.x = element_text(margin = unit(c(3, 1, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")),
        plot.margin = margin(0.2,0.8,0.2,0.2, "cm"))

cs_summary <- summarySE(cs_df, measurevar='cs', groupvars='total_FS')

pred_cs_3 <- plot(ggpredict(cs_model, terms = "total_FS"), show_title = F, colors = "#248fc9") + 
  geom_jitter(data=cs_df, aes(x=total_FS, y=cs), size=1.8, alpha=0.25, colour = "#838282") +
  geom_linerange(data=cs_summary, aes(x=total_FS, y=cs, ymin=cs-ci, ymax=cs+ci), linewidth = 0.6, colour = "black") +
  geom_point(data=cs_summary, aes(x=total_FS, y=cs), colour = "black", size = 2, alpha = 1) + 
  labs(x = "Foliage score", y = "Clutch size") +
  theme_bw() +
  scale_y_continuous(breaks=seq(1,15,2)) +
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 11),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")),
        plot.margin = margin(0.2,0.8,0.2,0.2, "cm"))

suc_summary <- summarySE(suc_df, measurevar='suc', groupvars='total_FS')

pred_suc_3 <- plot(ggpredict(suc_model, terms = "total_FS"), show_title = F, colors = "#248fc9") + 
  geom_jitter(data=suc_df, aes(x=total_FS, y=suc), size=1.8, alpha=0.25, colour = "#838282") +
  geom_linerange(data=suc_summary, aes(x=total_FS, y=suc, ymin=suc-ci, ymax=suc+ci), linewidth = 0.6, colour = "black") +
  geom_point(data=suc_summary, aes(x=total_FS, y=suc), colour = "black", size = 2, alpha = 1) + 
  labs(x = "Foliage score", y = "Number of fledgelings") +
  theme_bw() +
  scale_y_continuous(breaks = seq(0,15,2)) +
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 11),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")),
        plot.margin = margin(0.2,0.8,0.2,0.2, "cm"))
