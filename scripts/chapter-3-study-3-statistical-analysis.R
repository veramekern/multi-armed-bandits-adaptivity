# rm(list = ls())
# rm(points.dists)

# colors
#  #797979   silver gray   base color
#  #1fc5c3   blue green    group color
#  #3b8c84   grey green    group color
#  #ff101f   red           group color
#  #edd83d   grey yellow   group color
#  #FFC2B4   orange pink   highlighter

# ---------- INIT ---------- 

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = T)
  sapply(pkg, require, character.only = T)
}
packages <- c("tidyverse", "plotly", "ggplot2", "gridExtra", "matrixStats", 
              "ggthemes", "psych", "biotools", "MASS", "lattice", "GGally", 
              "extrafont", "car", "Rfit", "rmarkdown", "tinytex")
ipak(packages)
font_import()
loadfonts(device = "win")
windowsFonts()

# set default ggplot title to be adjusted to center
gill.sans <- theme_update(plot.title=
                            element_text(hjust = 0.5, vjust=3, size=14, 
                                         family="Gill Sans Nova"),
                          plot.subtitle=
                            element_text(hjust=0.5, vjust=3, size=12, 
                                         family="Gill Sans Nova Light"),
                          plot.caption=
                            element_text(hjust=0, size=12, color="#bbbbbb",
                                         family="Gill Sans Nova Light", vjust=-1), 
                          axis.title=element_text(face="plain", size=12,
                                                  family="Gill Sans Nova Light"), 
                          axis.title.x=element_text(vjust=0),
                          strip.background=element_rect(fill="#f9f9f9"),
                          axis.text=element_text(color="#bbbbbb"), 
                          legend.text=
                            element_text(size=12, family="Gill Sans Nova Light"),
                          panel.background=
                            element_rect(fill="#ffffff", colour="#ffffff"), 
                          axis.line=element_line(color="#bbbbbb", size=0.5),
                          panel.grid.minor=element_line(color="#dddddd"), 
                          panel.grid.major=element_line(color="#dddddd"), 
                          plot.margin=margin(20,12,12,30))


# ---------- LOAD DATA ---------- 
# # TO LOAD DATA FROM RAW DATA FILE
# dat.eprime <- read_delim(
#   file="../../../03-data/chapter-3-study-2-raw-data-eprime.csv", delim = ";")
# dat.eprime <- subset(dat.eprime, select=c("ExperimentName", "Subject",
#                                           "OverallGain2[Block]", "ExpBlock",
#                                           "chose_hist", "keuze.RT", "noise",
#                                           "payoff1", "payoff2", "payoff3",
#                                           "payoff4", "pts_hist"))
# colnames(dat.eprime) <- c("version", "id", "total", "trial", "choice", "rt",
#                           "noise", "arm1", "arm2", "arm3", "arm4", "points")
# dat.eprime$version <- recode(dat.eprime$version, GokTaak_VersieA = "a")
# dat.eprime$version <- recode(dat.eprime$version, GokTaak_VersieB = "b")
# 


# TO LOAD FROM CLEANED FILE (EXCLUDED SOME PARTICIPANTS based on demographics)
dat.full <- read_delim(file="../../../03-data/chapter-3-study-3-raw-data.csv", 
                       delim=",")
# remove row numbers
dat.full <- subset(dat.full, select=-nr)

# exclude participants due to recent drug use, psychiatric history
exclude.demo <- read_delim(
  file="../../../03-data/chapter-3-study-3-exclude-demo.csv", delim=",")
dat.full <- left_join(dat.full, exclude.demo, by="id")
dat.full <- subset(dat.full, exclude==0)
length(unique(dat.full$id)) # down to N = 62
dat.full <- subset(dat.full, select=-exclude) # remove exclude variable

# rename version variable
# first letter is drift (h=high, l=low), second letter is noise (h=high, l=low)
dat.full$version <- dplyr::recode(dat.full$version, S_HRW_LN="shl", 
                                  S_LRW_HN="slh")

# Rename currently chosen data to 'dat'
dat <- dat.full
rm(dat.full)

# ---------- CALCULATE VARIABLES FOR BEHAVIORAL ANALYSES ---------- 
# calculate clusters
dat <- dat %>%
  group_by(id) %>%
  mutate(cluster = ifelse(choice == lag(choice), 1, 0)) %>%
  mutate(cluster = ifelse(lead(cluster) == 1 & cluster == 0, 1, cluster)) %>%
  mutate(cluster = ifelse(is.na(cluster) & lead(choice)==choice, 1, cluster)) %>%
  mutate(cluster = ifelse(is.na(cluster), 0, cluster))

# calculate cluster starts
dat <- dat %>%
  group_by(id) %>%
  mutate(cluster.start = ifelse(cluster == 1 & lag(choice) != choice, 1, 0)) %>%
  mutate(cluster.start = ifelse(cluster == 1 & is.na(lag(cluster)), 1, 
                                cluster.start))

# calculate switches
dat <- dat %>%
  group_by(id) %>%
  mutate(switch = ifelse(choice != lag(choice), 1, 0)) %>%
  mutate(switch = ifelse(is.na(switch), 0, switch))

# calculate exploratory switches
dat <- dat %>%
  group_by(id) %>%
  mutate(expl.switch = ifelse(choice != lag(choice) & 
                                choice != lag(choice, n=2), 1, 0)) %>%
  mutate(expl.switch = ifelse(is.na(expl.switch), 0, expl.switch))

# find best choice
dat$best.possible <- max.col(dat[,5:8])
dat <- dat %>%
  mutate(best.chosen = ifelse(best.possible == choice, 1, 0))

# find n best arm switch
dat <- dat %>%
  mutate(best.switched = ifelse(best.possible != lag(best.possible), 1, 0))
dat$best.switched <- ifelse(is.na(dat$best.switched)==T, 0, dat$best.switched)
