# rm(list = ls())
# rm(points.dists)

# ---------- INIT ---------- 
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = T)
  sapply(pkg, require, character.only = T)
}
packages <- c("tidyverse", "plotly", "ggplot2", "gridExtra", "matrixStats", 
              "ggthemes", "psych", "biotools", "MASS", "lattice", "GGally", 
              "extrafont", "car")
ipak(packages)
font_import()
loadfonts(device = "win")
windowsFonts()

# set default ggplot title to be adjusted to center
theme_update(plot.title=element_text(hjust = 0.5, size=12, face="plain", 
                                     family="Gill Sans Nova Light"),
             axis.title=element_text(face="plain", size=10,
                                     family="Gill Sans Nova Light"), 
             strip.background=element_rect(fill="#f9f9f9", ),
             axis.text=element_text(color="#bbbbbb"), 
             panel.background=element_rect(fill="#ffffff"), 
             panel.grid.minor=element_line(color="#dddddd"))

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
dat.full <- read_csv(file="../../../03-data/chapter-3-study-2-full-data.csv")

# # exclude participants due to recent drug use, psychiatric history, non-nativity
# exclude.demo <- read_delim(
#   file="../../../03-data/chapter-3-study-1-exclude-demo.csv", delim=";")
# dat.full <- left_join(dat.eprime, exclude.demo, by="id")
# dat.full <- subset(dat.full, exclude==0)
# length(unique(dat.full$id)) # down to N = 66
# dat.full <- dat.full[complete.cases(dat.full),] # remove practice trials
# dat.full <- subset(dat.full, select=-exclude) # remove exclude variable
# write_csv(dat.full, path="../../../03-data/chapter-3-study-1-full-data.csv")