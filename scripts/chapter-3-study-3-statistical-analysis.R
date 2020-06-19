# rm(list = ls())
# rm(points.dists)

# ---------- INIT ---------- 
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = T)
  sapply(pkg, require, character.only = T)
}
packages <- c("dplyr", "psych", "ggplot2", "car", "MASS", "nlme", "tidyr", 
              "Hmisc", "stringr", "ez", "multcomp", "gridExtra", "doBy", "FSA", 
              "randtests", "gridExtra", "reshape2", "tibble", "DescTools", 
              "ggridges", "hrbrthemes", "plotly", "ggthemes")
ipak(packages)
warnings()