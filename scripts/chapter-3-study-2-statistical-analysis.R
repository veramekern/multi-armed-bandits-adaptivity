# rm(list = ls())
# rm(points.dists)

# coolors
# #797979 silver gray
# #1fc5c3 blue green
# #3b8c84 grey green
# #ff101f red
# #edd83d grey yellow

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
             plot.subtitle=element_text(hjust=0.5, size=10, face="plain", 
                                              family="Gill Sans Nova Light"),
             axis.title=element_text(face="plain", size=10,
                                     family="Gill Sans Nova Light"), 
             strip.background=element_rect(fill="#f9f9f9"),
             axis.text=element_text(color="#bbbbbb"), 
             legend.text=element_text(size=10, face="plain", 
                                      family="Gill Sans Nova Light"),
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

# exclude participants due to recent drug use, psychiatric history
exclude.demo <- read_delim(
  file="../../../03-data/chapter-3-study-2-exclude-demo.csv", delim=",")
dat.full <- left_join(dat.full, exclude.demo, by="id")
dat.full <- subset(dat.full, exclude==0)
length(unique(dat.full$id)) # down to N = 64
dat.full <- subset(dat.full, select=-exclude) # remove exclude variable

# rename version variable
# first letter is drift (h=high, l=low), second letter is noise (h=high, l=low)
dat.full$version <- dplyr::recode(dat.full$version, HRW_LN="hl", HRW_HN="hh", 
                                  LRW_LN="ll", LRW_HN="lh")

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

# find summary data per participant
dat.collect <- dat %>%
  group_by(id, version) %>%
  summarise(n.switches = sum(switch), 
            n.expl.switches = sum(expl.switch),
            cluster.sum = sum(cluster), 
            n.clusters = sum(cluster.start), 
            cluster.size = cluster.sum/n.clusters, 
            performance = max(total), 
            n.correct.arm = sum(best.chosen), 
            p.correct.arm = n.correct.arm/200, 
            mean.rt = mean(rt),
            rt.expl.switch = sum(rt * expl.switch)/n.expl.switches, 
            rt.switch = sum(rt * switch)/n.switches, 
            rt.cluster = sum(rt * cluster)/cluster.sum, 
            rt.start.cluster = sum(rt * cluster.start)/n.clusters) %>%
  mutate(drift = ifelse(version == "ll" | version == "lh", "l", "h"), 
         noise = ifelse(version == "ll" | version == "hl", "l", "h"))

dat.collect.150 <- dat %>%
  filter(trial > 50) %>%
  group_by(id, version) %>%
  summarise(n.switches = sum(switch), 
            n.expl.switches = sum(expl.switch),
            cluster.sum = sum(cluster), 
            n.clusters = sum(cluster.start), 
            cluster.size = cluster.sum/n.clusters, 
            performance = sum(points), 
            n.correct.arm = sum(best.chosen), 
            p.correct.arm = n.correct.arm/150, 
            mean.rt = mean(rt),
            rt.expl.switch = sum(rt * expl.switch)/n.expl.switches, 
            rt.switch = sum(rt * switch)/n.switches, 
            rt.cluster = sum(rt * cluster)/cluster.sum, 
            rt.start.cluster = sum(rt * cluster.start)/n.clusters) %>%
  mutate(drift = ifelse(version == "ll" | version == "lh", "l", "h"), 
         noise = ifelse(version == "ll" | version == "hl", "l", "h"))

dat.collect.100 <- dat %>%
  filter(trial > 100) %>%
  group_by(id, version) %>%
  summarise(n.switches = sum(switch), 
            n.expl.switches = sum(expl.switch),
            cluster.sum = sum(cluster), 
            n.clusters = sum(cluster.start), 
            cluster.size = cluster.sum/n.clusters, 
            performance = sum(points), 
            n.correct.arm = sum(best.chosen), 
            p.correct.arm = n.correct.arm/100, 
            mean.rt = mean(rt),
            rt.expl.switch = sum(rt * expl.switch)/n.expl.switches, 
            rt.switch = sum(rt * switch)/n.switches, 
            rt.cluster = sum(rt * cluster)/cluster.sum, 
            rt.start.cluster = sum(rt * cluster.start)/n.clusters) %>%
  mutate(drift = ifelse(version == "ll" | version == "lh", "l", "h"), 
         noise = ifelse(version == "ll" | version == "hl", "l", "h"))

dat.collect.100$cluster.size <- ifelse(dat.collect.100$cluster.size==Inf, 100, 
                                       dat.collect.100$cluster.size)

table.best <- dat %>%
  group_by(version, trial) %>%
  summarise(best = first(best.possible))
count.best.choice <- table.best %>%
  group_by(version, best) %>%
  summarise(count=n())

count.best.choice <- spread(count.best.choice, best, count)

# calculate distribution variables
# find payoff distributions
payoff.dist <- dat %>%
  group_by(version, trial) %>%
  summarise(arm1 = mean(arm1) + mean(noise), 
            arm2 = mean(arm2) + mean(noise), 
            arm3 = mean(arm3) + mean(noise), 
            arm4 = mean(arm4) + mean(noise))

payoff.dist.noise.sep <- dat %>%
  group_by(version, trial) %>%
  summarise(arm1 = mean(arm1), 
            arm2 = mean(arm2), 
            arm3 = mean(arm3), 
            arm4 = mean(arm4), 
            noise = first(noise))

expected.value <- payoff.dist %>%
  rowwise() %>%
  mutate(mean = mean(unlist(c(arm1, arm2, arm3, arm4))),
         minimum = min(unlist(c(arm1, arm2, arm3, arm4))), 
         maximum = max(unlist(c(arm1, arm2, arm3, arm4)))) %>%
  group_by(version) %>%
  summarise(expected.value = sum(mean), 
            minimum.value = sum(minimum), 
            maximum.value = sum(maximum))

# make long distribution table for graphs
long.payoff.dist <- payoff.dist %>% gather(arm, points, arm1:arm4)

# split per group
dist.ll <- filter(long.payoff.dist, version == 'll')
dist.lh <- filter(long.payoff.dist, version == 'lh')
dist.hl <- filter(long.payoff.dist, version == 'hl')
dist.hh <- filter(long.payoff.dist, version == 'hh')

# ---------- VISUAL EXPLORATION ---------- 

# coolors
# #797979 silver gray
# #1fc5c3 blue green
# #3b8c84 grey green
# #ff101f red
# #edd83d grey yellow

# plot switches per participant to pdf
# split data per id
split.dat <- split(dat, dat$id)
names.dat <- colnames(dat)

#prepare pdf
pdf("../../results/chapter-3-study-2-switches.pdf")

#plot to pdf
for (i in 1:length(unique(dat$id))) {
  temp.dat <- data.frame(split.dat[i])
  names(temp.dat) <- names.dat
  if (temp.dat$version[1]=="ll") {
    print(ggplot(data=dist.ll,
                 aes(x=trial, y=points, colour=arm)) +
            geom_line(size=0.1) + ylim(0, 100) +
            geom_vline(colour="#797979",
                       xintercept=temp.dat$trial[temp.dat$switch==1], size=0.1) +
            ggtitle(label=paste0("Participant ", temp.dat[1,2]), 
                    subtitle=paste0("Version ", temp.dat[1,1])) + 
            xlab("Points earned on trial") + 
            ylab("Trial number") +
            scale_color_manual(values=c("#1fc5c3", "#3b8c84", 
                                        "#ff101f", "#edd83d"))
    )
  }
  else if (temp.dat$version[1]=="lh") {
    print(ggplot(data=dist.lh,
                 aes(x=trial, y=points, colour=arm)) +
            geom_line(size=0.1) + ylim(0, 100) +
            geom_vline(colour="#797979",
                       xintercept=temp.dat$trial[temp.dat$switch==1], size=0.1) +
            ggtitle(label=paste0("Participant ", temp.dat[1,2]), 
                    subtitle=paste0("Version ", temp.dat[1,1])) + 
            xlab("Points earned on trial") + 
            ylab("Trial number") +
            scale_color_manual(values=c("#1fc5c3", "#3b8c84", 
                                        "#ff101f", "#edd83d"))
    )
  }
  else if (temp.dat$version[1]=="hl") {
    print(ggplot(data=dist.hl,
                 aes(x=trial, y=points, colour=arm)) +
            geom_line(size=0.1) + ylim(0, 100) +
            geom_vline(colour="#797979",
                       xintercept=temp.dat$trial[temp.dat$switch==1], size=0.1) +
            ggtitle(label=paste0("Participant ", temp.dat[1,2]), 
                    subtitle=paste0("Version ", temp.dat[1,1])) + 
            xlab("Points earned on trial") + 
            ylab("Trial number") +
            scale_color_manual(values=c("#1fc5c3", "#3b8c84", 
                                        "#ff101f", "#edd83d"))
    )
  }
  else {
    print(ggplot(data=dist.hh,
                 aes(x=trial, y=points, colour=arm)) +
            geom_line(size=0.1) + ylim(0, 100) +
            geom_vline(colour="#797979",
                       xintercept=temp.dat$trial[temp.dat$switch==1], size=0.1) +
            ggtitle(label=paste0("Participant ", temp.dat[1,2]), 
                    subtitle=paste0("Version ", temp.dat[1,1])) + 
            xlab("Points earned on trial") + 
            ylab("Trial number") +
            scale_color_manual(values=c("#1fc5c3", "#3b8c84", 
                                        "#ff101f", "#edd83d"))
    )
  }
}
dev.off()


print(ggplot(data=dist.ll,
             aes(x=trial, y=points, colour=arm)) +
        geom_line(size=0.1) + ylim(0, 100) +
        geom_vline(colour="#797979",
                   xintercept=temp.dat$trial[temp.dat$switch==1], size=0.1) +
        ggtitle(label=paste0("Participant ", temp.dat[1,2]), 
                subtitle=paste0("Version ", temp.dat[1,1])) + 
        xlab("Points earned on trial") + 
        ylab("Trial number") +
        scale_color_manual(values=c("#1fc5c3", "#3b8c84", 
                                    "#ff101f", "#edd83d"))
)
