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
              "extrafont", "car", "Rfit", "rmarkdown", "tinytex", "zoo", 
              "splines")
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
  mutate(cluster = ifelse(choice == lag(choice), 1, 0), 
         cluster = ifelse(lead(cluster) == 1 & cluster == 0, 1, cluster), 
         cluster = ifelse(is.na(cluster) & lead(choice)==choice, 1, cluster), 
         cluster = ifelse(is.na(cluster), 0, cluster),
         cluster.start = ifelse(cluster == 1 & lag(choice) != choice, 1, 0),
         cluster.start = ifelse(cluster == 1 & is.na(lag(cluster)), 1, 
                                cluster.start), 
         switch = ifelse(choice != lag(choice), 1, 0), 
         switch = ifelse(is.na(switch), 0, switch), 
         expl.switch = ifelse(choice != lag(choice) & 
                                choice != lag(choice, n=2), 1, 0), 
         expl.switch = ifelse(is.na(expl.switch), 0, expl.switch), 
         best.possible = pmax(arm1, arm2, arm3, arm4),
         best.possible = ifelse(best.possible == arm1, 0, 
                                ifelse(best.possible == arm2, 1, 
                                       ifelse(best.possible == arm3, 2, 3))),
         best.chosen = ifelse(best.possible == choice, 1, 0), 
         best.switched = ifelse(best.possible != lag(best.possible), 1, 0),
         best.switched = ifelse(is.na(best.switched)==T, 0, best.switched),
         block.nr = ifelse(between(trial, 1, 100), 1, 
                           ifelse(between(trial, 101, 200), 2, 
                                  ifelse(between(trial, 201, 300), 3, 4))), 
         block.hl = ifelse((block.nr==1 | block.nr==3) & version=="shl", 1, 
                           ifelse((block.nr==2 | block.nr==4) & version=="slh", 1, 
                                  0)), 
         block.lh = ifelse((block.nr==1 | block.nr==3) & version=="slh", 1, 
                           ifelse((block.nr==2 | block.nr==4) & version=="shl", 1, 
                                  0)), 
         blockhl.nr = ifelse(block.hl==1 & block.nr<3, 1, 
                             ifelse(block.hl==1 & block.nr>2, 2, 0)), 
         block.lh.nr = ifelse(block.lh==1 & block.nr<3, 1, 
                              ifelse(block.lh==1 & block.nr>2, 2, 0))) %>%
  group_by(id, choice) %>%
  mutate(p.arm1 = ifelse(choice==0, row_number(), NA), 
         p.arm2 = ifelse(choice==1, row_number(), NA), 
         p.arm3 = ifelse(choice==2, row_number(), NA), 
         p.arm4 = ifelse(choice==3, row_number(), NA))%>%
  group_by(id) %>%
  mutate(p.arm1 = na.locf(p.arm1, na.rm=F), 
         p.arm2 = na.locf(p.arm2, na.rm=F), 
         p.arm3 = na.locf(p.arm3, na.rm=F), 
         p.arm4 = na.locf(p.arm4, na.rm=F), 
         p.arm1 = p.arm1/trial, 
         p.arm2 = p.arm2/trial, 
         p.arm3 = p.arm3/trial, 
         p.arm4 = p.arm4/trial, 
         p.arm1 = ifelse(is.na(p.arm1)==T, 0, p.arm1), 
         p.arm2 = ifelse(is.na(p.arm2)==T, 0, p.arm2), 
         p.arm3 = ifelse(is.na(p.arm3)==T, 0, p.arm3), 
         p.arm4 = ifelse(is.na(p.arm4)==T, 0, p.arm4), 
         entropy = -1 * ((p.arm1 * log2(p.arm1)) + (p.arm2 * log2(p.arm2)) + 
                           (p.arm3 * log2(p.arm3)) + (p.arm4 * log2(p.arm4))), 
         entropy = ifelse(is.nan(entropy)==T, 2, entropy))

# ---------- CALCULATE SUMMARY DATA FOR BEHAVIORAL ANALYSES ALL TRIALS ---------- 
# for all trials, over all blocks
dat.collect <- dat %>%
  group_by(id, version) %>%
  summarise(n.switches = sum(switch), 
            n.expl.switches = sum(expl.switch),
            cluster.sum = sum(cluster), 
            n.clusters = sum(cluster.start), 
            cluster.size = cluster.sum/n.clusters, 
            performance = max(total), 
            n.correct.arm = sum(best.chosen), 
            p.correct.arm = n.correct.arm/length(trial), 
            mean.rt = mean(rt),
            rt.expl.switch = sum(rt * expl.switch)/n.expl.switches, 
            rt.switch = sum(rt * switch)/n.switches, 
            rt.cluster = sum(rt * cluster)/cluster.sum, 
            rt.start.cluster = sum(rt * cluster.start)/n.clusters,
            n.best.switch = sum(best.switched)) 

# find summary data per participant
# for high drift/low noise blocks
dat.collect.hl <- dat %>%
  filter(block.hl==1) %>%
  group_by(id, version) %>%
  summarise(n.switches = sum(switch), 
            n.expl.switches = sum(expl.switch),
            cluster.sum = sum(cluster), 
            n.clusters = sum(cluster.start), 
            cluster.size = cluster.sum/n.clusters, 
            performance = sum(points), 
            n.correct.arm = sum(best.chosen), 
            p.correct.arm = n.correct.arm/length(trial), 
            mean.rt = mean(rt),
            rt.expl.switch = sum(rt * expl.switch)/n.expl.switches, 
            rt.switch = sum(rt * switch)/n.switches, 
            rt.cluster = sum(rt * cluster)/cluster.sum, 
            rt.start.cluster = sum(rt * cluster.start)/n.clusters,
            n.best.switch = sum(best.switched)) 

# for the first high drift/low noise blocks 
# (first block for some, second for the rest)
dat.collect.hl.first <- dat %>%
  filter(block.hl.nr==1) %>%
  group_by(id, version) %>%
  summarise(n.switches = sum(switch), 
            n.expl.switches = sum(expl.switch),
            cluster.sum = sum(cluster), 
            n.clusters = sum(cluster.start), 
            cluster.size = cluster.sum/n.clusters, 
            performance = sum(points), 
            n.correct.arm = sum(best.chosen), 
            p.correct.arm = n.correct.arm/length(trial), 
            mean.rt = mean(rt),
            rt.expl.switch = sum(rt * expl.switch)/n.expl.switches, 
            rt.switch = sum(rt * switch)/n.switches, 
            rt.cluster = sum(rt * cluster)/cluster.sum, 
            rt.start.cluster = sum(rt * cluster.start)/n.clusters,
            n.best.switch = sum(best.switched)) 

# for the second high drift/low noise blocks 
# (third block for some, fourth for the rest)
dat.collect.hl.second <- dat %>%
  filter(block.hl.nr==2) %>%
  group_by(id, version) %>%
  summarise(n.switches = sum(switch), 
            n.expl.switches = sum(expl.switch),
            cluster.sum = sum(cluster), 
            n.clusters = sum(cluster.start), 
            cluster.size = cluster.sum/n.clusters, 
            performance = sum(points), 
            n.correct.arm = sum(best.chosen), 
            p.correct.arm = n.correct.arm/length(trial), 
            mean.rt = mean(rt),
            rt.expl.switch = sum(rt * expl.switch)/n.expl.switches, 
            rt.switch = sum(rt * switch)/n.switches, 
            rt.cluster = sum(rt * cluster)/cluster.sum, 
            rt.start.cluster = sum(rt * cluster.start)/n.clusters,
            n.best.switch = sum(best.switched)) 

# for the first low drift/high noise blocks 
dat.collect.lh <- dat %>%
  filter(block.lh==1) %>%
  group_by(id, version) %>%
  summarise(n.switches = sum(switch), 
            n.expl.switches = sum(expl.switch),
            cluster.sum = sum(cluster), 
            n.clusters = sum(cluster.start), 
            cluster.size = cluster.sum/n.clusters, 
            performance = sum(points), 
            n.correct.arm = sum(best.chosen), 
            p.correct.arm = n.correct.arm/length(trial), 
            mean.rt = mean(rt),
            rt.expl.switch = sum(rt * expl.switch)/n.expl.switches, 
            rt.switch = sum(rt * switch)/n.switches, 
            rt.cluster = sum(rt * cluster)/cluster.sum, 
            rt.start.cluster = sum(rt * cluster.start)/n.clusters,
            n.best.switch = sum(best.switched)) 

# for the first low drift/high noise blocks 
# (first block for some, second for the rest)
dat.collect.lh.first <- dat %>%
  filter(block.lh.nr==1) %>%
  group_by(id, version) %>%
  summarise(n.switches = sum(switch), 
            n.expl.switches = sum(expl.switch),
            cluster.sum = sum(cluster), 
            n.clusters = sum(cluster.start), 
            cluster.size = cluster.sum/n.clusters, 
            performance = sum(points), 
            n.correct.arm = sum(best.chosen), 
            p.correct.arm = n.correct.arm/length(trial), 
            mean.rt = mean(rt),
            rt.expl.switch = sum(rt * expl.switch)/n.expl.switches, 
            rt.switch = sum(rt * switch)/n.switches, 
            rt.cluster = sum(rt * cluster)/cluster.sum, 
            rt.start.cluster = sum(rt * cluster.start)/n.clusters,
            n.best.switch = sum(best.switched)) 

# for the first low drift/high noise blocks 
# (third block for some, fourth for the rest)
dat.collect.lh.second <- dat %>%
  filter(block.lh.nr==2) %>%
  group_by(id, version) %>%
  summarise(n.switches = sum(switch), 
            n.expl.switches = sum(expl.switch),
            cluster.sum = sum(cluster), 
            n.clusters = sum(cluster.start), 
            cluster.size = cluster.sum/n.clusters, 
            performance = sum(points), 
            n.correct.arm = sum(best.chosen), 
            p.correct.arm = n.correct.arm/length(trial), 
            mean.rt = mean(rt),
            rt.expl.switch = sum(rt * expl.switch)/n.expl.switches, 
            rt.switch = sum(rt * switch)/n.switches, 
            rt.cluster = sum(rt * cluster)/cluster.sum, 
            rt.start.cluster = sum(rt * cluster.start)/n.clusters,
            n.best.switch = sum(best.switched)) 

# --------- CALCULATE SUMMARY DATA FOR BEHAVIORAL ANALYSES TRUNC TRIALS --------- 
# for all trials, over all blocks
# truncate data to remove first 50 trials for each block
dat.trunc <- dat %>%
  filter(between(trial, 51, 100) | between(trial, 151, 200) |
           between(trial, 251, 300) | between(trial, 351, 400))

# for all trials minus the first 50, over all blocks
dat.collect.trunc <- dat.trunc %>%
  group_by(id, version) %>%
  summarise(n.switches = sum(switch), 
            n.expl.switches = sum(expl.switch),
            cluster.sum = sum(cluster), 
            n.clusters = sum(cluster.start), 
            cluster.size = cluster.sum/n.clusters, 
            performance = max(total), 
            n.correct.arm = sum(best.chosen), 
            p.correct.arm = n.correct.arm/length(trial), 
            mean.rt = mean(rt),
            rt.expl.switch = sum(rt * expl.switch)/n.expl.switches, 
            rt.switch = sum(rt * switch)/n.switches, 
            rt.cluster = sum(rt * cluster)/cluster.sum, 
            rt.start.cluster = sum(rt * cluster.start)/n.clusters,
            n.best.switch = sum(best.switched)) 

# find summary data per participant
# for high drift/low noise blocks
dat.collect.hl.trunc <- dat.trunc %>%
  filter(block.hl==1) %>%
  group_by(id, version) %>%
  summarise(n.switches = sum(switch), 
            n.expl.switches = sum(expl.switch),
            cluster.sum = sum(cluster), 
            n.clusters = sum(cluster.start), 
            cluster.size = cluster.sum/n.clusters, 
            performance = sum(points), 
            n.correct.arm = sum(best.chosen), 
            p.correct.arm = n.correct.arm/length(trial), 
            mean.rt = mean(rt),
            rt.expl.switch = sum(rt * expl.switch)/n.expl.switches, 
            rt.switch = sum(rt * switch)/n.switches, 
            rt.cluster = sum(rt * cluster)/cluster.sum, 
            rt.start.cluster = sum(rt * cluster.start)/n.clusters,
            n.best.switch = sum(best.switched)) 

# for the first high drift/low noise blocks 
# (first block for some, second for the rest)
dat.collect.hl.first.trunc <- dat.trunc %>%
  filter(block.hl.nr==1) %>%
  group_by(id, version) %>%
  summarise(n.switches = sum(switch), 
            n.expl.switches = sum(expl.switch),
            cluster.sum = sum(cluster), 
            n.clusters = sum(cluster.start), 
            cluster.size = cluster.sum/n.clusters, 
            performance = sum(points), 
            n.correct.arm = sum(best.chosen), 
            p.correct.arm = n.correct.arm/length(trial), 
            mean.rt = mean(rt),
            rt.expl.switch = sum(rt * expl.switch)/n.expl.switches, 
            rt.switch = sum(rt * switch)/n.switches, 
            rt.cluster = sum(rt * cluster)/cluster.sum, 
            rt.start.cluster = sum(rt * cluster.start)/n.clusters,
            n.best.switch = sum(best.switched)) 

# for the second high drift/low noise blocks 
# (third block for some, fourth for the rest)
dat.collect.hl.second.trunc <- dat.trunc %>%
  filter(block.hl.nr==2) %>%
  group_by(id, version) %>%
  summarise(n.switches = sum(switch), 
            n.expl.switches = sum(expl.switch),
            cluster.sum = sum(cluster), 
            n.clusters = sum(cluster.start), 
            cluster.size = cluster.sum/n.clusters, 
            performance = sum(points), 
            n.correct.arm = sum(best.chosen), 
            p.correct.arm = n.correct.arm/length(trial), 
            mean.rt = mean(rt),
            rt.expl.switch = sum(rt * expl.switch)/n.expl.switches, 
            rt.switch = sum(rt * switch)/n.switches, 
            rt.cluster = sum(rt * cluster)/cluster.sum, 
            rt.start.cluster = sum(rt * cluster.start)/n.clusters,
            n.best.switch = sum(best.switched)) 

# for the first low drift/high noise blocks 
dat.collect.lh.trunc <- dat.trunc %>%
  filter(block.lh==1) %>%
  group_by(id, version) %>%
  summarise(n.switches = sum(switch), 
            n.expl.switches = sum(expl.switch),
            cluster.sum = sum(cluster), 
            n.clusters = sum(cluster.start), 
            cluster.size = cluster.sum/n.clusters, 
            performance = sum(points), 
            n.correct.arm = sum(best.chosen), 
            p.correct.arm = n.correct.arm/length(trial), 
            mean.rt = mean(rt),
            rt.expl.switch = sum(rt * expl.switch)/n.expl.switches, 
            rt.switch = sum(rt * switch)/n.switches, 
            rt.cluster = sum(rt * cluster)/cluster.sum, 
            rt.start.cluster = sum(rt * cluster.start)/n.clusters,
            n.best.switch = sum(best.switched)) 

# for the first low drift/high noise blocks 
# (first block for some, second for the rest)
dat.collect.lh.first.trunc <- dat.trunc %>%
  filter(block.lh.nr==1) %>%
  group_by(id, version) %>%
  summarise(n.switches = sum(switch), 
            n.expl.switches = sum(expl.switch),
            cluster.sum = sum(cluster), 
            n.clusters = sum(cluster.start), 
            cluster.size = cluster.sum/n.clusters, 
            performance = sum(points), 
            n.correct.arm = sum(best.chosen), 
            p.correct.arm = n.correct.arm/length(trial), 
            mean.rt = mean(rt),
            rt.expl.switch = sum(rt * expl.switch)/n.expl.switches, 
            rt.switch = sum(rt * switch)/n.switches, 
            rt.cluster = sum(rt * cluster)/cluster.sum, 
            rt.start.cluster = sum(rt * cluster.start)/n.clusters,
            n.best.switch = sum(best.switched)) 

# for the first low drift/high noise blocks 
# (third block for some, fourth for the rest)
dat.collect.lh.second.trunc <- dat.trunc %>%
  filter(block.lh.nr==2) %>%
  group_by(id, version) %>%
  summarise(n.switches = sum(switch), 
            n.expl.switches = sum(expl.switch),
            cluster.sum = sum(cluster), 
            n.clusters = sum(cluster.start), 
            cluster.size = cluster.sum/n.clusters, 
            performance = sum(points), 
            n.correct.arm = sum(best.chosen), 
            p.correct.arm = n.correct.arm/length(trial), 
            mean.rt = mean(rt),
            rt.expl.switch = sum(rt * expl.switch)/n.expl.switches, 
            rt.switch = sum(rt * switch)/n.switches, 
            rt.cluster = sum(rt * cluster)/cluster.sum, 
            rt.start.cluster = sum(rt * cluster.start)/n.clusters,
            n.best.switch = sum(best.switched)) 

# ---------- CALCULATE SUMMARY DATA TABLES ---------- 
# calculate n arm was best arm per version
table.best <- dat %>%
  group_by(version, trial) %>%
  summarise(best = first(best.possible))
count.best.choice <- table.best %>%
  group_by(version, best) %>%
  summarise(count=n())

count.best.choice <- spread(count.best.choice, best, count)

# find n best arm switches per version
table.best.switch.version <- dat.collect %>%
  group_by(version) %>%
  summarise(n=mean(n.best.switch))

# find n per version
table.n.version <- dat.collect %>%
  group_by(version) %>%
  summarise(n=length(unique(id)))

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
dist.slh <- filter(long.payoff.dist, version == 'slh')
dist.shl <- filter(long.payoff.dist, version == 'shl')


# coolors
# #797979 silver gray
# #1fc5c3 blue green
# #3b8c84 grey green
# #ff101f red
# #edd83d grey yellow

# ---------- VISUAL EXPLORATION PAYOFF ---------- 
# plot payoff distributions per version
plot.slh <- ggplot(data=dist.slh, aes(x=trial, y=points, colour=arm)) +
  geom_line(size=0.5) + ylim(0, 100) +
  labs(title="Point distribution per arm", 
       subtitle="Version: Start Low Drift/High Noise",
       y="Points Earned on Trial", x="Trial Number", color="Arm") +
  scale_color_manual(values=c("#1fc5c3", "#3b8c84", "#ff101f", "#edd83d"), 
                     labels=c("Arm 1", "Arm 2", "Arm 3", "Arm 4"))

plot.shl <- ggplot(data=dist.shl, aes(x=trial, y=points, colour=arm)) +
  geom_line(size=0.5) + ylim(0, 100) +
  labs(title="Point distribution per arm", 
       subtitle="Version: Start High Drift/Low Noise",
       y="Points Earned on Trial", x="Trial Number", color="Arm") +
  scale_color_manual(values=c("#1fc5c3", "#3b8c84", "#ff101f", "#edd83d"), 
                     labels=c("Arm 1", "Arm 2", "Arm 3", "Arm 4"))

# ---------- VISUAL EXPLORATION PDFs ---------- 
# split data per id
split.dat <- split(dat, dat$id)
names.dat <- colnames(dat)

# plot switches per participant to pdf
# plot to pdfs to combine w/ acrobat pro
for (i in 1:length(unique(dat$id))) {
  temp.dat <- data.frame(split.dat[i])
  names(temp.dat) <- names.dat
  if (temp.dat$version[1]=="slh") {
    temp_plot <- ggplot(data=dist.slh, aes(x=trial, y=points, colour=arm)) +
      geom_line(size=0.1) + ylim(0, 100) +
      geom_vline(colour="#FFC2B4", xintercept=temp.dat$trial[temp.dat$switch==1], 
                 size=0.3, alpha=0.5) +
      ggtitle(label=paste0("Participant ", temp.dat[1,3]), 
              subtitle=paste0("Version ", temp.dat[1,1])) + 
      xlab("Trial number") + 
      ylab("Points earned on trial") +
      scale_color_manual(values=c("#1fc5c3", "#3b8c84", "#ff101f", "#edd83d"))
    ggsave(temp_plot, 
           file=paste0("../../results/chapter-3-study-3-switches-", i,".pdf"), 
           width = 42, height = 21, units = "cm", dpi=150, device=cairo_pdf)
  }
  else {
    temp_plot <- ggplot(data=dist.shl, aes(x=trial, y=points, colour=arm)) +
      geom_line(size=0.1) + ylim(0, 100) +
      geom_vline(colour="#FFC2B4", xintercept=temp.dat$trial[temp.dat$switch==1], 
                 size=0.3, alpha=0.5) +
      ggtitle(label=paste0("Participant ", temp.dat[1,3]), 
              subtitle=paste0("Version ", temp.dat[1,1])) + 
      xlab("Trial number") + 
      ylab("Points earned on trial") +
      scale_color_manual(values=c("#1fc5c3", "#3b8c84", "#ff101f", "#edd83d"))
    ggsave(temp_plot, 
           file=paste0("../../results/chapter-3-study-3-switches-", i,".pdf"), 
           width = 42, height = 21, units = "cm", dpi=150, device=cairo_pdf)
  }
}

# density plot for switches per version
ggplot(dat.collect, aes(x=n.switches, fill=version)) +
  geom_density(alpha=0.6) + 
  scale_fill_discrete(name = "Version", labels = c("Start high drift/\nlow noise", 
                                                   "Start low drift/\nhigh noise"))

# plot best choice or not per participant to pdf
# separate pdfs to be combined in acrobat pro
for (i in 1:length(unique(dat$id))) {
  temp.dat <- data.frame(split.dat[i])
  names(temp.dat) <- names.dat
  if (temp.dat$version[1]=="slh") {
    temp_plot <- ggplot(data=dist.slh, aes(x=trial, y=points, colour=arm)) +
      geom_line(size=0.1) + ylim(0, 100) +
      geom_vline(colour="#FFC2B4", 
                 xintercept=temp.dat$trial[temp.dat$best.chosen==1], 
                 size=0.1, alpha=0.5) + 
      ggtitle(label=paste0("Participant ", temp.dat[1,3]), 
              subtitle=paste0("Version ", temp.dat[1,1])) + 
      xlab("Trial number") + 
      ylab("Points earned on trial") +
      scale_color_manual(values=c("#1fc5c3", "#3b8c84", "#ff101f", "#edd83d")) + 
      labs(caption="vertical lines indicate arm chosen was best arm")
    ggsave(temp_plot, 
           file=paste0("../../results/chapter-3-study-3-best.best-chosen-", 
                       i,".pdf"), 
           width = 42, height = 21, units = "cm", dpi=150, device=cairo_pdf)
  }
  else {
    temp_plot <- ggplot(data=dist.shl, aes(x=trial, y=points, colour=arm)) +
      geom_line(size=0.1) + ylim(0, 100) +
      geom_vline(colour="#FFC2B4", 
                 xintercept=temp.dat$trial[temp.dat$best.chosen==1], 
                 size=0.1, alpha=0.5) +
      ggtitle(label=paste0("Participant ", temp.dat[1,3]), 
              subtitle=paste0("Version ", temp.dat[1,1])) + 
      xlab("Trial number") + 
      ylab("Points earned on trial") +
      scale_color_manual(values=c("#1fc5c3", "#3b8c84", "#ff101f", "#edd83d")) +
      labs(caption="vertical lines indicate arm chosen was best arm")
    ggsave(temp_plot, 
           file=paste0("../../results/chapter-3-study-3-best.best-chosen-", 
                       i,".pdf"), 
           width = 42, height = 21, units = "cm", dpi=150, device=cairo_pdf)
  }
}

# entropy per participant
for (i in 1:length(unique(dat$id))) {
  temp.dat <- data.frame(split.dat[i])
  names(temp.dat) <- names.dat
  temp_plot <- ggplot(data=temp.dat, aes(x=trial, y=entropy)) +
    geom_line(colour="#1fc5c3") + 
    geom_vline(colour="#999999", 
               xintercept=temp.dat$trial[temp.dat$best.chosen==1], 
               size=1, alpha=0.5) +
    geom_vline(colour="#FFC2B4", xintercept=c(100, 200, 300, 400), 
               size=1, alpha=0.5) +
    ggtitle(label=paste0("Participant ", temp.dat[1,3]), 
            subtitle=paste0("Version ", temp.dat[1,1])) + 
    xlab("Trial number") + 
    ylab("Entropy") +
    labs(caption="vertical gray lines indicate arm chosen was best arm\n
         vertical red lines indicate change of environment")
  ggsave(temp_plot, 
         file=paste0("../../results/chapter-3-study-3-entropy-", i,".pdf"), 
         width = 42, height = 21, units = "cm", dpi=150, device=cairo_pdf)
}

# ---------- VISUAL EXPLORATION PLOTS FULL TRIAL BLOCKS ---------- 
# density plots for choosing best arm per version per version per environment
best.all <- ggplot(dat.collect, aes(x=p.correct.arm, fill=version)) +
  geom_density(alpha=0.6) + 
  scale_fill_discrete(name = "Version", 
                      labels = c("Start high drift/\nlow noise", 
                                 "Start low drift/\nhigh noise")) +
  labs(title="proportion best arm throughout task", 
       subtitle="both environments, four blocks combined") + 
  xlim(0.0, 0.6)

best.hl <- ggplot(dat.collect.hl, aes(x=p.correct.arm, fill=version)) +
  geom_density(alpha=0.6) + 
  scale_fill_discrete(name = "Version", 
                      labels = c("Start high drift/\nlow noise", 
                                 "Start low drift/\nhigh noise")) +
  labs(title="proportion best arm in high drift/low noise", 
       subtitle="both high drift/low noise blocks combined") + 
  xlim(0.0, 0.6)

best.lh <- ggplot(dat.collect.lh, aes(x=p.correct.arm, fill=version)) +
  geom_density(alpha=0.6) + 
  scale_fill_discrete(name = "Version", 
                      labels = c("Start high drift/\nlow noise", 
                                 "Start low drift/\nhigh noise")) +
  labs(title="proportion best arm in low drift/high noise", 
       subtitle="both low drift/high noise blocks combined") + 
  xlim(0.0, 0.6)

grid.arrange(best.all, best.hl, best.lh)

# density plots for choosing best arm per version per version per environment
# second blocks only
best.hl.first <- ggplot(dat.collect.hl.first, 
                        aes(x=p.correct.arm, fill=version)) +
  geom_density(alpha=0.6) + 
  scale_fill_discrete(name = "Version", 
                      labels = c("Start high drift/\nlow noise", 
                                 "Start low drift/\nhigh noise")) +
  labs(title="proportion best arm in high drift/low noise", 
       subtitle="first high drift/low noise block only") + 
  xlim(0.0, 0.6) +
  ylim(0, 8)

best.lh.first <- ggplot(dat.collect.lh.first, 
                        aes(x=p.correct.arm, fill=version)) +
  geom_density(alpha=0.6) + 
  scale_fill_discrete(name = "Version", 
                      labels = c("Start high drift/\nlow noise", 
                                 "Start low drift/\nhigh noise")) +
  labs(title="proportion best arm in low drift/high noise", 
       subtitle="first low drift/high noise block only") + 
  xlim(0.0, 0.6) +
  ylim(0, 8)

# density plots for choosing best arm per version per version per environment
# second blocks only
best.hl.second <- ggplot(dat.collect.hl.second, 
                         aes(x=p.correct.arm, fill=version)) +
  geom_density(alpha=0.6) + 
  scale_fill_discrete(name = "Version", 
                      labels = c("Start high drift/\nlow noise", 
                                 "Start low drift/\nhigh noise")) +
  labs(title="proportion best arm in high drift/low noise", 
       subtitle="second high drift/low noise block only") + 
  xlim(0.0, 0.6) +
  ylim(0, 8)

best.lh.second <- ggplot(dat.collect.lh.second, 
                         aes(x=p.correct.arm, fill=version)) +
  geom_density(alpha=0.6) + 
  scale_fill_discrete(name = "Version", 
                      labels = c("Start high drift/\nlow noise", 
                                 "Start low drift/\nhigh noise")) +
  labs(title="proportion best arm in low drift/high noise", 
       subtitle="second low drift/high noise block only") + 
  xlim(0.0, 0.6) +
  ylim(0, 8)

grid.arrange(best.hl.first, best.hl.second, best.lh.first, best.lh.second, ncol=2)

### violin plots ###
# SWITCHES
# number of switches
switch.violin <- dat.collect %>%
  ggplot(aes(x=version, y=n.switches, fill=version)) +
  geom_violin(draw_quantiles=T, alpha=0.5) +
  theme(legend.position="none") +
  labs(title="Number of switches per version",
       y="Number of switches over trials", x="Version", color="Version") +
  scale_fill_manual(values=c("#1fc5c3", "#ff101f", "#3b8c84", "#edd83d")) +
  ylim(0, 350)

# number of switches hl blocks
switch.violin.hl <- dat.collect.hl %>%
  ggplot(aes(x=version, y=n.switches, fill=version)) +
  geom_violin(draw_quantiles=T, alpha=0.5) +
  theme(legend.position="none") +
  labs(title="Number of switches per version", 
       subtitle="high drift/low noise blocks",
       y="Number of switches over trials", x="Version", color="Version") +
  scale_fill_manual(values=c("#1fc5c3", "#ff101f", "#3b8c84", "#edd83d")) +
  ylim(0, 350)

# number of switches lh blocks
switch.violin.lh <- dat.collect.lh %>%
  ggplot(aes(x=version, y=n.switches, fill=version)) +
  geom_violin(draw_quantiles=T, alpha=0.5) +
  theme(legend.position="none") +
  labs(title="Number of switches per version", 
       subtitle="low drift/high noise blocks",
       y="Number of switches over trials", x="Version", color="Version") +
  scale_fill_manual(values=c("#1fc5c3", "#ff101f", "#3b8c84", "#edd83d")) +
  ylim(0, 350)

grid.arrange(switch.violin, switch.violin.lh, switch.violin.hl, ncol=3)

# PERFORMANCE
# performance in best arm chosen
perf.arm.violin <-  dat.collect %>%
  ggplot(aes(x=version, y=p.correct.arm, fill=version)) +
  geom_violin(draw_quantiles=T, alpha=0.5) +
  theme(legend.position="none") +
  labs(title="Performance proportion best arm per version",
       y="Proportion best arm chosen", x="Version", color="Version") +
  scale_fill_manual(values=c("#1fc5c3", "#ff101f", "#3b8c84", "#edd83d")) +
  ylim(0, 0.55)

perf.arm.violin.lh <-  dat.collect.lh %>%
  ggplot(aes(x=version, y=p.correct.arm, fill=version)) +
  geom_violin(draw_quantiles=T, alpha=0.5) +
  theme(legend.position="none") +
  labs(title="Performance proportion best arm per version",
       subtitle="low drift/high noise blocks",
       y="Proportion best arm chosen", x="Version", color="Version") +
  scale_fill_manual(values=c("#1fc5c3", "#ff101f", "#3b8c84", "#edd83d")) +
  ylim(0, 0.55)

perf.arm.violin.hl <-  dat.collect.hl %>%
  ggplot(aes(x=version, y=p.correct.arm, fill=version)) +
  geom_violin(draw_quantiles=T, alpha=0.5) +
  theme(legend.position="none") +
  labs(title="Performance proportion best arm per version",
       subtitle="high drift/low noise blocks",
       y="Proportion best arm chosen", x="Version", color="Version") +
  scale_fill_manual(values=c("#1fc5c3", "#ff101f", "#3b8c84", "#edd83d")) +
  ylim(0, 0.55)

grid.arrange(perf.arm.violin, perf.arm.violin.lh, perf.arm.violin.hl, ncol=3)

# ---------- VISUAL EXPLORATION PLOTS TRUNCATED BLOCKS ---------- 
# density plots for choosing best arm per version per version per environment
best.all.trunc <- ggplot(dat.collect.trunc, aes(x=p.correct.arm, fill=version)) +
  geom_density(alpha=0.6) + 
  scale_fill_discrete(name = "Version", 
                      labels = c("Start high drift/\nlow noise", 
                                 "Start low drift/\nhigh noise")) +
  labs(title="proportion best arm throughout task", 
       subtitle="both environments, four blocks combined") + 
  xlim(0.0, 0.6)

best.hl.trunc <- ggplot(dat.collect.hl.trunc, aes(x=p.correct.arm, fill=version)) +
  geom_density(alpha=0.6) + 
  scale_fill_discrete(name = "Version", 
                      labels = c("Start high drift/\nlow noise", 
                                 "Start low drift/\nhigh noise")) +
  labs(title="proportion best arm in high drift/low noise", 
       subtitle="both high drift/low noise blocks combined") + 
  xlim(0.0, 0.6)

best.lh.trunc <- ggplot(dat.collect.lh.trunc, aes(x=p.correct.arm, fill=version)) +
  geom_density(alpha=0.6) + 
  scale_fill_discrete(name = "Version", 
                      labels = c("Start high drift/\nlow noise", 
                                 "Start low drift/\nhigh noise")) +
  labs(title="proportion best arm in low drift/high noise", 
       subtitle="both low drift/high noise blocks combined") + 
  xlim(0.0, 0.6)

grid.arrange(best.all.trunc, best.hl.trunc, best.lh.trunc)

# density plots for choosing best arm per version per version per environment
# second blocks only
best.hl.first.trunc <- ggplot(dat.collect.hl.first.trunc, 
                        aes(x=p.correct.arm, fill=version)) +
  geom_density(alpha=0.6) + 
  scale_fill_discrete(name = "Version", 
                      labels = c("Start high drift/\nlow noise", 
                                 "Start low drift/\nhigh noise")) +
  labs(title="proportion best arm in high drift/low noise", 
       subtitle="first high drift/low noise block only") + 
  xlim(0.0, 0.6) +
  ylim(0, 8)

best.lh.first.trunc <- ggplot(dat.collect.lh.first.trunc, 
                        aes(x=p.correct.arm, fill=version)) +
  geom_density(alpha=0.6) + 
  scale_fill_discrete(name = "Version", 
                      labels = c("Start high drift/\nlow noise", 
                                 "Start low drift/\nhigh noise")) +
  labs(title="proportion best arm in low drift/high noise", 
       subtitle="first low drift/high noise block only") + 
  xlim(0.0, 0.6) +
  ylim(0, 8)

# density plots for choosing best arm per version per version per environment
# second blocks only
best.hl.second.trunc <- ggplot(dat.collect.hl.second.trunc, 
                         aes(x=p.correct.arm, fill=version)) +
  geom_density(alpha=0.6) + 
  scale_fill_discrete(name = "Version", 
                      labels = c("Start high drift/\nlow noise", 
                                 "Start low drift/\nhigh noise")) +
  labs(title="proportion best arm in high drift/low noise", 
       subtitle="second high drift/low noise block only") + 
  xlim(0.0, 0.6) +
  ylim(0, 8)

best.lh.second.trunc <- ggplot(dat.collect.lh.second.trunc, 
                         aes(x=p.correct.arm, fill=version)) +
  geom_density(alpha=0.6) + 
  scale_fill_discrete(name = "Version", 
                      labels = c("Start high drift/\nlow noise", 
                                 "Start low drift/\nhigh noise")) +
  labs(title="proportion best arm in low drift/high noise", 
       subtitle="second low drift/high noise block only") + 
  xlim(0.0, 0.6) +
  ylim(0, 8)

grid.arrange(best.hl.first.trunc, best.hl.second.trunc, 
             best.lh.first.trunc, best.lh.second.trunc, ncol=2)

### violin plots ###
# SWITCHES
# number of switches
switch.violin.trunc <- dat.collect.trunc %>%
  ggplot(aes(x=version, y=n.switches, fill=version)) +
  geom_violin(draw_quantiles=T, alpha=0.5) +
  theme(legend.position="none") +
  labs(title="Number of switches per version",
       y="Number of switches over trials", x="Version", color="Version") +
  scale_fill_manual(values=c("#1fc5c3", "#ff101f", "#3b8c84", "#edd83d")) +
  ylim(0, 200)

# number of switches hl blocks
switch.violin.hl.trunc <- dat.collect.hl.trunc %>%
  ggplot(aes(x=version, y=n.switches, fill=version)) +
  geom_violin(draw_quantiles=T, alpha=0.5) +
  theme(legend.position="none") +
  labs(title="Number of switches per version", 
       subtitle="high drift/low noise blocks",
       y="Number of switches over trials", x="Version", color="Version") +
  scale_fill_manual(values=c("#1fc5c3", "#ff101f", "#3b8c84", "#edd83d")) +
  ylim(0, 200)

# number of switches lh blocks
switch.violin.lh.trunc <- dat.collect.lh.trunc %>%
  ggplot(aes(x=version, y=n.switches, fill=version)) +
  geom_violin(draw_quantiles=T, alpha=0.5) +
  theme(legend.position="none") +
  labs(title="Number of switches per version", 
       subtitle="low drift/high noise blocks",
       y="Number of switches over trials", x="Version", color="Version") +
  scale_fill_manual(values=c("#1fc5c3", "#ff101f", "#3b8c84", "#edd83d")) +
  ylim(0, 200)

grid.arrange(switch.violin.trunc, switch.violin.lh.trunc, 
             switch.violin.hl.trunc, ncol=3)

# PERFORMANCE
# performance in best arm chosen
perf.arm.violin.trunc <-  dat.collect.trunc %>%
  ggplot(aes(x=version, y=p.correct.arm, fill=version)) +
  geom_violin(draw_quantiles=T, alpha=0.5) +
  theme(legend.position="none") +
  labs(title="Performance proportion best arm per version",
       y="Proportion best arm chosen", x="Version", color="Version") +
  scale_fill_manual(values=c("#1fc5c3", "#ff101f", "#3b8c84", "#edd83d")) +
  ylim(0, 0.55)

perf.arm.violin.lh.trunc <-  dat.collect.lh.trunc %>%
  ggplot(aes(x=version, y=p.correct.arm, fill=version)) +
  geom_violin(draw_quantiles=T, alpha=0.5) +
  theme(legend.position="none") +
  labs(title="Performance proportion best arm per version",
       subtitle="low drift/high noise blocks",
       y="Proportion best arm chosen", x="Version", color="Version") +
  scale_fill_manual(values=c("#1fc5c3", "#ff101f", "#3b8c84", "#edd83d")) +
  ylim(0, 0.55)

perf.arm.violin.hl.trunc <-  dat.collect.hl.trunc %>%
  ggplot(aes(x=version, y=p.correct.arm, fill=version)) +
  geom_violin(draw_quantiles=T, alpha=0.5) +
  theme(legend.position="none") +
  labs(title="Performance proportion best arm per version",
       subtitle="high drift/low noise blocks",
       y="Proportion best arm chosen", x="Version", color="Version") +
  scale_fill_manual(values=c("#1fc5c3", "#ff101f", "#3b8c84", "#edd83d")) +
  ylim(0, 0.55)

grid.arrange(perf.arm.violin.trunc, perf.arm.violin.lh.trunc, 
             perf.arm.violin.hl.trunc, ncol=3)

# ---------- ANALYSES ENTROPY ---------- 
# Entropy higher on exploration than on exploitation
# (t-test on groups switch vs. non-switch trials)

# Gradual decrease in entropy
# (effect of trial on entropy)
mod.decr.entr <- lm(entropy ~ trial, data=dat)
summary(mod.decr.entr) 

plot.decr.entr <- dat %>%
  filter(version=="shl" & id==2) %>%
  ggplot(aes(x=trial, y=entropy, color=id)) +
  geom_line()

# Entropy higher in low drift/high noise blocks than high drift/low nosie blocks
# (t-test l/h blocks vs h/l blocks)

