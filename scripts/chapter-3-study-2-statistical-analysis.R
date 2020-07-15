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

# plot payoff distributions per version
plot.ll <- ggplot(data=dist.ll, aes(x=trial, y=points, colour=arm)) +
  geom_line(size=0.5) + ylim(0, 100) +
  labs(title="Point distribution per arm", subtitle="Version: Low Drift/Low Noise",
       y="Points Earned on Trial", x="Trial Number", color="Arm") +
  scale_color_manual(values=c("#1fc5c3", "#3b8c84", "#ff101f", "#edd83d"), 
                     labels=c("Arm 1", "Arm 2", "Arm 3", "Arm 4"))

plot.lh <- ggplot(data=dist.lh, aes(x=trial, y=points, colour=arm)) +
  geom_line(size=0.5) + ylim(0, 100) +
  labs(title="Point distribution per arm", subtitle="Version: Low Drift/High Noise",
       y="Points Earned on Trial", x="Trial Number", color="Arm") +
  scale_color_manual(values=c("#1fc5c3", "#3b8c84", "#ff101f", "#edd83d"), 
                     labels=c("Arm 1", "Arm 2", "Arm 3", "Arm 4"))

plot.hl <- ggplot(data=dist.hl, aes(x=trial, y=points, colour=arm)) +
  geom_line(size=0.5) + ylim(0, 100) +
  labs(title="Point distribution per arm", subtitle="Version: High Drift/Low Noise",
       y="Points Earned on Trial", x="Trial Number", color="Arm") +
  scale_color_manual(values=c("#1fc5c3", "#3b8c84", "#ff101f", "#edd83d"), 
                     labels=c("Arm 1", "Arm 2", "Arm 3", "Arm 4"))

plot.hh <- ggplot(data=dist.hh, aes(x=trial, y=points, colour=arm)) +
  geom_line(size=0.5) + ylim(0, 100) +
  labs(title="Point distribution per arm", subtitle="Version: High Drift/High Noise",
       y="Points Earned on Trial", x="Trial Number", color="Arm") +
  scale_color_manual(values=c("#1fc5c3", "#3b8c84", "#ff101f", "#edd83d"), 
                     labels=c("Arm 1", "Arm 2", "Arm 3", "Arm 4"))

# split data per id
split.dat <- split(dat, dat$id)
names.dat <- colnames(dat)

# plot switches per participant to pdf
# plot to pdfs to combine w/ acrobat pro
for (i in 1:length(unique(dat$id))) {
  temp.dat <- data.frame(split.dat[i])
  names(temp.dat) <- names.dat
  if (temp.dat$version[1]=="ll") {
    temp_plot <- ggplot(data=dist.ll, aes(x=trial, y=points, colour=arm)) +
      geom_line(size=0.1) + ylim(0, 100) +
      geom_vline(colour="#FFC2B4", xintercept=temp.dat$trial[temp.dat$switch==1], 
                 size=0.3, alpha=0.5) +
      ggtitle(label=paste0("Participant ", temp.dat[1,2]), 
              subtitle=paste0("Version ", temp.dat[1,1])) + 
      xlab("Trial number") + 
      ylab("Points earned on trial") +
      scale_color_manual(values=c("#1fc5c3", "#3b8c84", "#ff101f", "#edd83d"))
    ggsave(temp_plot, 
           file=paste0("../../results/chapter-3-study-2-switches-", i,".pdf"), 
           width = 21, height = 21, units = "cm", dpi=150, device=cairo_pdf)
  }
  else if (temp.dat$version[1]=="lh") {
    temp_plot <- ggplot(data=dist.lh, aes(x=trial, y=points, colour=arm)) +
      geom_line(size=0.1) + ylim(0, 100) +
      geom_vline(colour="#FFC2B4", xintercept=temp.dat$trial[temp.dat$switch==1], 
                 size=0.3, alpha=0.5) +
      ggtitle(label=paste0("Participant ", temp.dat[1,2]), 
              subtitle=paste0("Version ", temp.dat[1,1])) + 
      xlab("Trial number") + 
      ylab("Points earned on trial") +
      scale_color_manual(values=c("#1fc5c3", "#3b8c84", "#ff101f", "#edd83d"))
    ggsave(temp_plot, 
           file=paste0("../../results/chapter-3-study-2-switches-", i,".pdf"), 
           width = 21, height = 21, units = "cm", dpi=150, device=cairo_pdf)
  }
  else if (temp.dat$version[1]=="hl") {
    temp_plot <- ggplot(data=dist.hl, aes(x=trial, y=points, colour=arm)) +
      geom_line(size=0.1) + ylim(0, 100) +
      geom_vline(colour="#FFC2B4", xintercept=temp.dat$trial[temp.dat$switch==1], 
                 size=0.3, alpha=0.5) +
      ggtitle(label=paste0("Participant ", temp.dat[1,2]), 
              subtitle=paste0("Version ", temp.dat[1,1])) + 
      xlab("Trial number") + 
      ylab("Points earned on trial") +
      scale_color_manual(values=c("#1fc5c3", "#3b8c84", "#ff101f", "#edd83d"))
    ggsave(temp_plot, 
           file=paste0("../../results/chapter-3-study-2-switches-", i,".pdf"), 
           width = 21, height = 21, units = "cm", dpi=150, device=cairo_pdf)
  }
  else {
    temp_plot <- ggplot(data=dist.hh, aes(x=trial, y=points, colour=arm)) +
      geom_line(size=0.1) + ylim(0, 100) +
      geom_vline(colour="#FFC2B4", xintercept=temp.dat$trial[temp.dat$switch==1], 
                 size=0.3, alpha=0.5) +
      ggtitle(label=paste0("Participant ", temp.dat[1,2]), 
              subtitle=paste0("Version ", temp.dat[1,1])) + 
      xlab("Trial number") + 
      ylab("Points earned on trial") +
      scale_color_manual(values=c("#1fc5c3", "#3b8c84", "#ff101f", "#edd83d"))
    ggsave(temp_plot, 
           file=paste0("../../results/chapter-3-study-2-switches-", i,".pdf"), 
           width = 21, height = 21, units = "cm", dpi=150, device=cairo_pdf)
  }
}

# density plot for switches per version
ggplot(dat.collect, aes(x=n.switches, fill=version)) +
  geom_density(alpha=0.6) + 
  scale_fill_discrete(name = "Version", labels = c("High drift/High noise", 
                                                   "High drift/Low noise", 
                                                   "Low drift/High noise", 
                                                   "Low drift/Low noise"))

# plot best choice or not per participant to pdf
# separate pdfs to be combined in acrobat pro
for (i in 1:length(unique(dat$id))) {
  temp.dat <- data.frame(split.dat[i])
  names(temp.dat) <- names.dat
  if (temp.dat$version[1]=="ll") {
    temp_plot <- ggplot(data=dist.ll, aes(x=trial, y=points, colour=arm)) +
      geom_line(size=0.1) + ylim(0, 100) +
      geom_vline(colour="#FFC2B4", xintercept=temp.dat$trial[temp.dat$best.chosen==1], 
                 size=0.1, alpha=0.5) +
      ggtitle(label=paste0("Participant ", temp.dat[1,2]), 
              subtitle=paste0("Version ", temp.dat[1,1])) + 
      xlab("Trial number") + 
      ylab("Points earned on trial") +
      scale_color_manual(values=c("#1fc5c3", "#3b8c84", "#ff101f", "#edd83d")) +
      labs(caption="vertical lines indicate arm chosen was best arm")
    ggsave(temp_plot, 
           file=paste0("../../results/chapter-3-study-2-best.best-chosen-", i,".pdf"), 
           width = 21, height = 21, units = "cm", dpi=150, device=cairo_pdf)
  }
  else if (temp.dat$version[1]=="lh") {
    temp_plot <- ggplot(data=dist.lh, aes(x=trial, y=points, colour=arm)) +
      geom_line(size=0.1) + ylim(0, 100) +
      geom_vline(colour="#FFC2B4", xintercept=temp.dat$trial[temp.dat$best.chosen==1], 
                 size=0.1, alpha=0.5) +
      ggtitle(label=paste0("Participant ", temp.dat[1,2]), 
              subtitle=paste0("Version ", temp.dat[1,1])) + 
      xlab("Trial number") + 
      ylab("Points earned on trial") +
      scale_color_manual(values=c("#1fc5c3", "#3b8c84", "#ff101f", "#edd83d")) +
      labs(caption="vertical lines indicate arm chosen was best arm")
    ggsave(temp_plot, 
           file=paste0("../../results/chapter-3-study-2-best.best-chosen-", i,".pdf"), 
           width = 21, height = 21, units = "cm", dpi=150, device=cairo_pdf)
  }
  else if (temp.dat$version[1]=="hl") {
    temp_plot <- ggplot(data=dist.hl, aes(x=trial, y=points, colour=arm)) +
      geom_line(size=0.1) + ylim(0, 100) +
      geom_vline(colour="#FFC2B4", xintercept=temp.dat$trial[temp.dat$best.chosen==1], 
                 size=0.1, alpha=0.5) +
      ggtitle(label=paste0("Participant ", temp.dat[1,2]), 
              subtitle=paste0("Version ", temp.dat[1,1])) + 
      xlab("Trial number") + 
      ylab("Points earned on trial") +
      scale_color_manual(values=c("#1fc5c3", "#3b8c84", "#ff101f", "#edd83d")) +
      labs(caption="vertical lines indicate arm chosen was best arm")
    ggsave(temp_plot, 
           file=paste0("../../results/chapter-3-study-2-best.best-chosen-", i,".pdf"), 
           width = 21, height = 21, units = "cm", dpi=150, device=cairo_pdf)
  }
  else {
    temp_plot <- ggplot(data=dist.hh, aes(x=trial, y=points, colour=arm)) +
      geom_line(size=0.1) + ylim(0, 100) +
      geom_vline(colour="#FFC2B4", xintercept=temp.dat$trial[temp.dat$best.chosen==1], 
                 size=0.1, alpha=0.5) +
      ggtitle(label=paste0("Participant ", temp.dat[1,2]), 
              subtitle=paste0("Version ", temp.dat[1,1])) + 
      xlab("Trial number") + 
      ylab("Points earned on trial") +
      scale_color_manual(values=c("#1fc5c3", "#3b8c84", "#ff101f", "#edd83d")) +
      labs(caption="vertical lines indicate arm chosen was best arm")
    ggsave(temp_plot, 
           file=paste0("../../results/chapter-3-study-2-best.best-chosen-", i,".pdf"), 
           width = 21, height = 21, units = "cm", dpi=150, device=cairo_pdf)
  }
}

# density plots for choosing best arm per version
ggplot(dat.collect, aes(x=p.correct.arm, fill=version)) +
  geom_density(alpha=0.6) + 
  scale_fill_discrete(name = "Version", labels = c("High drift/High noise", 
                                                   "High drift/Low noise", 
                                                   "Low drift/High noise", 
                                                   "Low drift/Low noise"))

### violin plots ###
# number of switches
switch.violin <- dat.collect %>%
  ggplot(aes(x=version, y=n.switches, fill=version)) +
  geom_violin(draw_quantiles=T, alpha=0.5) +
  theme(legend.position="none") +
  labs(title="Number of switches per version",
       y="Number of switches over trials", x="Version", color="Version") +
  scale_fill_manual(values=c("#1fc5c3", "#ff101f", "#3b8c84", "#edd83d"))
# ggplotly(switch.violin)

# number of clusters
clust.violin <- dat.collect %>%
  ggplot(aes(x=version, y=cluster.size, fill=version)) +
  geom_violin(draw_quantiles=T, alpha=0.5) +
  theme(legend.position="none") +
  labs(title="Average cluster size per version",
       y="Average cluster size over trials", x="Version", color="Version") +
  scale_fill_manual(values=c("#1fc5c3", "#ff101f", "#3b8c84", "#edd83d"))
# ggplotly(clust.violin)

# performance in total payoff
perf.violin <- dat.collect %>%
  ggplot(aes(x=version, y=performance, fill=version)) +
  geom_violin(draw_quantiles=T, alpha=0.5) +
  theme(legend.position="none") +
  labs(title="Performance in points per version",
       y="Total amount of points gained", x="Version", color="Version") +
  scale_fill_manual(values=c("#1fc5c3", "#ff101f", "#3b8c84", "#edd83d"))
# ggplotly(perf.violin)

# performance in best arm chosen
perf.arm.violin <-  dat.collect %>%
  ggplot(aes(x=version, y=p.correct.arm, fill=version)) +
  geom_violin(draw_quantiles=T, alpha=0.5) +
  theme(legend.position="none") +
  labs(title="Performance proportion best arm per version",
       y="Proportion best arm chosen", x="Version", color="Version") +
  scale_fill_manual(values=c("#1fc5c3", "#ff101f", "#3b8c84", "#edd83d"))
# ggplotly(perf.arm.violin)

grid.arrange(switch.violin, clust.violin, perf.violin, perf.arm.violin, nrow=4)

# ---------- ANALYSES ---------- 

# n.switches on drift * noise
dat.collect %>%
  group_by(drift, noise) %>%
  summarise(mean=mean(n.switches), 
            stdev=sd(n.switches), 
            median=median(n.switches))

dat.collect %>%
  ggplot(aes(x=drift, y=n.switches, fill=noise)) +
  geom_boxplot()

outliers <- dat.collect %>%
  group_by(drift, noise) %>%
  identify_outliers(n.switches)

resmod <- lm(n.switches ~ version, data = dat.collect)
qqPlot(residuals(resmod))
shapiro_test(residuals(resmod))
dat.collect %>%
  group_by(drift, noise) %>%
  shapiro_test(n.switches)
plot(resmod)

model = lm(n.switches ~ drift + noise + drift:noise,
           data=dat.collect)
Anova(model, type="III")

raov(n.switches ~ drift + noise + drift:noise,
   data=dat.collect)

# p.correct.arm on drift * noise

dat.collect %>%
  group_by(drift, noise) %>%
  summarise(mean=mean(p.correct.arm), 
            stdev=sd(p.correct.arm), 
            median=median(p.correct.arm))

dat.collect %>%
  ggplot(aes(x=drift, y=p.correct.arm, fill=noise)) +
  geom_boxplot()

outliers <- dat.collect %>%
  group_by(drift, noise) %>%
  identify_outliers(p.correct.arm)

resmod <- lm(p.correct.arm ~ version, data = dat.collect)
qqPlot(residuals(resmod))
shapiro_test(residuals(resmod))
dat.collect %>%
  group_by(drift, noise) %>%
  shapiro_test(p.correct.arm)
plot(resmod)
leveneTest(y=dat.collect$p.correct.arm, group=dat.collect$version, center=median)

model = lm(p.correct.arm ~ drift + noise + drift:noise,
           data=dat.collect)
Anova(model, type="III")

raov(p.correct.arm ~ drift + noise + drift:noise,
     data=dat.collect)
