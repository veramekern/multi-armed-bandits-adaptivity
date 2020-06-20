# rm(list = ls())
# rm(points.dists)
#  "psych", "car", "MASS", "nlme",  
# "Hmisc", "stringr", "multcomp", "gridExtra", "doBy", 
# "gridExtra", "reshape2", "tibble", "DescTools", 
# "ggridges", "NCmisc"

# ---------- INIT ---------- 
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = T)
  sapply(pkg, require, character.only = T)
}
packages <- c("dplyr", "plotly", "ggplot2", "ggthemes", "hrbrthemes", "tidyr")
ipak(packages)

# ---------- LOAD DATA ---------- 
# TO LOAD DATA FROM RAW DATA FILE
# dat <- read.csv(file=paste("../data/erc-wp1-bp76-full-data-mab.csv", sep=""),
#                 sep=";",
#                 dec=".")
# dat <- subset(dat, select=c("ExperimentName", "Subject", "OverallGain2.Block.",
#                             "ExpBlock", "chose_hist", "keuze.RT", "noise",
#                             "payoff1", "payoff2", "payoff3", "payoff4",
#                             "pts_hist"))
# colnames(dat) <- c("version", "id", "total", "trial", "choice", "rt", "noise",
#                    "arm1", "arm2", "arm3", "arm4", "points")
# levels(dat$version) <- list("a" = "GokTaak_VersieA", "b" = "GokTaak_VersieB")
# 
# # exclude participants due to recent drug use, psychiatric history, non-nativity
# # exclude participant 104 for random playing
# # exclude participant 64 for only playing two machine arms
# exclude <- read.csv(file=paste("../data/erc-wp1-bp76-exclude-mab.csv",
#                                sep=""), sep=";", dec=".")
# dat <- left_join(dat, exclude, by="id")
# dat <- subset(dat, exclude==0)
# length(unique(dat$id)) # down to N = 64
# 
# write.table(dat, file=paste0("../../../0003-chapter-3-adaptivity-bandit-tasks/",
#             "research/03-data/chapter-3-full-data-study-1.csv"),
#             row.names=F, col.names = T, sep=";", dec=".")

# TO LOAD FROM CLEANED FILE (STILL INCLUDES PRACTICE TRIALS, 
# EXCLUDES PARTICIPANTS THAT SHOULD BE EXCLUDED)
dat <- read.csv(file=paste0("../../../0003-chapter-3-adaptivity-bandit-tasks/", 
                            "research/03-data/chapter-3-full-data-study-1.csv"),
                sep=";", dec=".")

dat <- dat[complete.cases(dat),] # remove practice trials
dat <- subset(dat, select=-exclude) # remove exclude variable

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
  mutate(cluster.start = ifelse(cluster == 1 & lag(cluster) == 0, 1, 0)) %>%
  mutate(cluster.start = ifelse(cluster == 1 & is.na(lag(cluster)), 1, cluster.start))

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
dat$best.possible <- max.col(dat[,8:11])
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
            rt.start.cluster = sum(rt * cluster.start)/n.clusters)

# ---------- VISUAL EXPLORATION ---------- 

# calculate distribution variables
# find payoff distributions
payoff.dist <- dat %>%
  group_by(version, trial) %>%
  summarise(arm1 = mean(arm1) + mean(noise), 
            arm2 = mean(arm2) + mean(noise), 
            arm3 = mean(arm3) + mean(noise), 
            arm4 = mean(arm4) + mean(noise))

# make long distribution table for graphs
long.payoff.dist <- payoff.dist %>% gather(arm, points, arm1:arm4)

# split per group
dist.a <- filter(long.payoff.dist, version == 'a')
dist.b <- filter(long.payoff.dist, version == 'b')

# histograms, violin plots

# number of switches
switch.hist <- dat.collect %>%
  ggplot(aes(x=n.switches, fill=version)) +
  geom_histogram(color="#e9ecef", alpha=0.5, position = 'identity', bins = 20) +
  scale_fill_manual(values=c("#e24068", "#fa6c0c")) +
  theme_ipsum() +
  labs(fill="")
ggplotly(switch.hist)

switch.violin <-  dat.collect %>%
  ggplot(aes(x=version, y=n.switches, fill=version)) +
  geom_violin(draw_quantiles=T, alpha=0.5) +
  scale_fill_manual(values=c("#e24068", "#fa6c0c")) +
  theme_ipsum()
ggplotly(switch.violin)

# number of clusters
clust.hist <- dat.collect %>%
  ggplot( aes(x=n.clusters, fill=version)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 20) +
  scale_fill_manual(values=c("#e24068", "#fa6c0c")) +
  theme_ipsum() +
  labs(fill="")
ggplotly(clust.hist)

clust.violin <-  dat.collect %>%
  ggplot(aes(x=version, y=n.clusters, fill=version)) +
  geom_violin(draw_quantiles=T, alpha=0.5) +
  scale_fill_manual(values=c("#e24068", "#fa6c0c")) +
  theme_ipsum()
ggplotly(clust.violin)

# performance in total payoff
perf.hist <- dat.collect %>%
  ggplot( aes(x=performance, fill=version)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 20) +
  scale_fill_manual(values=c("#e24068", "#fa6c0c")) +
  theme_ipsum() +
  labs(fill="")
ggplotly(perf.hist)

perf.violin <-  dat.collect %>%
  ggplot(aes(x=version, y=performance, fill=version)) +
  geom_violin(draw_quantiles=T, alpha=0.5) +
  scale_fill_manual(values=c("#e24068", "#fa6c0c")) +
  theme_ipsum()
ggplotly(perf.violin)

# plot switches per participant to pdf
# split data per id
split.dat <- split(dat, dat$id)
names.dat <- colnames(dat)

#prepare pdf
pdf("switches.pdf") 

#plot to pdf
for (i in 1:length(unique(dat$id))) {
  temp.dat <- data.frame(split.dat[i])
  names(temp.dat) <- names.dat
  if (temp.dat$version[1]=="a") {
    print(ggplot(data=dist.a,
                 aes(x=trial, y=points, colour=arm)) + 
            geom_line(size=0.1) + ylim(0, 100) +
            geom_vline(colour="gray", 
                       xintercept=temp.dat$trial[temp.dat$switch==1], size=0.1) +
            labs(y="Payoff", x="Trial", fill="",
                 title=paste0("Participant ", temp.dat[1,2])) +
            theme_calc() + 
            scale_color_manual(values=c("#e24068", "#fa6c0c", "#509E75", "#508F9E"))
    )
  }
  else {
    print(ggplot(data=dist.b,
                 aes(x=trial, y=points, colour=arm)) + 
            geom_line(size=0.1) + ylim(0, 100) +
            geom_vline(colour="gray", 
                       xintercept=temp.dat$trial[temp.dat$switch==1], size=0.1) +
            labs(y="Payoff", x="Trial", fill="",
                 title=paste0("Participant ", temp.dat[1,2])) +
            theme_calc() + 
            scale_color_manual(values=c("#e24068", "#fa6c0c", "#509E75", "#508F9E"))
    )
  }
}
dev.off()
