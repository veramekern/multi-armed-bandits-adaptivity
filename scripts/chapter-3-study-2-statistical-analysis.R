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

# ---------- LOAD DATA ----------
# TO LOAD DATA FROM RAW DATA FILE
# dat <- read.csv(file=paste("../../../03-data/chapter-3-study-2-raw-data.csv"),
#                 sep=",",
#                 dec=".")
# 
# dat$version <- as.factor(dat$version)
# levels(dat$version)
# 
# # exclude participants due to recent drug use, psychiatric history, non-nativity
# exclude <- read.csv(file=paste("../../../03-data/chapter-3-study-2-exclude.csv"),
#                     sep=",", dec=".")
# dat <- left_join(dat, exclude, by="id")
# dat <- subset(dat, exclude==0)
# length(unique(dat$id)) # down to N = 64
# 
# write.table(dat, file=paste0("../../../03-data/chapter-3-study-2-full-data.csv"),
#             row.names=F, col.names = T, sep=";", dec=".")

# TO LOAD FROM CLEANED FILE (STILL INCLUDES PRACTICE TRIALS, 
# EXCLUDES PARTICIPANTS THAT SHOULD BE EXCLUDED)
dat <- read.csv(file=paste0("../../../03-data/chapter-3-study-2-full-data.csv"),
                sep=";", dec=".")

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
dist.hh <- filter(long.payoff.dist, version == 'HRW_HN')
dist.hl <- filter(long.payoff.dist, version == 'HRW_LN')
dist.lh <- filter(long.payoff.dist, version == 'LRW_HN')
dist.ll <- filter(long.payoff.dist, version == 'LRW_LN')

# ridge plots, violin plots

# number of switches
switch.ridge <- dat.collect %>%
  ggplot(aes(x=n.switches, y=version, fill=version)) +
  geom_density_ridges(color="#e9ecef", alpha=0.5) +
  theme_ridges() +
  theme(legend.position="none")
plot(switch.ridge)

switch.violin <-  dat.collect %>%
  ggplot(aes(x=version, y=n.switches, fill=version)) +
  geom_violin(draw_quantiles=T, alpha=0.5) +
  theme_ipsum() +
  theme(legend.position="none")
ggplotly(switch.violin)

# number of clusters
# ***TODO

# performance in total payoff
# ***TODO


# plot switches per participant to pdf
# split data per id
split.dat <- split(dat, dat$id)
names.dat <- colnames(dat)

#prepare pdf
pdf("../../results/chapter-3-study-2-switches-per-pp.pdf") 

#plot to pdf
for (i in 1:length(unique(dat$id))) {
  temp.dat <- data.frame(split.dat[i])
  names(temp.dat) <- names.dat
  if (temp.dat$version[1]=="HRW_HN") {
    print(ggplot(data=dist.hh,
                 aes(x=trial, y=points, colour=arm)) + 
            geom_line(size=0.1) + ylim(0, 100) +
            geom_vline(colour="gray", 
                       xintercept=temp.dat$trial[temp.dat$switch==1], size=0.1) +
            labs(y="Payoff", x="Trial", fill="",
                 title=paste0("Participant ", temp.dat[1,2], ", Version ", 
                              temp.dat[1,1])) +
            theme_calc() + 
            scale_color_manual(values=c("#e24068", "#fa6c0c", "#509E75", "#508F9E"))
    )
  }
  else if (temp.dat$version[1]=="HRW_LN"){
    print(ggplot(data=dist.hl,
                 aes(x=trial, y=points, colour=arm)) + 
            geom_line(size=0.1) + ylim(0, 100) +
            geom_vline(colour="gray", 
                       xintercept=temp.dat$trial[temp.dat$switch==1], size=0.1) +
            labs(y="Payoff", x="Trial", fill="",
                 title=paste0("Participant ", temp.dat[1,2], ", Version ", 
                              temp.dat[1,1])) +
            theme_calc() + 
            scale_color_manual(values=c("#e24068", "#fa6c0c", "#509E75", "#508F9E"))
    )
  }
  else if (temp.dat$version[1]=="LRW_HN") {
    print(ggplot(data=dist.lh,
                 aes(x=trial, y=points, colour=arm)) + 
            geom_line(size=0.1) + ylim(0, 100) +
            geom_vline(colour="gray", 
                       xintercept=temp.dat$trial[temp.dat$switch==1], size=0.1) +
            labs(y="Payoff", x="Trial", fill="",
                 title=paste0("Participant ", temp.dat[1,2], ", Version ", 
                              temp.dat[1,1])) +
            theme_calc() + 
            scale_color_manual(values=c("#e24068", "#fa6c0c", "#509E75", "#508F9E"))
    )
  }
  else {
    print(ggplot(data=dist.ll,
                 aes(x=trial, y=points, colour=arm)) + 
            geom_line(size=0.1) + ylim(0, 100) +
            geom_vline(colour="gray", 
                       xintercept=temp.dat$trial[temp.dat$switch==1], size=0.1) +
            labs(y="Payoff", x="Trial", fill="",
                 title=paste0("Participant ", temp.dat[1,2], ", Version ", 
                              temp.dat[1,1])) +
            theme_calc() + 
            scale_color_manual(values=c("#e24068", "#fa6c0c", "#509E75", "#508F9E"))
    )
  }
}
dev.off()

# -------------------- ANOVA --------------------

lm.switch <- lm(n.switches ~version, data=dat.collect)

leveneTest(n.switches ~ version, data = dat.collect)

aov.switch <- aov(lm.switch)
summary(aov.switch)
names(dat.collect)
summaryBy(data = dat.collect, n.switches ~ version)
TukeyHSD(aov.switch)

lm.expl.switch <- lm(n.expl.switches ~version, data=dat.collect)

leveneTest(n.expl.switches ~ version, data = dat.collect)

aov.expl.switch <- aov(lm.expl.switch)
summary(aov.expl.switch)
names(dat.collect)
summaryBy(data = dat.collect, n.expl.switches ~ version)
TukeyHSD(aov.expl.switch)

