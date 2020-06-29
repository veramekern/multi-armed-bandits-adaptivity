rm(list = ls())
# rm(points.dists)
#  "psych", "car", "MASS", "nlme",  
# "Hmisc", "stringr", "multcomp", "gridExtra", "doBy", 
# "reshape2", "tibble", "DescTools", 
# "ggridges", "NCmisc"

# ---------- INIT ---------- 
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = T)
  sapply(pkg, require, character.only = T)
}
packages <- c("tidyverse", "plotly", "ggplot2", "gridExtra", "matrixStats", 
              "ggthemes", "psych")
ipak(packages)

# ---------- LOAD DATA ---------- 
# # TO LOAD DATA FROM RAW DATA FILE
# dat.eprime <- read_delim(
#   file="../../../03-data/chapter-3-study-1-raw-data-eprime.csv", delim = ";")
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
# # exclude participants due to recent drug use, psychiatric history, non-nativity
# exclude.demo <- read_delim(
#   file="../../../03-data/chapter-3-study-1-exclude-demo.csv", delim=";")
# dat.full <- left_join(dat.eprime, exclude.demo, by="id")
# dat.full <- subset(dat.full, exclude==0)
# length(unique(dat.full$id)) # down to N = 66
# dat.full <- dat.full[complete.cases(dat.full),] # remove practice trials
# dat.full <- subset(dat.full, select=-exclude) # remove exclude variable
# write_csv(dat.full, path="../../../03-data/chapter-3-study-1-full-data.csv")

# TO LOAD FROM CLEANED FILE (EXCLUDED SOME PARTICIPANTS based on demographics)
dat.full <- read_csv(file="../../../03-data/chapter-3-study-1-full-data.csv", 
                     skip=5)

# # exclude participants due to recent drug use, psychiatric history, non-nativity
# # exclude participant 104 for random playing
# # exclude participant 64 for only playing two machine arms
# exclude <- read.csv(file=paste("../data/erc-wp1-bp76-exclude-mab.csv",
#                                sep=""), sep=";", dec=".")
# dat <- left_join(dat, exclude, by="id")
# dat <- subset(dat, exclude==0)
# length(unique(dat$id)) # down to N = 64
# dat <- dat[complete.cases(dat),] # remove practice trials
# dat <- subset(dat, select=-exclude) # remove exclude variable
# write.table(dat, file=paste0("../../../03-data/chapter-3-study-1-full-data.csv"),
#             row.names=F, col.names = T, sep=";", dec=".")
# 
# # TO LOAD FROM CLEANED FILE (EXCLUDED SOME PARTICIPANTS)
# dat <- read.csv(file=paste0("../../../03-data/chapter-3-study-1-full-data.csv"),
#                 sep=";", dec=".")

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
            rt.start.cluster = sum(rt * cluster.start)/n.clusters)

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
            rt.start.cluster = sum(rt * cluster.start)/n.clusters)

# calculate how many times arm was best arm per version
table.best <- dat %>%
  group_by(version, trial) %>%
  summarise(best = mean(best.possible))
split.table.best <- split(x=table.best, f=table.best$version)

dist.best.a <- c(79, 0, 70, 51)
dist.best.b <- c(47, 120, 25, 8)

rm(table.best, split.table.best)

# show distribution choice pp
n.chose.1 <- dat %>%
  group_by(id) %>%
  filter(choice==1) %>%
  tally(choice)

n.chose.2 <- dat %>%
  group_by(id) %>%
  filter(choice==2) %>%
  tally(choice)
n.chose.2$n <- n.chose.2$n/2

dat.dist.arms <- full_join(n.chose.1, n.chose.2, by="id")

n.chose.3 <- dat %>%
  group_by(id) %>%
  filter(choice==3) %>%
  tally(choice)
n.chose.3$n <- n.chose.3$n/3

dat.dist.arms <- full_join(dat.dist.arms, n.chose.3, by="id")

n.chose.4 <- dat %>%
  group_by(id) %>%
  filter(choice==4) %>%
  tally(choice)
n.chose.4$n <- n.chose.4$n/4

dat.dist.arms <- full_join(dat.dist.arms, n.chose.4, by="id")

names(dat.dist.arms) <- c("id", "observed.1", "observed.2", "observed.3", 
                          "observed.4")
version <- select(dat.collect, id, version)
dat.dist.arms <- full_join(version, dat.dist.arms, by="id")
rm(n.chose.1, n.chose.2, n.chose.3, n.chose.4, version)

# add expected distribution pp
dat.expected <- dat.dist.arms %>%
  summarise(case_when(version=="a" ~ dist.best.a, 
                      version=="b" ~ dist.best.b)) 
names(dat.expected) <- c("id", "expected.choice")
arm <- rep(c(1,2,3,4), length(unique(dat$id)))
dat.expected <- cbind(dat.expected, arm)
names(dat.expected)[3] <- "arm"
dat.expected <- spread(dat.expected, arm, expected.choice)
names(dat.expected) <- c("id", "expected.1", "expected.2", "expected.3", 
                         "expected.4")

dat.dist.arms <- full_join(dat.dist.arms, dat.expected, by="id")
rm(dat.expected, arm, dist.best.a, dist.best.b)

# add random expected distribution pp
r.1 <- r.2 <- r.3 <- r.4 <- rep(50, length(unique(dat$id)))
dat.unif.expected <- data.frame(unif.1=r.1, unif.2=r.2, unif.3=r.3, unif.4=r.4)
dat.dist.arms <- cbind(dat.dist.arms, dat.unif.expected)
dat.collect <- left_join(dat.collect, dat.dist.arms, by="id")
dat.collect <- select(dat.collect, -version.y)
names(dat.collect)[2] <- 'version'
rm(r.1, r.2, r.3, r.4, dat.unif.expected)

dat.collect <- dat.collect %>% mutate(observed.3 = replace_na(observed.3, 0))
dat.collect <- dat.collect %>% mutate(observed.4 = replace_na(observed.4, 0))

# perform chi sq per pp to check how much they deviate from best machine choice 
# in distribution
chisq <- dat.collect %>%
  rowwise() %>% 
  mutate(
    chisq.test.stat.expected = chisq.test(c(observed.1:observed.4, 
                                            expected.1:expected.4))$statistic,
    chisq.p.val.expected = chisq.test(c(observed.1:observed.4,
                                        expected.1:expected.4))$p.value
  )


# perform chi sq per pp to check how much they deviate from 
# uniform distribution
chisq <- chisq %>%
  rowwise() %>% 
  mutate(
    chisq.test.stat.unif = chisq.test(c(observed.1:observed.4, 
                                        unif.1:unif.4))$statistic,
    chisq.p.val.unif = chisq.test(c(observed.1:observed.4,unif.1:unif.4))$p.value
  )

# ---------- VISUAL EXPLORATION ---------- 

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
dist.a <- filter(long.payoff.dist, version == 'a')
dist.b <- filter(long.payoff.dist, version == 'b')

# # plot switches per participant to pdf
# # split data per id
# split.dat <- split(dat, dat$id)
# names.dat <- colnames(dat)
# 
# #prepare pdf
# pdf("../../results/switches-excluded-demo.pdf")
# 
# #plot to pdf
# for (i in 1:length(unique(dat$id))) {
#   temp.dat <- data.frame(split.dat[i])
#   names(temp.dat) <- names.dat
#   if (temp.dat$version[1]=="a") {
#     print(ggplot(data=dist.a,
#                  aes(x=trial, y=points, colour=arm)) +
#             geom_line(size=0.1) + ylim(0, 100) +
#             geom_vline(colour="gray",
#                        xintercept=temp.dat$trial[temp.dat$switch==1], size=0.1) +
#             labs(y="Payoff", x="Trial", fill="",
#                  title=paste0("Participant ", temp.dat[1,2])) +
#             theme_calc() +
#             scale_color_manual(values=c("#e24068", "#fa6c0c", "#509E75", "#508F9E"))
#     )
#   }
#   else {
#     print(ggplot(data=dist.b,
#                  aes(x=trial, y=points, colour=arm)) +
#             geom_line(size=0.1) + ylim(0, 100) +
#             geom_vline(colour="gray",
#                        xintercept=temp.dat$trial[temp.dat$switch==1], size=0.1) +
#             labs(y="Payoff", x="Trial", fill="",
#                  title=paste0("Participant ", temp.dat[1,2])) +
#             theme_calc() +
#             scale_color_manual(values=c("#e24068", "#fa6c0c", "#509E75", "#508F9E"))
#     )
#   }
# }
# dev.off()
# 
# # plot best arm chosen per participant to pdf
# # split data per id
# split.dat <- split(dat, dat$id)
# names.dat <- colnames(dat)
# 
# #prepare pdf
# pdf("../../results/best-choice-excluded-demo.pdf")
# 
# #plot to pdf
# for (i in 1:length(unique(dat$id))) {
#   temp.dat <- data.frame(split.dat[i])
#   names(temp.dat) <- names.dat
#   if (temp.dat$version[1]=="a") {
#     print(ggplot(data=dist.a,
#                  aes(x=trial, y=points, colour=arm)) +
#             geom_line(size=0.1) + ylim(0, 100) +
#             geom_vline(colour="gray",
#                        xintercept=temp.dat$trial[temp.dat$best.chosen==1],
#                        size=0.1) + labs(y="Payoff", x="Trial", fill="",
#                                         title=paste0("Participant ",
#                                                      temp.dat[1,2])) +
#             theme_calc() +
#             scale_color_manual(values=c("#e24068", "#fa6c0c",
#                                         "#509E75", "#508F9E"))
#     )
#   }
#   else {
#     print(ggplot(data=dist.b,
#                  aes(x=trial, y=points, colour=arm)) +
#             geom_line(size=0.1) + ylim(0, 100) +
#             geom_vline(colour="gray",
#                        xintercept=temp.dat$trial[temp.dat$best.chosen==1],
#                        size=0.1) + labs(y="Payoff", x="Trial", fill="",
#                                         title=paste0("Participant ",
#                                                      temp.dat[1,2])) +
#             theme_calc() +
#             scale_color_manual(values=c("#e24068", "#fa6c0c",
#                                         "#509E75", "#508F9E"))
#     )
#   }
# }
# dev.off()


# ---------- DESCRIPTIVES ---------- 
### descriptive statistics of the payoff distributions ###
payoff.dist.noise.sep %>%
  group_by(version) %>%
  summarise(sd.arm1 = sd(arm1), 
            sd.arm2 = sd(arm2), 
            sd.arm3 = sd(arm3), 
            sd.arm4 = sd(arm4), 
            sd.noise = sd(noise), 
            mssd.arm1 = mssd(arm1),
            mssd.arm2 = mssd(arm2),
            mssd.arm3 = mssd(arm3),
            mssd.arm4 = mssd(arm4))


### descriptive plots full data ###
### violin plots ###

# number of switches
switch.violin <- dat.collect %>%
  ggplot(aes(x=version, y=n.switches, fill=version)) +
  geom_violin(draw_quantiles=T, alpha=0.5) +
  scale_fill_manual(values=c("#0596F7", "#F74A05")) +
  theme_minimal() +
  theme(legend.position="none") 
# ggplotly(switch.violin)

# number of clusters
clust.violin <-  dat.collect %>%
  ggplot(aes(x=version, y=n.clusters, fill=version)) +
  geom_violin(draw_quantiles=T, alpha=0.5) +
  scale_fill_manual(values=c("#0596F7", "#F74A05")) +
  theme_minimal() +
  theme(legend.position="none") 
# ggplotly(clust.violin)

# performance in total payoff
perf.violin <-  dat.collect %>%
  ggplot(aes(x=version, y=performance, fill=version)) +
  geom_violin(draw_quantiles=T, alpha=0.5) +
  scale_fill_manual(values=c("#0596F7", "#F74A05")) +
  theme_minimal() +
  theme(legend.position="none") 
# ggplotly(perf.violin)

# performance in best arm chosen
perf.arm.violin <-  dat.collect %>%
  ggplot(aes(x=version, y=p.correct.arm, fill=version)) +
  geom_violin(draw_quantiles=T, alpha=0.5) +
  scale_fill_manual(values=c("#0596F7", "#F74A05")) +
  theme_minimal() 
# ggplotly(perf.arm.violin)

grid.arrange(switch.violin, clust.violin, perf.violin, perf.arm.violin, ncol=4)

### descriptive plots data minus first 50 trials ###
### violin plots ###

# number of switches
switch.violin.150 <- dat.collect.150 %>%
  ggplot(aes(x=version, y=n.switches, fill=version)) +
  geom_violin(draw_quantiles=T, alpha=0.5) +
  scale_fill_manual(values=c("#0596F7", "#F74A05")) +
  theme_minimal() +
  theme(legend.position="none") 
# ggplotly(switch.violin)

# number of clusters
clust.violin.150 <-  dat.collect.150 %>%
  ggplot(aes(x=version, y=n.clusters, fill=version)) +
  geom_violin(draw_quantiles=T, alpha=0.5) +
  scale_fill_manual(values=c("#0596F7", "#F74A05")) +
  theme_minimal() +
  theme(legend.position="none") 
# ggplotly(clust.violin)

# performance in total payoff
perf.violin.150 <-  dat.collect.150 %>%
  ggplot(aes(x=version, y=performance, fill=version)) +
  geom_violin(draw_quantiles=T, alpha=0.5) +
  scale_fill_manual(values=c("#0596F7", "#F74A05")) +
  theme_minimal() +
  theme(legend.position="none") 
# ggplotly(perf.violin)

# performance in best arm chosen
perf.arm.violin.150 <-  dat.collect.150 %>%
  ggplot(aes(x=version, y=p.correct.arm, fill=version)) +
  geom_violin(draw_quantiles=T, alpha=0.5) +
  scale_fill_manual(values=c("#0596F7", "#F74A05")) +
  theme_minimal()
# ggplotly(perf.arm.violin)

grid.arrange(switch.violin.150, clust.violin.150, perf.violin.150, 
             perf.arm.violin.150, ncol=4)

### descriptive plots data minus first 100 trials ###
### violin plots ###

switch.violin.100 <- dat.collect.100 %>%
  ggplot(aes(x=version, y=n.switches, fill=version)) +
  geom_violin(draw_quantiles=T, alpha=0.5) +
  scale_fill_manual(values=c("#0596F7", "#F74A05")) +
  theme_minimal() +
  theme(legend.position="none") 
# ggplotly(switch.violin)

# number of clusters
clust.violin.100 <-  dat.collect.100 %>%
  ggplot(aes(x=version, y=n.clusters, fill=version)) +
  geom_violin(draw_quantiles=T, alpha=0.5) +
  scale_fill_manual(values=c("#0596F7", "#F74A05")) +
  theme_minimal() +
  theme(legend.position="none") 
# ggplotly(clust.violin)

# performance in total payoff
perf.violin.100 <-  dat.collect.100 %>%
  ggplot(aes(x=version, y=performance, fill=version)) +
  geom_violin(draw_quantiles=T, alpha=0.5) +
  scale_fill_manual(values=c("#0596F7", "#F74A05")) +
  theme_minimal() +
  theme(legend.position="none") 
# ggplotly(perf.violin)

# performance in best arm chosen
perf.arm.violin.100 <-  dat.collect.100 %>%
  ggplot(aes(x=version, y=p.correct.arm, fill=version)) +
  geom_violin(draw_quantiles=T, alpha=0.5) +
  scale_fill_manual(values=c("#0596F7", "#F74A05")) +
  theme_minimal()
# ggplotly(perf.arm.violin)

grid.arrange(switch.violin.100, clust.violin.100, perf.violin.100, 
             perf.arm.violin.100, ncol=4)

# ---------- ANALYSES ---------- 

mod1 <- lm(p.correct.arm ~ n.switches, data=dat.collect)
cooksd <- cooks.distance(mod1)

sample_size <- nrow(dat.collect)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = 4/sample_size, col="red") 
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, 
                                                   names(cooksd),""), col="red")

