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
              "ggthemes", "psych", "biotools", "MASS", "lattice", "GGally", 
              "extrafont", "car")
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

# find n best arm switch
dat <- dat %>%
  mutate(best.switched = ifelse(best.possible != lag(best.possible), 1, 0))
dat$best.switched <- ifelse(is.na(dat$best.switched)==T, 0, dat$best.switched)

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
            rt.start.cluster = sum(rt * cluster.start)/n.clusters,
            n.best.switch = sum(best.switched))

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
            rt.start.cluster = sum(rt * cluster.start)/n.clusters,
            n.best.switch = sum(best.switched))

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
            rt.start.cluster = sum(rt * cluster.start)/n.clusters,
            n.best.switch = sum(best.switched))

dat.collect.100$cluster.size <- ifelse(dat.collect.100$cluster.size==Inf, 100, 
                                       dat.collect.100$cluster.size)

# calculate how many times arm was best arm per version
table.best <- dat %>%
  group_by(version, trial) %>%
  summarise(best = mean(best.possible))
count.best.choice <- table.best %>%
  group_by(version, best) %>%
  summarise(count=n())

count.best.choice <- spread(count.best.choice, best, count)
count.best.choice[is.na(count.best.choice)] <- 0

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
version <- dplyr::select(dat.collect, id, version)
dat.dist.arms <- full_join(version, dat.dist.arms, by="id")
rm(n.chose.1, n.chose.2, n.chose.3, n.chose.4, version)

# add expected distribution pp
dist.best.a <- as.list(count.best.choice[1,2:5])
dist.best.b <- as.list(count.best.choice[2,2:5])

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
dat.collect <- dplyr::select(dat.collect, -version.y)
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

# number of participants in each version
dat.collect %>%
  group_by(version) %>%
  summarise(n = length(unique(id)))

### descriptive plots full data ###
# line plots payoff distributions
plot.a <- ggplot(data=dist.a, aes(x=trial, y=points, colour=arm)) +
  geom_line(size=0.5) + ylim(0, 100)  +
  labs(title="Point distribution per arm", subtitle="Version A",
       y="Points Earned on Trial", x="Trial Number", color="Arm") +
  scale_color_manual(values=c("#1fc5c3", "#3b8c84", "#ff101f", "#edd83d"), 
                     labels=c("Arm 1", "Arm 2", "Arm 3", "Arm 4"))

plot.b <- ggplot(data=dist.b, aes(x=trial, y=points, colour=arm)) +
  geom_line(size=0.5) + ylim(0, 100)  +
  labs(title="Point distribution per arm", subtitle="Version B",
       y="Points Earned on Trial", x="Trial Number", color="Arm") +
  scale_color_manual(values=c("#1fc5c3", "#3b8c84", "#ff101f", "#edd83d"), 
                     labels=c("Arm 1", "Arm 2", "Arm 3", "Arm 4"))


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
  ggplot(aes(x=version, y=cluster.size, fill=version)) +
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
  ggplot(aes(x=version, y=cluster.size, fill=version)) +
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
  ggplot(aes(x=version, y=cluster.size, fill=version)) +
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
# mean differences between versions

# MANOVA
# assumptions
# normally distributed errors
n.err.man <- lm(data=dat.collect, p.correct.arm ~ n.switches + cluster.size)
errors <- data.frame(residuals(n.err.man))
names(errors)
n.err.man <- ggplot(errors, aes(x=residuals.n.err.man.)) + 
  geom_histogram()
rm(n.err.man, errors)

# homogeneity of var-covar matrices
# largest group != 1.5 times or more bigger than the smallest group: robust

# MANOVA
study.1.manova <- manova(cbind(n.switches, cluster.size, p.correct.arm) ~version, 
                      data=dat.collect)

summary(study.1.manova)

# linear discriminant analysis follow up MANOVA
dat.lda <- dplyr::select(dat.collect, version, n.switches, cluster.size, p.correct.arm)
ggpairs(dat.lda, aes(colour=version, alpha=0.4))

study.1.lda <- MASS::lda(version ~ n.switches + cluster.size + p.correct.arm, 
                         data=dat.lda) 
out.study.1.lda <- predict(study.1.lda)
plot(study.1.lda)
table(dat.lda$version == out.study.1.lda$class)

# univariate ANOVAs follow up MANOVA
summary.aov(study.1.manova)

# logistic regression to classify
dat.logit <- dat.lda
dat.logit$version <- recode(dat.logit$version, a=0, b=1)
study.1.logit <- glm(version ~ n.switches + cluster.size + p.correct.arm,
                  data=dat.logit, family=binomial())
summary(study.1.logit)
pred <- predict(study.1.logit)
probs <- exp(pred)/(1+exp(pred))
probs <- ifelse(probs <.50, 0, 1)
table(probs==dat.logit$version)

# regression
dat.collect.a <- filter(dat.collect, version=="a")
dat.collect.b <- filter(dat.collect, version=="b")
dat.collect.a.150 <- filter(dat.collect.150, version=="a")
dat.collect.b.150 <- filter(dat.collect.150, version=="b")
dat.collect.a.100 <- filter(dat.collect.100, version=="a")
dat.collect.b.100 <- filter(dat.collect.100, version=="b")

# plots n.switches
plot.study.1.lr <- ggplot(dat.collect, aes(n.switches, p.correct.arm)) + 
  geom_point(color="#797979") + 
  geom_smooth(method="lm", color="#1fc5c3", fill="#3b8c84", alpha=0.2, size=0.5) + 
  facet_wrap(~version) + 
  ggtitle(label="All Trials") + 
  xlab("number of switches") + 
  ylab("proportion correct arm")

plot.study.1.lr.150 <- ggplot(dat.collect.150, aes(n.switches, p.correct.arm)) + 
  geom_point(color="#797979") + 
  geom_smooth(method="lm", color="#1fc5c3", fill="#3b8c84", alpha=0.2, size=0.5) + 
  facet_wrap(~version) + 
  ggtitle(label="Last 150 Trials") + 
  xlab("number of switches") + 
  ylab("proportion correct arm") 

plot.study.1.lr.100 <- ggplot(dat.collect.100, aes(n.switches, p.correct.arm)) + 
  geom_point(color="#797979") + 
  geom_smooth(method="lm", color="#1fc5c3", fill="#3b8c84", alpha=0.2, size=0.5) + 
  facet_wrap(~version) + 
  ggtitle(label="Last 100 Trials") + 
  xlab("number of switches") + 
  ylab("proportion correct arm")
# + geom_text(x = 25, y = 300, label = lm_eqn(df), parse = TRUE)

grid.arrange(plot.study.1.lr, plot.study.1.lr.150, plot.study.1.lr.100)

# plots cluster size
plot.study.1.csize <- ggplot(dat.collect, aes(cluster.size, p.correct.arm)) + 
  geom_point(color="#797979") + 
  geom_smooth(method="lm", color="#1fc5c3", fill="#3b8c84", alpha=0.2, size=0.5) + 
  facet_wrap(~version) + 
  ggtitle(label="All Trials") + 
  xlab("average cluster size") + 
  ylab("proportion correct arm")

plot.study.1.csize.150 <- ggplot(dat.collect.150, aes(cluster.size, p.correct.arm)) + 
  geom_point(color="#797979") + 
  geom_smooth(method="lm", color="#1fc5c3", fill="#3b8c84", alpha=0.2, size=0.5) + 
  facet_wrap(~version) + 
  ggtitle(label="Last 150 Trials") + 
  xlab("average cluster size") + 
  ylab("proportion correct arm") 

plot.study.1.csize.100 <- ggplot(dat.collect.100, aes(cluster.size, p.correct.arm)) + 
  geom_point(color="#797979") + 
  geom_smooth(method="lm", color="#1fc5c3", fill="#3b8c84", alpha=0.2, size=0.5) + 
  facet_wrap(~version) + 
  ggtitle(label="Last 100 Trials") + 
  xlab("average cluster size") + 
  ylab("proportion correct arm")
# + geom_text(x = 25, y = 300, label = lm_eqn(df), parse = TRUE)

grid.arrange(plot.study.1.csize, plot.study.1.csize.150, plot.study.1.csize.100)


# REGRESSION VERSION A
# ---
# regression assumptions
study.1.glm.a <- glm(p.correct.arm ~ n.switches + cluster.size, data=dat.collect.a)

# homoscedasticity
pred <- scale(predict(study.1.glm.a))
resid <- scale(residuals(study.1.glm.a))
plot.tibble.a <- tibble(pred, resid)

ggplot(plot.tibble.a, aes(pred, resid)) +
  geom_point() + 
  geom_hline(yintercept = 0)

# outliers
glm.a.cooks <- tibble(dat.collect.a$id, cooks.distance(study.1.glm.a))
colnames(glm.a.cooks) <- c("id", "cooksd")
ggplot(glm.a.cooks, aes(id, cooksd)) +
  geom_point() + 
  geom_segment(aes(x=id, xend=id, y=cooksd, yend=cooksd)) +
  geom_text(aes(label=id), nudge_y=0.1, size=2) +
  geom_hline(yintercept=1, color="red") + 
  geom_hline(yintercept=4/(length(dat.collect$id)-3), color="blue")

# multicollinearity
vif(study.1.glm.a)
1/vif(study.1.glm.a) # tol

# normally distributed errors
ggplot(plot.tibble.a, aes(x=resid)) + 
  geom_density(fill="#3b8c84", color="#ffffff", alpha=0.5)

# regression summary
summary(study.1.glm.a)

# REGRESSION VERSION A MINUS 104
# ---
# regression assumptions
dat.collect.a.ro <- filter(dat.collect.a, id!=104)
study.1.glm.a.ro <- glm(p.correct.arm ~ n.switches + cluster.size, data=dat.collect.a.ro)

# homoscedasticity
pred.ro <- scale(predict(study.1.glm.a))
resid.ro <- scale(residuals(study.1.glm.a))
plot.tibble.a.ro <- tibble(pred.ro, resid.ro)

ggplot(plot.tibble.a.ro, aes(pred.ro, resid.ro)) +
  geom_point() + 
  geom_hline(yintercept = 0)

# outliers
glm.a.cooks.ro <- tibble(dat.collect.a.ro$id, cooks.distance(study.1.glm.a.ro))
colnames(glm.a.cooks.ro) <- c("id", "cooksd")
ggplot(glm.a.cooks.ro, aes(id, cooksd)) +
  geom_point() + 
  geom_segment(aes(x=id, xend=id, y=cooksd, yend=cooksd)) +
  geom_text(aes(label=id), nudge_y=0.1, size=2) +
  geom_hline(yintercept=1, color="red") + 
  geom_hline(yintercept=4/(length(dat.collect$id)-3), color="blue")

# multicollinearity
vif(study.1.glm.a.ro)
1/vif(study.1.glm.a.ro) # tol

# normally distributed errors
ggplot(plot.tibble.a.ro, aes(x=resid.ro)) + 
  geom_density(fill="#3b8c84", color="#ffffff", alpha=0.5)

# regression summary
summary(study.1.glm.a.ro)

# REGRESSION VERSION B
# ---
# regression assumptions
study.1.glm.b <- glm(p.correct.arm ~ n.switches + cluster.size, data=dat.collect.b)

# homoscedasticity - ok 
pred <- scale(predict(study.1.glm.b))
resid <- scale(residuals(study.1.glm.b))
plot.tibble.b <- tibble(pred, resid)

ggplot(plot.tibble.b, aes(pred, resid)) +
  geom_point() + 
  geom_hline(yintercept = 0)

# outliers - bad
glm.b.cooks <- tibble(dat.collect.b$id, cooks.distance(study.1.glm.b))
colnames(glm.b.cooks) <- c("id", "cooksd")
ggplot(glm.b.cooks, aes(id, cooksd)) +
  geom_point() + 
  geom_segment(aes(x=id, xend=id, y=cooksd, yend=cooksd)) +
  geom_text(aes(label=id), nudge_y=0.1, size=2) +
  geom_hline(yintercept=1, color="red") + 
  geom_hline(yintercept=4/(length(dat.collect$id)-3), color="blue")

# multicollinearity -good
vif(study.1.glm.b)
1/vif(study.1.glm.b) # tol

# normally distributed errors - meh
ggplot(plot.tibble.b, aes(x=resid)) + 
  geom_density(fill="#3b8c84", color="#ffffff", alpha=0.5)

# regression summary
summary(study.1.glm.b)

# REGRESSION VERSION A MINUS 112
# ---
# regression assumptions
dat.collect.b.ro <- filter(dat.collect.b, id!=112 & id!=14)
study.1.glm.b.ro <- glm(p.correct.arm ~ n.switches + cluster.size, data=dat.collect.b.ro)

# homoscedasticity
pred.ro <- scale(predict(study.1.glm.b))
resid.ro <- scale(residuals(study.1.glm.b))
plot.tibble.b.ro <- tibble(pred.ro, resid.ro)

ggplot(plot.tibble.b.ro, aes(pred.ro, resid.ro)) +
  geom_point() + 
  geom_hline(yintercept = 0)

# outliers
glm.b.cooks.ro <- tibble(dat.collect.b.ro$id, cooks.distance(study.1.glm.b.ro))
colnames(glm.b.cooks.ro) <- c("id", "cooksd")
ggplot(glm.b.cooks.ro, aes(id, cooksd)) +
  geom_point() + 
  geom_segment(aes(x=id, xend=id, y=cooksd, yend=cooksd)) +
  geom_text(aes(label=id), nudge_y=0.1, size=2) +
  geom_hline(yintercept=1, color="red") + 
  geom_hline(yintercept=4/(length(dat.collect$id)-3), color="blue")

# multicollinearity
vif(study.1.glm.b.ro)
1/vif(study.1.glm.b.ro) # tol

# normally distributed errors
ggplot(plot.tibble.b.ro, aes(x=resid.ro)) + 
  geom_histogram(fill="#3b8c84", color="#ffffff")

# regression summary
summary(study.1.glm.b.ro)

