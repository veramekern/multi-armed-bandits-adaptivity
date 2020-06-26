# rm(list = ls())
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
packages <- c("dplyr", "plotly", "ggplot2", "gridExtra", "tidyr", "matrixStats")
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
# dat <- dat[complete.cases(dat),] # remove practice trials
# dat <- subset(dat, select=-exclude) # remove exclude variable
# write.table(dat, file=paste0("../../../03-data/chapter-3-study-1-full-data.csv"),
#             row.names=F, col.names = T, sep=";", dec=".")

# TO LOAD FROM CLEANED FILE (EXCLUDED SOME PARTICIPANTS)
dat <- read.csv(file=paste0("../../../03-data/chapter-3-study-1-full-data.csv"),
                sep=";", dec=".")

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

# # plot switches per participant to pdf
# # split data per id
# split.dat <- split(dat, dat$id)
# names.dat <- colnames(dat)
# 
# #prepare pdf
# pdf("switches.pdf") 
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

### violin plots ###

# number of switches
switch.violin <- dat.collect %>%
  ggplot(aes(x=version, y=n.switches, fill=version)) +
  geom_violin(draw_quantiles=T, alpha=0.5) +
  scale_fill_manual(values=c("#0596F7", "#F74A05")) +
  theme_minimal()
# ggplotly(switch.violin)

# number of clusters
clust.violin <-  dat.collect %>%
  ggplot(aes(x=version, y=n.clusters, fill=version)) +
  geom_violin(draw_quantiles=T, alpha=0.5) +
  scale_fill_manual(values=c("#0596F7", "#F74A05")) +
  theme_minimal()
# ggplotly(clust.violin)

# performance in total payoff
perf.violin <-  dat.collect %>%
  ggplot(aes(x=version, y=performance, fill=version)) +
  geom_violin(draw_quantiles=T, alpha=0.5) +
  scale_fill_manual(values=c("#0596F7", "#F74A05")) +
  theme_minimal()
# ggplotly(perf.violin)

# performance in best arm chosen
perf.arm.violin <-  dat.collect %>%
  ggplot(aes(x=version, y=p.correct.arm, fill=version)) +
  geom_violin(draw_quantiles=T, alpha=0.5) +
  scale_fill_manual(values=c("#0596F7", "#F74A05")) +
  theme_minimal()
# ggplotly(perf.arm.violin)

grid.arrange(switch.violin, clust.violin, perf.violin, perf.arm.violin, ncol=2)

# ---------- DESCRIPTIVES ---------- 
# calculate how many times arm was best arm per version

table.best <- dat %>%
  group_by(version, trial) %>%
  summarise(best = mean(best.possible))
split.table.best <- split(x=table.best, f=table.best$version)

best.a <- c(79, 0, 70, 51)
best.b <- c(47, 120, 25, 8)

# show distribution choice pp
dat.guess.1 <- dat %>%
  group_by(id) %>%
  filter(choice==1) %>%
  tally(choice)

dat.guess.2 <- dat %>%
  group_by(id) %>%
  filter(choice==2) %>%
  tally(choice)
dat.guess.2$n <- dat.guess.2$n/2

dat.guess <- full_join(dat.guess.1, dat.guess.2, by="id")

dat.guess.3 <- dat %>%
  group_by(id) %>%
  filter(choice==3) %>%
  tally(choice)
dat.guess.3$n <- dat.guess.3$n/3

dat.guess <- full_join(dat.guess, dat.guess.3, by="id")

dat.guess.4 <- dat %>%
  group_by(id) %>%
  filter(choice==4) %>%
  tally(choice)
dat.guess.4$n <- dat.guess.4$n/4

dat.guess.observed <- full_join(dat.guess, dat.guess.4, by="id")
names(dat.guess.observed) <- c("id", "n.1", "n.2", "n.3", "n.4")
version <- select(dat.collect, id, version)
dat.guess <- full_join(version, dat.guess.observed, by="id")

# add expected distribution pp
dat.guess.expected <- dat.guess %>%
  summarise(case_when(version=="a" ~ best.a, 
                      version=="b" ~ best.b)) 
names(dat.guess.expected) <- c("id", "expected.choice")
arm <- rep(c(1,2,3,4), 64)
dat.guess.expected <- cbind(dat.guess.expected, arm)
names(dat.guess.expected)[3] <- "arm"
dat.guess.expected <- spread(dat.guess.expected, arm, expected.choice)
names(dat.guess.expected) <- c("id", "e.1", "e.2", "e.3", "e.4")

head(dat.guess.expected)
head(dat.guess.observed)
dat.guess <- full_join(dat.guess.observed, dat.guess.expected, by="id")
head(dat.guess)

chisq <- dat.guess %>%
  rowwise() %>% 
  mutate(
    test_stat = chisq.test(c(n.1:n.4, e.1:e.4))$statistic,
    p_val = chisq.test(c(n.1:n.4, e.1:e.4))$p.value
  )
View(chisq)


chisq %>%
  ggplot( aes(x=p_val)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

dat.collect <- full_join(dat.collect, chisq, by="id")

# ---------- ANALYSES ---------- 


mod1 <- lm(p.correct.arm ~ n.switches, data=dat.collect)
cooksd <- cooks.distance(mod1)

sample_size <- nrow(dat.collect)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = 4/sample_size, col="red") 
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, 
                                                   names(cooksd),""), col="red")

names(dat.collect)

dat.collect %>%
  ggplot(aes(x=p_val, y=p.correct.arm)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  facet_wrap(~ version)

dat.collect %>%
  ggplot( aes(x=p_val)) +
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  facet_wrap(~ version)


# there is a huge difference between the two versions
# the relationship between some variables are much more logical in version a
# this might be due to the fact that there is a much more uniform distribution
# of which arm is the best choice, even though one arm is never the best choice
# in version b the best arm is arm 2 most of the time, over half of the trials
# apparently, though, people have a tendency to keep switching machines as the
# chisq values indicate that the distribution of their choices does not match 
# the disrtibution of the best arm, at all. 
# especially compared to version a
