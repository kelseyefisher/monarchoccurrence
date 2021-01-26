#setwd("C:/Users/kefisher/Box/ISU Research/2020/Summer Telemetry Analyses/CTMM 19")
#lab19<-read.csv("19_label.csv", header=TRUE)
#setwd("C:/Users/kefisher/Box/Publications/19&20 Habitat Utilization/Data Analysis")
#sum19 <- read.csv("19_trialsummary.csv", header=TRUE)
#sum20 <- read.csv("20_trialsummary.csv", header=TRUE)
#sum19b <- merge(lab19, sum19, by="monarchrunid")
#summary<-rbind(sum19b, sum20)
#names(summary)
#write.csv(summary,"1920_trialsummary.csv")

#home
setwd("C:/Users/kelse/Box Sync/Publications/19&20 Habitat Utilization/Data Analysis")

summary<-read.csv("1920_trialsummary.csv", header=TRUE)

#monarchs in the trial summary (delete those outside of temperature thresholds)
monarchs<-read.csv("1920_trialmonarchs.csv", header=TRUE)

library(dplyr)
library(circular)
library(ggplot2)
library(grid)
library(lubridate)
library(emmeans)
library(lattice)
library(Rmisc)
library(gridExtra)

#######################################################################################

#is there a difference in the number of duds based on tag, shamtag, or untag?
table(summary$attachment, summary$fly)
chisq.test(summary$attachment, summary$fly, correct=FALSE)

#dayscaptive?
table(summary$dayscaptive, summary$fly)
chisq.test(summary$dayscaptive, summary$fly, correct=FALSE)

#release?
table(summary$Release, summary$fly)
chisq.test(summary$Release, summary$fly, correct=FALSE)
#release 2019?
table(sum19b$Release, sum19b$fly)
chisq.test(sum19b$Release, sum19b$fly, correct=FALSE)
#a<-subset(sum19, Release == "corn" | Release == "edge")
#b<-subset(sum19, Release == "corn" | Release == "grass")
#c<-subset(sum19, Release == "corn" | Release == "prairie")
#d<-subset(sum19, Release == "edge" | Release == "grass")
#e<-subset(sum19, Release == "edge" | Release == "prairie")
#f<-subset(sum19, Release == "grass" | Release == "prairie")

#chisq.test(a$Release, a$fly, correct=FALSE)
#chisq.test(b$Release, b$fly, correct=FALSE)
#chisq.test(c$Release, c$fly, correct=FALSE)
#chisq.test(d$Release, d$fly, correct=FALSE)
#chisq.test(e$Release, e$fly, correct=FALSE)
#chisq.test(f$Release, f$fly, correct=FALSE)

#release 2020?
table(sum20$Release, sum20$fly)
chisq.test(sum20$Release, sum20$fly, correct=FALSE)