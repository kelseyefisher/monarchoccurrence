#####################################

#home
#setwd("C:/Users/kelse/Box Sync/Publications/19&20 Habitat Utilization/Data Analysis")
#work
setwd("C:/Users/kefisher/Box/Publications/19&20 Habitat Utilization/Data Analysis")
summary<-read.csv("1920_trialsummary.csv", header=TRUE)
#monarchs in the trial summary (delete those outside of temperature thresholds)
monarchs<-read.csv("1920_trialmonarchs.csv", header=TRUE)

step<-read.csv("1920_locations_steplengths.csv", header=TRUE)
step<-merge(monarchs,step,by="monarchrunid")
step<-step[!(step$crosswithin==""),]

library(ggplot2)
library(grid)
library(dplyr)
library(lubridate)
library(circular)
library(emmeans)
library(lattice)
library(Rmisc)
library(multcomp)
library(gridExtra)

########################################

#released on edge monarchs
names(step)
edge<-subset(step, release == "edge")
unique(edge$monarchrunid)
nrow(edge)

#released on edge cross to prairie at some point
edgetorich<-subset(edge, crossto == "rich")
unique(edgetorich$monarchrunid)
nrow(edgetorich)

#from edge first cross into 
edge1st<-read.csv("1920_locations_steplengths_fromedge.csv",header=TRUE)
edge1st<-merge(monarchs,edge1st,by="monarchrunid")
edge1st<-edge1st[!(edge1st$X1ststep==""),]
table(edge1st$X1ststep)

##########################################

#monarchs to cross based on release habitat
releasehab<-c("corn","east","edge","grass","prairie","west")
spatialconfig<-c("mixed","poor","mixed","mixed","mixed","rich")
released<-c(18,19,19,20,18,20)
cross<-c(16,16,14,15,10,17)
monarchscross<-data.frame(releasehab,spatialconfig,released,cross)
monarchscross

table(summary$Release, summary$fly)

monarchscross$percross<-monarchscross$cross/monarchscross$released
chisq.test(monarchscross$releasehab, monarchscross$percross, correct=FALSE)
chisq.test(monarchscross$spatialconfig, monarchscross$percross, correct=FALSE)

################################
#number of times to cross habitat boundaries

stepcross<-subset(step, crosswithinexit == "cross")
names(stepcross)
unique(stepcross$crosswithinexit)

unique(stepcross$monarchrunid)
timescross<-table(stepcross$monarchrunid, stepcross$crosswithinexit)
timescross<-as.data.frame(timescross)
head(timescross)
names(timescross)[1]<-"monarchrunid"
names(timescross)[2]<-"stepclass"

timescross<-merge(timescross,summary,by="monarchrunid")

names(timescross)
unique(timescross$Release)


library(emmeans)
freqcross<-glm(Freq~Release,data=timescross,family = gaussian(link = "identity"))
emm<-emmeans(freqcross,"Release",type="response")
joint_tests(emm)
pairs(emm)
emm
CLD(emm)
#pwpp(emm)


SE<- summarySE(timescross, measurevar="Freq", groupvars=c("Release"))
SE
SE$Sig<- NA
SE$Sig<- c("a","a","a","a","a","b")
SE$class <-c("Crop","Roadside without Resource-Rich","Edge","Resource-Poor","Resource-Rich","Roadside with Resource-Rich")
SE$spatialconfig<-c("Mixed Land Use","Roadside without Resource-Rich","Mixed Land Use","Mixed Land Use","Mixed Land Use","Roadside with Resource-Rich")
#SE2$avg<-c("77.5m", "10.3m")
#SE2$p <-c("p < 0.0001", "")
SE$n<- c("n=16", "n=16", "n=14", "n=15", "n=10","n=17")
SE


SE$class<-factor(SE$class, levels=c("Crop","Resource-Rich","Resource-Poor","Edge","Roadside without Resource-Rich","Roadside with Resource-Rich"))
SE$spatialconfig<-factor(SE$spatialconfig, levels=c("Mixed Land Use", "Roadside without Resource-Rich", "Roadside with Resource-Rich"))

library(ggplot2)
pal<-c("grey75","grey43","grey15")

timescrossplot<-ggplot(SE, aes(y=Freq, x=class, width=.8))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  #scale_fill_manual(values=pal,
                    #name="Spatial Configuration",
                    #breaks=c("Mixed Land Use", "Roadside without Resource-Rich", "Roadside with Resource-Rich"),
                    #labels=c("Mixed Land Use", "Roadside without Resource-Rich", "Roadside with Resource-Rich"))+
  xlab("Release Habitat") +
  ylab("Times to Cross Habitat Boundary")+
  theme_bw()+
  geom_errorbar(aes(ymin=Freq-sd, ymax=Freq+sd), width=.2, 
                position=position_dodge(.8))+
  geom_text(aes(label=Sig, y=Freq+(sd)),vjust=-1.5, size=5, position=position_dodge(.8))+
  scale_y_continuous(expand=c(0,0), limits=c(0,6.1))+
  scale_x_discrete(labels=label_wrap_gen(width=12))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 14, vjust=-5))+
  #geom_text(aes(label = p), vjust=-13, hjust=2.75,size=7)+
  #geom_text(aes(label=n, y=steplength),color="white",fontface="bold",vjust=1.15, hjust=-.1,size=4,position=position_dodge(.8))+
  #ggtitle("a.")+
  theme(plot.title=element_text(size=18))+
  theme(legend.position = "null")
timescrossplot

####
#cross from each by release

stepcross<-subset(step, crosswithinexit == "cross")
names(stepcross)
unique(stepcross$crosswithinexit)

unique(stepcross$monarchrunid)
crossfrom<-table(stepcross$monarchrunid, stepcross$crossfrom)
crossfrom<-as.data.frame(crossfrom)
head(crossfrom)
names(crossfrom)[1]<-"monarchrunid"
names(crossfrom)[2]<-"crossfrom"

from<-merge(crossfrom,summary,by="monarchrunid")
head(from)

names(from)
unique(from$Release)


freqfrom<-glm(Freq~Release+crossfrom,data=from,family = gaussian(link = "identity"))
emm<-emmeans(freqfrom,c("Release","crossfrom"),type="response")
joint_tests(emm)
pairs(emm)
emm
CLD(emm)
pwpp(emm)


SE2<- summarySE(from, measurevar="Freq", groupvars=c("Release","crossfrom"))
SE2
#SE$Sig<- NA
#SE$Sig<- c("a","a","a","a","a","b")
SE2$Release2 <-c("Crop Field","Crop Field","Crop Field", "Roadside without Resource-Rich","Roadside without Resource-Rich","Roadside without Resource-Rich", "Edge","Edge","Edge", "Resource-Poor","Resource-Poor","Resource-Poor", "Resource-Rich","Resource-Rich","Resource-Rich", "Roadside with Resource-Rich","Roadside with Resource-Rich","Roadside with Resource-Rich")
SE2$crossfrom2<-c("Crop Field", "Resource-Poor","Resource-Rich","Crop Field", "Resource-Poor","Resource-Rich","Crop Field", "Resource-Poor","Resource-Rich","Crop Field", "Resource-Poor","Resource-Rich","Crop Field", "Resource-Poor","Resource-Rich","Crop Field", "Resource-Poor","Resource-Rich")
#SE2$avg<-c("77.5m", "10.3m")
#SE2$p <-c("p < 0.0001", "")
#SE$n<- c("n=16", "n=16", "n=14", "n=15", "n=10","n=17")
SE2
SE2$sdbelow<-c(0.5000000,0,0.5773503,0.37500000,0,0,0.6112498 ,0.07142857 ,0.5789342 ,0.0666666,0,0.13333333 ,0.20000000 ,0.10000000 ,0,0.82352941,0.8488747 ,0.64705882)

SE2$Release2<-factor(SE2$Release2, levels=c("Crop Field","Resource-Rich","Resource-Poor","Edge","Roadside without Resource-Rich","Roadside with Resource-Rich"))
#SE$spatialconfig<-factor(SE$spatialconfig, levels=c("Mixed Land Use", "Roadside without Resource-Rich", "Roadside with Resource-Rich"))

library(ggplot2)
pal<-c("grey75","grey43","grey15")

fromplot<-ggplot(SE2, aes(fill=crossfrom2, y=Freq, x=Release2, width=.8))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  scale_fill_manual(values=pal,
                    name="Exiting",
                    breaks=c("Crop Field", "Resource-Poor", "Resource-Rich"),
                    labels=c("Crop Field", "Resource-Poor", "Resource-Rich"))+
  xlab("Release Habitat") +
  ylab("Times to Exit")+
  theme_bw()+
  geom_errorbar(aes(ymin=Freq-sdbelow, ymax=Freq+sd), width=.2, 
                position=position_dodge(.8))+
  #geom_text(aes(label=Sig, y=Freq+(sd)),vjust=-1.5, size=5, position=position_dodge(.8))+
  scale_y_continuous(expand=c(0,0), limits=c(0,2.7))+
  scale_x_discrete(labels=label_wrap_gen(width=12))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  #theme(axis.text.x = element_text(size=10))+
  theme(axis.text.x=element_text(colour="black"))+
  #theme(axis.text.y = element_text(size=10))+
  theme(axis.text.y=element_text(colour="black"))+
  #theme(axis.title = element_text(size = 14, vjust=-5))+
  #geom_text(aes(label = p), vjust=-13, hjust=2.75,size=7)+
  #geom_text(aes(label=n, y=steplength),color="white",fontface="bold",vjust=1.15, hjust=-.1,size=4,position=position_dodge(.8))+
  ggtitle("b.")+
  theme(plot.title=element_text(size=18))+
  theme(legend.position = "bottom")
fromplot



####
#cross into each by release

stepcross<-subset(step, crosswithinexit == "cross")
names(stepcross)
unique(stepcross$crosswithinexit)

unique(stepcross$monarchrunid)
crossinto<-table(stepcross$monarchrunid, stepcross$crossto)
crossinto<-as.data.frame(crossinto)
head(crossinto)
names(crossinto)[1]<-"monarchrunid"
names(crossinto)[2]<-"crossto"

into<-merge(crossinto,summary,by="monarchrunid")
head(into)

names(into)
unique(into$Release)


freqinto<-glm(Freq~Release+crossto,data=into,family = gaussian(link = "identity"))
emm<-emmeans(freqinto,c("Release","crossto"),type="response")
joint_tests(emm)
pairs(emm)
emm
CLD(emm)
pwpp(emm)


SE3<- summarySE(into, measurevar="Freq", groupvars=c("Release","crossto"))
SE3
#SE$Sig<- NA
#SE$Sig<- c("a","a","a","a","a","b")
SE3$Release2 <-c("Crop Field","Crop Field","Crop Field","Crop Field", 
                 "Roadside without Resource-Rich","Roadside without Resource-Rich","Roadside without Resource-Rich","Roadside without Resource-Rich", 
                 "Edge","Edge","Edge", "Edge", 
                 "Resource-Poor","Resource-Poor","Resource-Poor", "Resource-Poor", 
                 "Resource-Rich","Resource-Rich","Resource-Rich","Resource-Rich", 
                 "Roadside with Resource-Rich","Roadside with Resource-Rich","Roadside with Resource-Rich","Roadside with Resource-Rich")
SE3$crossfrom2<-c("Crop Field", "Exit Survey Area", "Resource-Poor","Resource-Rich",
                  "Crop Field", "Exit Survey Area", "Resource-Poor","Resource-Rich",
                  "Crop Field", "Exit Survey Area", "Resource-Poor","Resource-Rich",
                  "Crop Field", "Exit Survey Area", "Resource-Poor","Resource-Rich",
                  "Crop Field", "Exit Survey Area", "Resource-Poor","Resource-Rich",
                  "Crop Field", "Exit Survey Area", "Resource-Poor","Resource-Rich")
#SE2$avg<-c("77.5m", "10.3m")
#SE2$p <-c("p < 0.0001", "")
#SE$n<- c("n=16", "n=16", "n=14", "n=15", "n=10","n=17")
SE3
SE3$sdbelow<-c(0.18750000,0.5000000,0.18750000,0.718795,
               0.2500000,0.18750000,0.25000000,0,
               0.3571428,0.50000000,0.0714285,0.633323,
               0.0666666,0.4666666,0.13333333,0.516397,
               0.30000000,0.483045,0.20000000,0.10000000,
               0.9275204,0.2941176,0.857492,0.8823529)

SE3$Release2<-factor(SE3$Release2, levels=c("Crop Field","Resource-Rich","Resource-Poor","Edge","Roadside without Resource-Rich","Roadside with Resource-Rich"))
SE3$crossfrom2<-factor(SE3$crossfrom2, levels=c("Crop Field", "Resource-Poor", "Resource-Rich","Exit Survey Area"))
#SE$spatialconfig<-factor(SE$spatialconfig, levels=c("Mixed Land Use", "Roadside without Resource-Rich", "Roadside with Resource-Rich"))

library(ggplot2)
pal<-c("grey75","grey43","grey15", "grey97")

toplot<-ggplot(SE3, aes(fill=crossfrom2, y=Freq, x=Release2, width=.8))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  scale_fill_manual(values=pal,
                    name="Entering",
                    breaks=c("Crop Field", "Resource-Poor", "Resource-Rich","Exit Survey Area"),
                    labels=c("Crop Field", "Resource-Poor", "Resource-Rich","Exit Survey Area"))+
  xlab("Release Habitat") +
  ylab("Times to Enter")+
  theme_bw()+
  geom_errorbar(aes(ymin=Freq-sdbelow, ymax=Freq+sd), width=.2, 
                position=position_dodge(.8))+
  #geom_text(aes(label=Sig, y=Freq+(sd)),vjust=-1.5, size=5, position=position_dodge(.8))+
  scale_y_continuous(expand=c(0,0), limits=c(0,2.7))+
  scale_x_discrete(labels=label_wrap_gen(width=12))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  #theme(axis.text.x = element_text(size=10))+
  theme(axis.text.x=element_text(colour="black"))+
  #theme(axis.text.y = element_text(size=10))+
  theme(axis.text.y=element_text(colour="black"))+
  #theme(axis.title = element_text(size = 14, vjust=-5))+
  #geom_text(aes(label = p), vjust=-13, hjust=2.75,size=7)+
  #geom_text(aes(label=n, y=steplength),color="white",fontface="bold",vjust=1.15, hjust=-.1,size=4,position=position_dodge(.8))+
  ggtitle("c.")+
  theme(plot.title=element_text(size=18))+
  theme(legend.position = "bottom")
toplot



grid.arrange(timescrossplot,fromplot, toplot)


##################
#habitat that inidiated "big steps"

ggplot(data=step, aes(x=steplength)) +
  geom_histogram(breaks=seq(0,200, by=10),
                 col="black",
                 fill="black",
                 alpha = .2) +
  xlab("Steplength")+ 
  ylab("Count")+
  theme_bw()+
  #ggtitle("Crop-Field")+
  #theme(plot.title=element_text(hjust = 0.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_y_continuous(expand=c(0,0), limits=c(0,600))+
  scale_x_continuous(expand=c(0,0), limits=c(0,310))+
  theme(axis.text.x = element_text(size=20))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=20))+
  theme(axis.text.y=element_text(colour="black"))+
  ggtitle("b.")+
  theme(plot.title=element_text(size=24))+
  theme(axis.title = element_text(size = 22))
#annotate("text", x=90, y=30, label="Poor-Roadside, Crop")#,size=7)

quanitles<-subset(step, select=c("steplength"))
summary(quanitles)

#steps from habitats with greater than 50m steps
step50<-subset(step,steplength>=50)
step50<-subset(step50,steplength<100)
step50$bigstepclass<-"50"
table(step50$crossfrom)

step100<-subset(step,steplength>=100)
step100<-subset(step100,steplength<250)
step100$bigstepclass<-"100"
table(step100$crossfrom)

step250<-subset(step,steplength>=250)
step250<-subset(step250,steplength<500)
step250$bigstepclass<-"250"
table(step250$crossfrom)

step500<-subset(step,steplength>=500)
step500<-subset(step500,steplength<1000)
step500$bigstepclass<-"500"
table(step500$crossfrom)

step1000<-subset(step,steplength>=1000)
step1000$bigstepclass<-"1000"
table(step1000$crossfrom)


###
#time before a "big step

bigstep<-rbind(step50,step100,step250,step500,step1000)
names(bigstep)


bigstepd<-glm(durationtobigstep~bigstepclass+crossfrom+spatialconfig,data=bigstep,family = gaussian(link = "identity"))
emm<-emmeans(bigstepd,c("bigstepclass","crossfrom","spatialconfig"),type="response")
joint_tests(emm)
pairs(emm)
emm
CLD(emm)
pwpp(emm)

bigstepd<-glm(durationtobigstep~spatialconfig,data=bigstep,family = gaussian(link = "identity"))
emm<-emmeans(bigstepd,c("spatialconfig"),type="response")



SE4<- summarySE(bigstep, measurevar="durationtobigstep", groupvars=c("crossfrom","spatialconfig"))
SE4
#SE$Sig<- NA
SE4$Sig<- c("","","b","","b","","a","")
SE4$sp2 <-c("Mosaic","Roadside without Resource-Rich","Roadside with Resource-Rich",
            "Mosaic","Roadside without Resource-Rich","Roadside with Resource-Rich",
            "Mosaic","Roadside with Resource-Rich")
SE4$crossfrom2<-c("Crop Field", "Crop Field","Crop Field",
                  "Resource-Poor","Resource-Poor","Resource-Poor",
                  "Resource-Rich","Resource-Rich")
#SE2$avg<-c("77.5m", "10.3m")
#SE2$p <-c("p < 0.0001", "")
#SE$n<- c("n=16", "n=16", "n=14", "n=15", "n=10","n=17")
SE4
SE4$sdbelow<-c(114.0000,930.8098,719.6673,823.54,1334.7435,757.7502,734.686,1105.9995)

SE4$sp2<-factor(SE4$sp2, levels=c("Mosaic","Roadside with Resource-Rich","Roadside without Resource-Rich"))
SE4$crossfrom2<-factor(SE4$crossfrom2, levels=c("Crop Field", "Resource-Poor", "Resource-Rich"))
#SE$spatialconfig<-factor(SE$spatialconfig, levels=c("Mixed Land Use", "Roadside without Resource-Rich", "Roadside with Resource-Rich"))

library(ggplot2)
pal<-c("grey75","grey43","grey15")

ggplot(SE4, aes(fill=crossfrom2, y=durationtobigstep, x=sp2, width=.8))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  scale_fill_manual(values=pal,
                    name="Initiating Habitat Class of Large Step",
                    breaks=c("Crop Field", "Resource-Poor", "Resource-Rich"),
                    labels=c("Crop Field", "Resource-Poor", "Resource-Rich"))+
  xlab("Habitat Configuration") +
  ylab("Time Prior to > 50 m Step (sec)")+
  theme_bw()+
  geom_errorbar(aes(ymin=durationtobigstep-sdbelow, ymax=durationtobigstep+sd), width=.2, 
                position=position_dodge(.8))+
  geom_text(aes(label=Sig, y=durationtobigstep+(sd)),vjust=-1.5, size=5, position=position_dodge())+
  scale_y_continuous(expand=c(0,0), limits=c(0,3500))+
  scale_x_discrete(labels=label_wrap_gen(width=12))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 14, vjust=-5))+
  #geom_text(aes(label = p), vjust=-13, hjust=2.75,size=7)+
  #geom_text(aes(label=n, y=steplength),color="white",fontface="bold",vjust=1.15, hjust=-.1,size=4,position=position_dodge(.8))+
  #ggtitle("c.")+
  theme(plot.title=element_text(size=18))+
  theme(legend.position = "bottom")



#################
#time in each habitat

names(step)
head(step)

timesec<-aggregate(durationsec~monarchrunid+release+landcoverclassification+spatialconfig,step,sum)

time <- glm(durationsec ~ landcoverclassification+spatialconfig, data=timesec, family = gaussian(link = "identity"))
emm <- emmeans(time, c("landcoverclassification","spatialconfig"), type='response')
joint_tests(emm)
pairs(emm)
emm
CLD(emm)

time <- glm(durationsec ~ landcoverclassification, data=timesec, family = gaussian(link = "identity"))
emm <- emmeans(time, c("landcoverclassification"), type='response')
joint_tests(emm)
pairs(emm)
emm
CLD(emm)


library(lattice)
library(Rmisc)
SE6<- summarySE(timesec, measurevar="durationsec", groupvars=c("landcoverclassification","spatialconfig"))
SE6
SE6$Sig<- c("","a","","","","b","","b")
SE6$land<-c("Crop Field","Crop Field","Crop Field",
            "Resource-Poor","Resource-Poor","Resource-Poor",
            "Resource-Rich","Resource-Rich")
SE6$sdbelow<-c(517.0000,620.8890,337.8765,
               774.2168,625.1965,966.2464,
               837.9071,839.2222)


SE6$spatialconfig<-factor(SE6$spatialconfig, levels=c("mixed", "poor-roadside", "rich-roadside"))
SE6$land<-factor(SE6$land, levels=c("Crop Field", "Resource-Poor", "Resource-Rich"))
#SE$spatialconfig<-factor(SE$spatialconfig, levels=c("Mixed Land Use", "Roadside without Resource-Rich", "Roadside with Resource-Rich"))


library(ggplot2)
pal<-c("grey75","grey43","grey15")

ggplot(SE6, aes(fill=spatialconfig, y=durationsec, x=land, width=.8))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  scale_fill_manual(values=pal,
                    name="Habitat Configuration",
                    breaks=c("mixed", "poor-roadside", "rich-roadside"),
                    labels=c("Mosaic", "Roadside without Resource-Rich", "Roadside with Resource-Rich"))+
  xlab("Habitat Classification") +
  ylab("Mean Duration (sec)")+
  theme_bw()+
  geom_errorbar(aes(ymin=durationsec-sdbelow, ymax=durationsec+sd), width=.2, 
                position=position_dodge(.8))+
  geom_text(aes(label=Sig, y=durationsec+(sd)),vjust=-1.5, size=5, position=position_dodge())+
  scale_y_continuous(expand=c(0,0), limits=c(0,2600))+
  scale_x_discrete(labels=label_wrap_gen(width=12))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 14, vjust=-5))+
  #geom_text(aes(label = p), vjust=-13, hjust=2.75,size=7)+
  #geom_text(aes(label=n, y=steplength),color="white",fontface="bold",vjust=1.15, hjust=-.1,size=4,position=position_dodge(.8))+
  #ggtitle("c.")+
  theme(plot.title=element_text(size=18))+
  theme(legend.position = "bottom")
