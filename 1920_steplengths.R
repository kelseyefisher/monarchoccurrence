setwd("C:/Users/kefisher/Box/Publications/19&20 Habitat Utilization/Data Analysis")
step<-read.csv("1920_locations_steplengths.csv", header=TRUE)
monarchs<-read.csv("1920_trialmonarchs.csv", header=TRUE)
step<-merge(monarchs,step,by="monarchrunid")
step<-step[!(step$crosswithin==""),]

unique(step$monarchrunid)

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

###########################################################
#histogram of all steps
a<-ggplot(data=step, aes(x=steplength)) +
  geom_histogram(breaks=seq(0,100, by=10),
                 col="black",
                 fill="black",
                 alpha = .2) +
  xlab("")+ 
  ylab("Count")+
  theme_bw()+
  #ggtitle("Crop-Field")+
  #theme(plot.title=element_text(hjust = 0.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_y_continuous(expand=c(0,0), limits=c(0,550))+
  scale_x_continuous(expand=c(0,0), limits=c(0,102))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.y=element_text(colour="black"))+
  ggtitle("a.")+
  theme(plot.title=element_text(size=14))+
  theme(axis.title = element_text(size = 13))
#annotate("text", x=90, y=30, label="Poor-Roadside, Crop")#,size=7)


b<-ggplot(data=step, aes(x=steplength)) +
  geom_histogram(breaks=seq(100,2000, by=10),
                 col="black",
                 fill="black",
                 alpha = .2) +
  xlab("Step ength (m)")+ 
  ylab("Count")+
  theme_bw()+
  #ggtitle("Crop-Field")+
  #theme(plot.title=element_text(hjust = 0.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_y_continuous(expand=c(0,0), limits=c(0,3.5))+
  scale_x_continuous(expand=c(0,0), limits=c(99,2050), breaks=c(100,500,1000,1500,2000))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.y=element_text(colour="black"))+
  ggtitle("b.")+
  theme(plot.title=element_text(size=14))+
  theme(axis.title = element_text(size = 13))
#annotate("text", x=90, y=30, label="Poor-Roadside, Crop")#,size=7)
b
grid.arrange(a,b)

min(step$steplength)
max(step$steplength)

count(step$steplength <= 50)

###########################################################
#steps across vs steps within

crosswithin <- glm(steplength ~ crosswithinexit + spatialconfig, data=step, family = gaussian(link = "identity"))
emm <- emmeans(crosswithin, c("crosswithinexit","spatialconfig"), type='response')
joint_tests(emm)

crosswithin <- glm(steplength ~ crosswithinexit, data=step, family = gaussian(link = "identity"))
emm <- emmeans(crosswithin, c("crosswithinexit"), type='response')
joint_tests(emm)
pairs(emm)
emm
CLD(emm)
#pwpp(dtm.emm2)

SE<- summarySE(step, measurevar="steplength", groupvars=c("crosswithinexit", "spatialconfig"))
SE
SE$Sig<- NA
SE$Sig<- c("","","b","","","a")
SE$class <-c("Cross Habitat Boundary","Cross Habitat Boundary","Cross Habitat Boundary","Within Habitat","Within Habitat","Within Habitat")
#SE2$avg<-c("77.5m", "10.3m")
#SE2$p <-c("p < 0.0001", "")
SE$n<- c("n=83", "n=22", "n=54", "n=428", "n=92","n=111")
SE

SE$class<-factor(SE$class, levels=c("Within Habitat","Cross Habitat Boundary"))
SE$spatialconfig<-factor(SE$spatialconfig, levels=c("mixed", "rich-roadside", "poor-roadside"))


library(ggplot2)
pal<-c("grey15","grey43","grey70","grey15","grey43","grey70")
crosswithinplot<-ggplot(SE, aes(fill=spatialconfig, y=steplength, x=class, width=.8))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  scale_fill_manual(values=pal,
                    name="Habitat Configuration",
                    breaks=c("mixed","rich-roadside","poor-roadside"),
                    labels=c("Mosaic", "Roadside with Resource-Rich","Roadside without Resource-Rich"))+
  xlab("Step Category") +
  ylab("Step Length (m)")+
  theme_bw()+
  geom_errorbar(aes(ymin=steplength-steplength, ymax=steplength+sd), width=.2, 
                position=position_dodge(.8))+
  geom_text(aes(label=Sig, y=steplength+(sd)),vjust=-1.5, size=5, position=position_dodge(.8))+
  scale_y_continuous(expand=c(0,0), limits=c(0,490))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  #theme(axis.text.x = element_text(size=10))+
  theme(axis.text.x=element_text(colour="black"))+
  #theme(axis.text.y = element_text(size=10))+
  theme(axis.text.y=element_text(colour="black"))+
  scale_x_discrete(labels=label_wrap_gen(width=14))+
  #theme(axis.title = element_text(size = 14, vjust=-5))+
  #geom_text(aes(label = p), vjust=-13, hjust=2.75,size=7)+
  #geom_text(aes(label=n, y=steplength),color="white",fontface="bold",vjust=1.15, hjust=-.1,size=4,position=position_dodge(.8))+
  ggtitle("a.")+
  theme(plot.title=element_text(size=18))+
  theme(legend.position = "NULL")
crosswithinplot


########################################################
#within

stepwithin<-subset(step, crosswithin == "within")

mean(stepwithin$steplength)
sd(stepwithin$steplength)

within <- glm(steplength ~ landcoverclassification + spatialconfig, data=stepwithin, family = gaussian(link = "identity"))
emm <- emmeans(within, c("landcoverclassification","spatialconfig"), type='response')
joint_tests(emm)
pairs(emm)
emm
CLD(emm)
#pwpp(dtm.emm2)

within <- glm(steplength ~ spatialconfig, data=stepwithin, family = gaussian(link = "identity"))
emm <- emmeans(within, c("spatialconfig"), type='response')
joint_tests(emm)
pairs(emm)
emm
CLD(emm)
#pwpp(dtm.emm2)


SE2<- summarySE(stepwithin, measurevar="steplength", groupvars=c("landcoverclassification", "spatialconfig"))
SE2
SE2$Sig<- NA
SE2$Sig<- c("ab","a","ab","b","ab","ab","b","ab")
SE2$class <-c("Within Crop Field", "Within Crop Field", "Within Crop Field", "Within Resource-Poor", "Within Resource-Poor","Within Resource-Poor", "Within Resource-Rich", "Within Resource-Rich")
#SE2$avg<-c("77.5m", "10.3m")
#SE2$p <-c("p < 0.0001", "")
SE2$n<- c("n=46", "n=36", "n=25", "n=131", "n=56","n=70","n=251","n=16")
SE2

SE2$class<-factor(SE2$class, levels=c("Within Resource-Rich","Within Resource-Poor", "Within Crop Field"))
SE2$spatialconfig<-factor(SE2$spatialconfig, levels=c("mixed", "rich-roadside", "poor-roadside"))


library(ggplot2)
pal<-c("grey15","grey43","grey70","grey15","grey43","grey70","grey15","grey70")
withinplot<-ggplot(SE2, aes(fill=spatialconfig, y=steplength, x=class, width=.8))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  scale_fill_manual(values=pal,
                    name="Habitat Configuration",
                    breaks=c("mixed","rich-roadside","poor-roadside"),
                    labels=c("Mosaic", "Roadside with Resource-Rich", "Roadside without Resource-Rich"))+
  xlab("Step Category") +
  ylab("Step Length (m)")+
  theme_bw()+
  geom_errorbar(aes(ymin=steplength-steplength, ymax=steplength+sd), width=.2, 
                position=position_dodge(.8))+
  #geom_text(aes(label=Sig, y=steplength+(sd)),vjust=-1.5, size=5, position=position_dodge(.8))+
  scale_y_continuous(expand=c(0,0), limits=c(0,200))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  #theme(axis.text.x = element_text(size=10))+
  theme(axis.text.x=element_text(colour="black"))+
  #theme(axis.text.y = element_text(size=10))+
  theme(axis.text.y=element_text(colour="black"))+
  scale_x_discrete(labels=label_wrap_gen(width=12))+
  #theme(axis.title = element_text(size = 14, vjust=-5))+
  #geom_text(aes(label = p), vjust=-13, hjust=2.75,size=7)+
  #geom_text(aes(label=n, y=steplength),color="white",fontface="bold",vjust=1.15, hjust=-.1,size=4,position=position_dodge(.8))+
  ggtitle("b.")+
  theme(plot.title=element_text(size=18))+
  theme(legend.position = "NULL")
withinplot


##############################################
#cross
stepcross<-subset(step, crosswithinexit == "cross")
stepcross<-stepcross[!(stepcross$crossto==""),]

mean(stepcross$steplength)
sd(stepcross$steplength)
unique(stepcross$monarchrunid)

#cross to
crossto <- glm(steplength ~ crossto + spatialconfig, data=stepcross, family = gaussian(link = "identity"))
emm <- emmeans(crossto, c("crossto","spatialconfig"), type='response')
joint_tests(emm)

crossto <- glm(steplength ~ crossto, data=stepcross, family = gaussian(link = "identity"))
emm <- emmeans(crossto, c("crossto"), type='response')
joint_tests(emm)
pairs(emm)
emm
CLD(emm)
#pwpp(dtm.emm2)

summarySE(stepcross, measurevar="steplength", groupvars=c("crossto"))

SE3<- summarySE(stepcross, measurevar="steplength", groupvars=c("crossto", "spatialconfig"))
SE3
SE3$Sig<- NA
SE3$Sig<- c("","","a","","","b","","","","","")
SE3$Sig2<- c("","","","","","","","a","","","a")
SE3$class <-c("Enter Crop Field", "Enter Crop Field", "Enter Crop Field", "Exit Survey Area","Exit Survey Area","Exit Survey Area", "Enter Resource-Poor", "Enter Resource-Poor","Enter Resource-Poor", "Enter Resource-Rich", "Enter Resource-Rich")
#SE3$avg<-c("77.5m", "10.3m")
#SE3$p <-c("p < 0.0001", "")
SE3$n<- c("n=12", "n=15", "n=19", "n=31", "n=3","n=5","n=8", "n=4", "n=15", "n=32", "n=15")
SE3

SE3$class<-factor(SE3$class, levels=c("Enter Resource-Rich","Enter Resource-Poor", "Enter Crop Field", "Exit Survey Area"))
SE3$spatialconfig<-factor(SE3$spatialconfig, levels=c("mixed", "rich-roadside", "poor-roadside"))

SE3


library(ggplot2)
pal<-c("grey15","grey43","grey70","grey15","grey43","grey70","grey15","grey43","grey70","grey15","grey70")
crosstoplot<-ggplot(SE3, aes(fill=spatialconfig, y=steplength, x=class, width=.8))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  scale_fill_manual(values=pal,
                    name="Habitat Configuration",
                    breaks=c("mixed","poor-roadside","rich-roadside"),
                    labels=c("Mosaic", "Roadside without Resource-Rich", "Roadside with Resource-Rich"))+
  xlab("Step Category") +
  ylab("Step Length (m)")+
  theme_bw()+
  geom_errorbar(aes(ymin=steplength-steplength, ymax=steplength+sd), width=.2, 
                position=position_dodge(.8))+
  geom_text(aes(label=Sig, y=steplength+(sd)),vjust=-1.5, size=5, position=position_dodge(.8))+
  geom_text(aes(label=Sig2, y=steplength+(sd)),vjust=-1.5, size=5, position=position_dodge(0))+
  scale_y_continuous(expand=c(0,0), limits=c(0,1800))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  #theme(axis.text.x = element_text(size=10))+
  theme(axis.text.x=element_text(colour="black"))+
  #theme(axis.text.y = element_text(size=10))+
  theme(axis.text.y=element_text(colour="black"))+
  scale_x_discrete(labels=label_wrap_gen(width=12))+
  #theme(axis.title = element_text(size = 14, vjust=-5))+
  #geom_text(aes(label = p), vjust=-13, hjust=2.75,size=7)+
  #geom_text(aes(label=n, y=steplength),color="white",fontface="bold",vjust=1.15, hjust=-.1,size=4,position=position_dodge(.8))+
  ggtitle("d.")+
  theme(plot.title=element_text(size=18))+
  theme(legend.position = "NULL")
crosstoplot

################
#cross from
crossfrom <- glm(steplength ~ crossfrom + spatialconfig, data=stepcross, family = gaussian(link = "identity"))
emm <- emmeans(crossfrom, c("crossfrom","spatialconfig"), type='response')
joint_tests(emm)

crossfrom <- glm(steplength ~ crossfrom, data=stepcross, family = gaussian(link = "identity"))
emm <- emmeans(crossfrom, c("crossfrom"), type='response')
joint_tests(emm)
pairs(emm)
emm
CLD(emm)
#pwpp(dtm.emm2)

summarySE(stepcross, measurevar="steplength", groupvars=c("crossfrom"))


SE4<- summarySE(stepcross, measurevar="steplength", groupvars=c("crossfrom", "spatialconfig"))
SE4
SE4$Sig<- NA
#SE4$Sig<- c("","","b","","","b","","")
SE4$Sig2<- c("","b","","b","","","","a")
SE4$class <-c("Exit Crop Field", "Exit Crop Field", "Exit Crop Field", "Exit Resource-Poor", "Exit Resource-Poor","Exit Resource-Poor", "Exit Resource-Rich", "Exit Resource-Rich")
#SE4$avg<-c("77.5m", "10.3m")
#SE4$p <-c("p < 0.0001", "")
SE4$n<- c("n=31", "n=6", "n=14", "n=17", "n=16","n=29","n=35", "n=11")
SE4

SE4$class<-factor(SE4$class, levels=c("Exit Resource-Rich","Exit Resource-Poor", "Exit Crop Field"))
SE4$spatialconfig<-factor(SE4$spatialconfig, levels=c("mixed", "rich-roadside", "poor-roadside"))


library(ggplot2)
pal<-c("grey15","grey43","grey70","grey15","grey43","grey70","grey15","grey70")
crossfromplot<-ggplot(SE4, aes(fill=spatialconfig, y=steplength, x=class, width=.8))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  scale_fill_manual(values=pal,
                    name="Habitat Configuration",
                    breaks=c("mixed","poor-roadside","rich-roadside"),
                    labels=c("Mosaic", "Roadside without Resource-Rich", "Roadside with Resource-Rich"))+
  xlab("Step Category") +
  ylab("Step Length (m)")+
  theme_bw()+
  geom_errorbar(aes(ymin=steplength-steplength, ymax=steplength+sd), width=.2, 
                position=position_dodge(.8))+
  #geom_text(aes(label=Sig, y=steplength+(sd)),vjust=-1.5, size=5, position=position_dodge(.8))+
  geom_text(aes(label=Sig2, y=steplength+(sd)),vjust=-1.5, size=5, position=position_dodge(0))+
  scale_y_continuous(expand=c(0,0), limits=c(0,1800))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  #theme(axis.text.x = element_text(size=10))+
  theme(axis.text.x=element_text(colour="black"))+
  #theme(axis.text.y = element_text(size=10))+
  theme(axis.text.y=element_text(colour="black"))+
  scale_x_discrete(labels=label_wrap_gen(width=12))+
  #theme(axis.title = element_text(size = 14, vjust=-5))+
  #geom_text(aes(label = p), vjust=-13, hjust=2.75,size=7)+
  #geom_text(aes(label=n, y=steplength),color="white",fontface="bold",vjust=1.15, hjust=-.1,size=4,position=position_dodge(.8))+
  ggtitle("c.")+
  theme(plot.title=element_text(size=18))+
  theme(legend.position = "bottom")
crossfromplot


grid.arrange(crosswithinplot,withinplot,crossfromplot,crosstoplot)








###########################################################################################

orient<-read.csv("1920_orient.csv",header=TRUE)
names(stepcross)
unique(stepcross$crossto)
names(orient)
steporient<-merge(stepcross,orient, by="monarchrunid")
torich<-subset(steporient, crossto == "rich")
unique(torich$monarchrunid)

##all steps to rich
min(torich$steplength)
max(torich$steplength)

torichplot<-ggplot(data=torich, aes(x=steplength)) +
  geom_histogram(breaks=seq(0,130, by=10),
                 col="black",
                 fill="black",
                 alpha = .2) +
  xlab("Step Length Entering Resource-Rich Habitat (m)")+ 
  ylab("Count")+
  theme_bw()+
  #ggtitle("Crop-Field")+
  #theme(plot.title=element_text(hjust = 0.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_y_continuous(expand=c(0,0), limits=c(0,15))+
  scale_x_continuous(expand=c(0,0), limits=c(0,130))+
  theme(axis.text.x = element_text(size=20))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=20))+
  theme(axis.text.y=element_text(colour="black"))+
  ggtitle("a.")+
  theme(plot.title=element_text(size=24))+
  theme(axis.title = element_text(size = 22))
  #annotate("text", x=90, y=30, label="Poor-Roadside, Crop")#,size=7)
torichplot


#to rich into wind

torich$windcorforstep <- (torich$bearing - torich$windbearing)
torich$adjstepbear <- ifelse(torich$windcorforstep <0, torich$windcorforstep +360, 
                            ifelse(torich$windcorforstep >360, torich$windcorforstep -360, torich$windcor))
torich$adjstepbeardeg<-circular(torich$adjstepbear, units="degrees", template="geographics")
torichinwind<-torich %>% filter((adjstepbeardeg >=315) | (adjstepbeardeg <=45))

nrow(torichinwind)
unique(torichinwind$monarchrunid)

##all steps to rich
min(torichinwind$steplength)
max(torichinwind$steplength)

torichinwindplot<-ggplot(data=torichinwind, aes(x=steplength)) +
  geom_histogram(breaks=seq(0,130, by=10),
                 col="black",
                 fill="black",
                 alpha = .2) +
  xlab("Step Length Entering Resource-Rich \n Habitat & into Wind (m)")+ 
  ylab("Count")+
  theme_bw()+
  #ggtitle("Crop-Field")+
  #theme(plot.title=element_text(hjust = 0.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_y_continuous(expand=c(0,0), limits=c(0,15))+
  scale_x_continuous(expand=c(0,0), limits=c(0,130))+
  theme(axis.text.x = element_text(size=20))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=20))+
  theme(axis.text.y=element_text(colour="black"))+
  ggtitle("b.")+
  theme(plot.title=element_text(size=24))+
  theme(axis.title = element_text(size = 22))
#annotate("text", x=90, y=30, label="Poor-Roadside, Crop")#,size=7)
torichinwindplot

quanitles<-subset(torichinwind, select=c("steplength"))
summary(quanitles)

grid.arrange(torichplot,torichinwindplot)



