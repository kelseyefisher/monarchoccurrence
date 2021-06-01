setwd("C:/Users/kefisher/Box/Publications/19&20 Habitat Utilization/Data Analysis")
summary<-read.csv("1920_trialsummary_4.csv", header=TRUE)
sumfly<-subset(summary, finaldud == "fly")
step<-read.csv("1920_locations_steplengths_6.csv", header=TRUE)
step2<-step[!(step$landonsteplength=="0"),]
step2<-step2[!is.na(step2$monarchrunid),]
step<-step[!(step$steplength=="0"),]
step<-step[!is.na(step$monarchrunid),]


names(step)
unique(step$stepover50)
stepu50<-subset(step, stepover50 == "under50")
stepo50<-subset(step, stepover50 == "over50")

nrow(step)
nrow(stepo50)
nrow(stepu50)
unique(stepu50$monarchrunid)
unique(stepo50$monarchrunid)


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
library(ggplot2)
library(car)
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
library(ggrepel)
library(ggplot2)
library(grid)
library(dplyr)
library(lubridate)
library(circular)


###########################################################
#histogram of all steps
a<-ggplot(data=step, aes(x=steplength)) +
  geom_histogram(breaks=seq(0,50, by=1),
                 col="black",
                 fill="black",
                 alpha = .2) +
  xlab("Step Length (m)")+ 
  ylab("Count")+
  theme_bw()+
  #ggtitle("Crop-Field")+
  #theme(plot.title=element_text(hjust = 0.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_y_continuous(expand=c(0,0), limits=c(0,115))+
  scale_x_continuous(expand=c(0,0), limits=c(0,51))+
  theme(axis.text.x = element_text(size=14))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=14))+
  theme(axis.text.y=element_text(colour="black"))+
  ggtitle("A.")+
  theme(plot.title=element_text(size=18))+
  theme(axis.title = element_text(size = 15))
#annotate("text", x=90, y=30, label="Poor-Roadside, Crop")#,size=7)
a

b<-ggplot(data=step, aes(x=steplength)) +
  geom_histogram(breaks=seq(51,2000, by=25),
                 col="black",
                 fill="black",
                 alpha = .2) +
  xlab("Step Length (m)")+ 
  ylab("Count")+
  theme_bw()+
  #ggtitle("Crop-Field")+
  #theme(plot.title=element_text(hjust = 0.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_y_continuous(expand=c(0,0), limits=c(0,22))+
  scale_x_continuous(expand=c(0,0), limits=c(50,2050), breaks=c(100,500,1000,1500,2000))+
  theme(axis.text.x = element_text(size=14))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=14))+
  theme(axis.text.y=element_text(colour="black"))+
  ggtitle("B.")+
  theme(plot.title=element_text(size=18))+
  theme(axis.title = element_text(size = 15))
#annotate("text", x=90, y=30, label="Poor-Roadside, Crop")#,size=7)
b
grid.arrange(a,b)

min(step$steplength)
max(step$steplength)
median(step$steplength)

count(step$steplength <= 50)


###############################################

names(within)
unique(step$stepwithincross)
within<-subset(step, stepwithincross == "within")
min(within$steplength)
max(within$steplength)
median(within$steplength)
unique(within$withinhab)
nrow(within)

withinp<-subset(within, withinhab == "rich")
min(withinp$steplength)
max(withinp$steplength)
median(withinp$steplength)
unique(withinp$monarchrunid)
nrow(withinp)
count(withinp$steplength <= 5)
count(withinp$steplength <= 50)


withinc<-subset(within, withinhab == "crop")
min(withinc$steplength)
max(withinc$steplength)
median(withinc$steplength)
unique(withinc$monarchrunid)
nrow(withinc)
count(withinc$steplength <= 5)
count(withinc$steplength <= 50)

withine<-subset(within, withinhab == "edge")
min(withine$steplength)
max(withine$steplength)
median(withine$steplength)
unique(withine$monarchrunid)
nrow(withine)
count(withine$steplength <= 5)
count(withine$steplength <= 50)

withing<-subset(within, withinhab == "poor")
min(withing$steplength)
max(withing$steplength)
median(withing$steplength)
unique(withing$monarchrunid)
nrow(withing)
count(withing$steplength <= 5)
count(withing$steplength <= 50)


cross<-subset(step, stepwithincross == "cross")
min(cross$steplength)
max(cross$steplength)
median(cross$steplength)
unique(cross$monarchrunid)
nrow(cross)
nrow(step)
count(cross$steplength >= 50)

cross2<-subset(cross, crosstohab == "rich"|crosstohab=="edge")
min(cross2$steplength)
max(cross2$steplength)

cross3<-subset(cross, crosstohabexit =="exit")
nrow(cross3)
unique(cross3$monarchrunid)
count(cross3$crossfromhab == "rich")
count(cross3$crossfromhab == "edge")
count(cross3$crossfromhab == "crop")
count(cross3$crossfromhab == "poor")


names(cross)
unique(cross3$crossfromhab)

###displacement steps
#within
chisq.test(c(51,18,10,21))#
chisq.test(c(51,18))#
chisq.test(c(51,10))#
chisq.test(c(51,21))#
chisq.test(c(18,10))#
chisq.test(c(18,21))#
chisq.test(c(10,21))#


#how many steps within habitat vs crossing
#steps across vs steps within

#within
chisq.test(c(81,64,215,227))#
chisq.test(c(81,64))
chisq.test(c(81,215))#
chisq.test(c(81,227))#
chisq.test(c(64,215))#
chisq.test(c(64,227))#
chisq.test(c(215,227))

#enter
chisq.test(c(35,69,33,64))#
chisq.test(c(35,69))#
chisq.test(c(35,33))
chisq.test(c(35,64))#
chisq.test(c(69,33))#
chisq.test(c(69,64))
chisq.test(c(33,64))#

#exit
chisq.test(c(28,64,63,48))#
chisq.test(c(28,64))#
chisq.test(c(28,63))#
chisq.test(c(28,48))
chisq.test(c(64,63))
chisq.test(c(64,48))
chisq.test(c(63,48))

0.05/7
#<0.007


cat<-c("Within","Within","Within","Within","Exit","Exit","Exit","Exit","Enter","Enter","Enter","Enter")
habitat<-c("ZD","HE","LD","HD","ZD","HE","LD","HD","ZD","HE","LD","HD")
count<-c(81,64,215,227,28,64,63,48,35,69,33,64)
sigwithin<-c("b","b","a","a","","","","","","","","")
sigexit<-c("","","","","b","a","a","ab","","","","")
sigenter<-c("","","","","","","","","b","a","b","a")
exitenter<-data.frame(cat,habitat,count)
exitenter

exitenter$cat<-factor(exitenter$cat, levels=c("Within", "Exit", "Enter"))
exitenter$habitat<-factor(exitenter$habitat, levels=c("HD", "LD", "ZD", "HE"))

#for manuscript
ggplot(exitenter, aes(fill=habitat, y=count, x=cat, width=.8))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Movement Category") +
  ylab("Count")+
  theme_bw()+
  scale_fill_brewer(palette="YlGnBu")+
  scale_y_continuous(expand=c(0,0), limits=c(0,260))+
  geom_text(aes(label=sigwithin, y=(count+2)),colour="darkolivegreen4",vjust=-1.5, size=5, position=position_dodge(.8),fontface="bold")+
  geom_text(aes(label=sigexit, y=(count+2)),colour="deepskyblue3",vjust=-1.5, size=5, position=position_dodge(.8),fontface="italic")+
  geom_text(aes(label=sigenter, y=(count+2)),colour="darkslategray",vjust=-1.5, size=5, position=position_dodge(.8),fontface="plain")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=14))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=14))+
  theme(axis.text.y=element_text(colour="black"))+
  scale_x_discrete(labels=label_wrap_gen(width=10))+
  theme(plot.title=element_text(size=18))+
  theme(axis.title = element_text(size = 16))+
  #ggtitle("c.")+
  theme(legend.position = "bottom")+
  guides(fill=guide_legend(title="Habitat Class"))+
  theme(legend.text = element_text(size = 14))+
  theme(legend.title = element_text(size = 14))+
  theme(legend.box.background=element_rect(colour = "black"),
        legend.background = element_blank())

#for supplemental
ggplot(exitenter, aes(fill=cat, y=count, x=habitat, width=.8))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Habitat Class") +
  ylab("Count")+
  theme_bw()+
  scale_fill_brewer(palette="YlGnBu")+
  scale_y_continuous(expand=c(0,0), limits=c(0,260))+
  #geom_text(aes(label=sigwithin, y=(count+2)),colour="darkolivegreen4",vjust=-1.5, size=5, position=position_dodge(.8),fontface="bold")+
  #geom_text(aes(label=sigexit, y=(count+2)),colour="darkslategray",vjust=-1.5, size=5, position=position_dodge(.8),fontface="italic")+
  #geom_text(aes(label=sigenter, y=(count+2)),colour="deepskyblue3",vjust=-1.5, size=5, position=position_dodge(.8),fontface="plain")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=14))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=14))+
  theme(axis.text.y=element_text(colour="black"))+
  scale_x_discrete(labels=label_wrap_gen(width=10))+
  theme(plot.title=element_text(size=18))+
  theme(axis.title = element_text(size = 16))+
  #ggtitle("c.")+
  theme(legend.position = "bottom")+
  guides(fill=guide_legend(title="Movement Category"))+
  theme(legend.text = element_text(size = 14))+
  theme(legend.title = element_text(size = 14))+
  theme(legend.box.background=element_rect(colour = "black"),
        legend.background = element_blank())

###############################

####steplengths
##within vs cross
names(step)
step2<-step[step$steplength!=0,]
unique(step2$stepwithincross)
step2<-step2[!is.na(step2$stepwithincross),]

mean(step2$steplength[step2$stepwithincross == "within"])
sd(step2$steplength[step2$stepwithincross == "within"])
mean(step2$steplength[step2$stepwithincross == "cross"])
sd(step2$steplength[step2$stepwithincross == "cross"])

crosswithin<-glm(steplength ~ stepwithincross + spatialconfig, data=step2, family= gaussian(link = "identity"))
emm <- emmeans(crosswithin, c("stepwithincross","spatialconfig"), type='response')
joint_tests(emm)
emm <- emmeans(crosswithin, c("stepwithincross"), type='response')
joint_tests(emm)
pairs(emm)
emm
CLD(emm)

SE<- summarySE(step2, measurevar="steplength", groupvars=c("stepwithincross"))
SE
SE$Sig<- c("b","a")
SE$class <-c("Cross Habitat Boundary","Within Habitat")
SE

SE$class<-factor(SE$class, levels=c("Within Habitat","Cross Habitat Boundary"))

a<-ggplot(SE, aes(y=steplength, x=class, width=.8))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Movement Category") +
  ylab("Step Length (m)")+
  theme_bw()+
  scale_fill_brewer(palette="YlGnBu")+
  geom_errorbar(aes(ymin=steplength-steplength, ymax=steplength+sd), width=.2, 
                position=position_dodge(.8))+
  geom_text(aes(label=Sig, y=steplength+sd),vjust=-1.5, size=5, position=position_dodge(.8))+
  scale_y_continuous(expand=c(0,0), limits=c(0,575))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=14))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=14))+
  theme(axis.text.y=element_text(colour="black"))+
  scale_x_discrete(labels=label_wrap_gen(width=10))+
  theme(plot.title=element_text(size=18))+
  theme(axis.title = element_text(size = 16))+
  ggtitle("A.")+
  theme(legend.position = "NULL")
a


#############################################
#steps within habitat
names(step)
step3<-step[step$steplength!=0,]
step3<-step3[step3$withinhab!=0,]
step3<-step3[!is.na(step3$withinhab),]
unique(step3$withinhab)

# mean(step2$steplength[step2$stepwithincross == "within"])
# sd(step2$steplength[step2$stepwithincross == "within"])
# mean(step2$steplength[step2$stepwithincross == "cross"])
# sd(step2$steplength[step2$stepwithincross == "cross"])

within<-glm(steplength ~ withinhab + spatialconfig, data=step3, family= gaussian(link = "identity"))
emm <- emmeans(within, c("withinhab","spatialconfig"), type='response')
joint_tests(emm)
emm <- emmeans(within, c("spatialconfig"), type='response')
joint_tests(emm)
pairs(emm)
emm
CLD(emm)
emm <- emmeans(within, c("withinhab"), type='response')
joint_tests(emm)
pairs(emm)
emm
CLD(emm)

SE3<- summarySE(step3, measurevar="steplength", groupvars=c("withinhab"))
SE3
SE3$Sig3<- c("","","","")
SE3$class <-c("Within","Within","Within","Within")
SE3$Habitat<-c("Crop","Edge","Resource-Poor","Resource-Rich")
SE3

SE3b<-summarySE(step3, measurevar="steplength", groupvars=c("spatialconfig"))
SE3b

#############################################
#steps exit habitat
names(step)
step4<-step[step$steplength!=0,]
step4<-step4[step4$crossfromhab!=0,]
step4<-step4[!is.na(step4$crossfromhab),]
unique(step4$crossfromhab)

mean(step4$steplength[step4$crossfromhab == "crop"])
sd(step4$steplength[step4$crossfromhab == "crop"])
mean(step4$steplength[step4$crossfromhab == "rich"])
sd(step4$steplength[step4$crossfromhab == "rich"])
mean(step4$steplength[step4$crossfromhab == "edge"])
sd(step4$steplength[step4$crossfromhab == "edge"])
mean(step4$steplength[step4$crossfromhab == "poor"])
sd(step4$steplength[step4$crossfromhab == "poor"])


from<-glm(steplength ~ crossfromhab + spatialconfig, data=step4, family= gaussian(link = "identity"))
emm <- emmeans(from, c("crossfromhab","spatialconfig"), type='response')
joint_tests(emm)
emm <- emmeans(from, c("crossfromhab"), type='response')
joint_tests(emm)
pairs(emm)
emm
CLD(emm)

SE4<- summarySE(step4, measurevar="steplength", groupvars=c("crossfromhab"))
SE4
SE4$Sig4<- c("ab","ab","b","a")
SE4$class <-c("Exit","Exit","Exit","Exit")
SE4$Habitat<-c("Crop","Edge","Resource-Poor","Resource-Rich")
SE4

SE4b<- summarySE(step4, measurevar="steplength", groupvars=c("spatialconfig"))
SE4b

#############################################
#steps enter habitat
names(step)
step5<-step[step$steplength!=0,]
step5<-step5[step5$crosstohab!=0,]
step5<-step5[!is.na(step5$crosstohab),]
unique(step5$crosstohab)

mean(step5$steplength[step5$crosstohab == "crop"])
sd(step5$steplength[step5$crosstohab == "crop"])
mean(step5$steplength[step5$crosstohab == "rich"])
sd(step5$steplength[step5$crosstohab == "rich"])
mean(step5$steplength[step5$crosstohab == "edge"])
sd(step5$steplength[step5$crosstohab == "edge"])
mean(step5$steplength[step5$crosstohab == "poor"])
sd(step5$steplength[step5$crosstohab == "poor"])


to<-glm(steplength ~ crosstohab + spatialconfig, data=step5, family= gaussian(link = "identity"))
emm <- emmeans(to, c("crosstohab","spatialconfig"), type='response')
joint_tests(emm)
emm <- emmeans(to, c("crosstohab"), type='response')
joint_tests(emm)
pairs(emm)
emm
CLD(emm)

SE5<- summarySE(step5, measurevar="steplength", groupvars=c("crosstohab"))
SE5
SE5$Sig5<- c("ab","b","a","ab")
SE5$class <-c("Enter","Enter","Enter","Enter")
SE5$Habitat<-c("Crop","Edge","Resource-Poor","Resource-Rich")
SE5

SE5b<- summarySE(step5, measurevar="steplength", groupvars=c("spatialconfig"))
SE5b

#by habitat class
cat<-c("Within","Within","Within","Within","Exit","Exit","Exit","Exit","Enter","Enter","Enter","Enter")
habitat<-c("ZD","HE","LD","HD","ZD","HE","LD","HD","ZD","HE","LD","HD")
steplength<-c(27.75561,13.132,11.27826,11.28437,60.56627,51.95107,35.90303,152.0798,45.01587,34.70477,134.2282,55.95135)
sd<-c(77.8,52.9,26.0,35.3,139.0,238.9,59.9,319.8,74.6,67.5,367.1,105.7)
sigwithin<-c("a","a","a","a","","","","","","","","")
sigexit<-c("","","","","ab","ab","b","a","","","","")
sigenter<-c("","","","","","","","","ab","b","a","ab")
exitenterstep<-data.frame(cat,habitat,steplength,sd,sigwithin,sigexit,sigenter)
exitenterstep

exitenterstep$cat<-factor(exitenterstep$cat, levels=c("Within", "Exit", "Enter"))
exitenterstep$habitat<-factor(exitenterstep$habitat, levels=c("HD", "LD", "ZD", "HE"))

b<-ggplot(exitenterstep, aes(fill=habitat, y=steplength, x=cat, width=.8))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Movement Category") +
  ylab("Step Length (m)")+
  theme_bw()+
  scale_fill_brewer(palette="YlGnBu")+
  scale_y_continuous(expand=c(0,0), limits=c(0,575))+
  geom_errorbar(aes(ymin=steplength-steplength, ymax=steplength+sd), width=.2, 
                position=position_dodge(.8))+
  geom_text(aes(label=sigwithin, y=(steplength+sd)),colour="darkolivegreen4",vjust=-1.5, size=5, position=position_dodge(.8),fontface="bold")+
  geom_text(aes(label=sigexit, y=(steplength+sd)),colour="deepskyblue3",vjust=-1.5, size=5, position=position_dodge(.8),fontface="italic")+
  geom_text(aes(label=sigenter, y=(steplength+sd)),colour="darkslategray",vjust=-1.5, size=5, position=position_dodge(.8),fontface="plain")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=14))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=14))+
  theme(axis.text.y=element_text(colour="black"))+
  scale_x_discrete(labels=label_wrap_gen(width=10))+
  theme(plot.title=element_text(size=18))+
  theme(axis.title = element_text(size = 16))+
  ggtitle("B.")+
  theme(legend.position = "bottom")+
  guides(fill=guide_legend(title="Habitat Class"))+
  theme(legend.text = element_text(size = 14))+
  theme(legend.title = element_text(size = 14))+
  theme(legend.box.background=element_rect(colour = "black"),
        legend.background = element_blank())
b

# #by spatial configuration
# cat<-c("Within","Within","Within","Exit","Exit","Exit","Enter","Enter","Enter")
# sp<-c("Mosaic","Roadside 500","Roadside 5","Mosaic","Roadside 500","Roadside 5","Mosaic","Roadside 500","Roadside 5")
# steplength<-c(8.951354,34.975177,16.193322,75.38241,49.99187,77.65375,51.81434,49.99187,77.65375)
# sd<-c(24.8,88.2,44.0,204.9,131.7,271.4,75.1,131.7,271.4)
# sigmosaic<-c("a","b","a","","","","","","")
# sigroad500<-c("","","","a","a","a","","","")
# sigroad5<-c("","","","","","","a","a","a")
# exitenter<-data.frame(cat,sp,steplength,sd,sigmosaic,sigroad500,sigroad5)
# exitenter
# 
# exitenter$sp<-factor(exitenter$sp, levels=c("Mosaic","Roadside 5","Roadside 500"))
# exitenter$cat<-factor(exitenter$cat, levels=c("Within", "Exit", "Enter"))

# c<-ggplot(exitenter, aes(fill=sp, y=steplength, x=cat, width=.8))+
#   geom_bar(stat="identity", color="black",
#            position=position_dodge())+
#   xlab("Movement Category") +
#   ylab("Step Length (m)")+
#   theme_bw()+
#   scale_fill_brewer(palette="YlOrRd")+
#   scale_y_continuous(expand=c(0,0), limits=c(0,575))+
#   geom_errorbar(aes(ymin=steplength-steplength, ymax=steplength+sd), width=.2, 
#                 position=position_dodge(.8))+
#   geom_text(aes(label=sigmosaic, y=(steplength+sd)),colour="firebrick",vjust=-1.5, size=5, position=position_dodge(.8),fontface="bold")+
#   #geom_text(aes(label=sigroad500, y=(steplength+sd)),colour="darkslategray",vjust=-1.5, size=5, position=position_dodge(.8),fontface="italic")+
#   #geom_text(aes(label=sigroad5, y=(steplength+sd)),colour="deepskyblue3",vjust=-1.5, size=5, position=position_dodge(.8),fontface="plain")+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"))+
#   theme(axis.text.x = element_text(size=14))+
#   theme(axis.text.x=element_text(colour="black"))+
#   theme(axis.text.y = element_text(size=14))+
#   theme(axis.text.y=element_text(colour="black"))+
#   scale_x_discrete(labels=label_wrap_gen(width=10))+
#   theme(plot.title=element_text(size=18))+
#   theme(axis.title = element_text(size = 16))+
#   ggtitle("c.")+
#   theme(legend.position = "bottom")+
#   guides(fill=guide_legend(title="Spatial Configuration"))+
#   theme(legend.text = element_text(size = 14))+
#   theme(legend.title = element_text(size = 14))+
#   theme(legend.box.background=element_rect(colour = "black"),
#         legend.background = element_blank())
# c

#formanuscript
# grid.arrange(a,b,c)
grid.arrange(a,b)


##########################################################################

#######################################
#steps over 50m
#####
##pca with steps over 50 or under 50 with environmental stimuli

names(sumfly)
summvtenv<-subset(sumfly,select=c("stepclass50","dayscaptive","tempc","windkph"))
head(summvtenv)
names(summvtenv)[1]<-"Step"

pcamvt<- prcomp(summvtenv[,2:4])
pcamvt

summary(pcamvt)

library(ggfortify)
pca.plot<-autoplot(pcamvt,data=summvtenv,colour='Step')
pca.plot

comps<-pca.plot$data
comps
cor(comps[4:6], comps[,c(1:2,7)])

#figure in manuscript
library(ggfortify)
autoplot(pcamvt,data=summvtenv,colour='Step',size=1.5)+
  theme_bw()+
  theme(legend.position="bottom", legend.direction="horizontal")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=14))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=14))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 16, vjust=-5))+
  theme(legend.text = element_text(size=12), legend.title = element_text(size=14))


#pca with steps over 50 or under 50 with movement characteristics

names(sumfly)
summvt<-subset(sumfly,select=c("stepclass50","euclidiandistance","total.distance","avgstep","turnstdev","numpoints"))
summvt<-summvt[!is.na(summvt$turnstdev),]
head(summvt)
names(summvt)[1]<-"Step"

pcamvt<- prcomp(summvt[,2:6])
pcamvt

summary(pcamvt)

library(ggfortify)
pca.plot<-autoplot(pcamvt,data=summvt,colour='Step')
pca.plot

comps<-pca.plot$data
comps
cor(comps[4:8], comps[,c(1:2,9:11)])

#figure in manuscript
library(ggfortify)
autoplot(pcamvt,data=summvt,colour='Step',size=1.5)+
  theme_bw()+
  theme(legend.position="bottom", legend.direction="horizontal")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=14))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=14))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 16, vjust=-5))+
  theme(legend.text = element_text(size=12), legend.title = element_text(size=14))

###########################################################
#prop with greater than 50 m steps
unique(sumfly$Release)
unique(sumfly$stepover50)

table(sumfly$Release, sumfly$stepover50)
# chisq.test(sumfly$Release, sumfly$stepover50, correct=FALSE)
# table(sumfly$trialsite, sumfly$stepover50)
# chisq.test(sumfly$trialsite, sumfly$stepover50, correct=FALSE)
chisq.test(c(44.4,42.1,42.1,45,50,55))



##################
#habitat that inidiated "big steps"
names(step)
table(step$pthabclass, step$stepover50)
# chisq.test(step$pthabclass, step$stepover50, correct=FALSE)
chisq.test(c(12.8,8.6,6.5,11.6))


# table(step$release, step$stepover50)
# chisq.test(step$release, step$stepover50, correct=FALSE)
# table(step$spatialconfig, step$stepover50)
# chisq.test(step$spatialconfig, step$stepover50, correct=FALSE)

#steps from habitats with greater than 50m steps - for table
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

####
#did they fly into the wind?
big2<-subset(step,stepover50=="over50")

names(big2)
names(sumfly)
big2<-merge(x = big2, y = sumfly[ , c("monarchrunid","windbearing")], by = "monarchrunid", all.x=TRUE)

unique(big2$windbearing)

big2$windcor <- (big2$bearing - big2$windbearing)
big2$adjeucbear <- ifelse(big2$windcor <0, big2$windcor +360, 
                          ifelse(big2$windcor >360, big2$windcor -360, big2$windcor)) 
big22<-big2[!is.na(big2$adjeucbear),]
big22$adjeucbeardeg<-circular(big22$adjeucbear, units="degrees", template=NULL)
names(big22)

plot.circular(big22$adjeucbeardeg, stack=TRUE, col="darkslategrey")
rayleigh.test(big22$adjeucbeardeg)

plot.circular(big22$adjeucbeardeg, stack=TRUE, col="darkslategrey")
cirmean<-mean(big22$adjeucbeardeg)
cirmean
arrows.circular(cirmean, col="darkslategrey", lwd=2)
rayleigh.test(big22$adjeucbeardeg)

big22$adjwind<-c(180)
big22$adjwind<-circular(big22$adjwind, units="degrees", template=NULL)
data <- list(
  a = circular(big22$adjeucbeardeg, units="degrees", template=NULL),
  awind = circular(big22$adjwind, units="degrees", template=NULL)
)
watson.williams.test(data)

plot.circular(big22$adjeucbeardeg, stack=TRUE, col="darkslategrey")
cirmean<-circular(-4.009707,units="degrees", template=NULL)
arrows.circular(cirmean, col="darkslategrey", lwd=2)
wind<- circular(180, units="degrees", template=NULL)
arrows.circular(wind, col="black", lwd=3, lty="dotted")

#########
#where did they go?
big3<-subset(step2,landonstepover50=="over50")
unique(big3$landonstepover50)
table(big3$crossfrom)
table(big3$surface)
table(big3$landon)

#recovered with radio telemetry
table(big3$perception)
######################################################
#individuals with number of big steps
bigstep<-read.csv("1920_countstepsover50.csv",header=TRUE)
names(bigstep)
bigstep
bigstep$n<-c("n=61","n=37","n=12","n=2","n=2")

ggplot(bigstep, aes(y=count, x=stepsover50, width=.8))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Number of Steps >50m \nwithin Observation of One Individual") +
  ylab("Number of Individuals")+
  theme_bw()+
  #geom_errorbar(aes(ymin=firstbigstep-sdbelow, ymax=firstbigstep+sd), width=.2, 
  #             position=position_dodge(.8))+
  geom_text(aes(label=n, y=count+1),vjust=0, size=5, position=position_dodge(.8))+
  scale_y_continuous(expand=c(0,0), limits=c(0,70))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=14))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=14))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 16, vjust=-5))




####number of steps prior to big step
big<-read.csv("1920_individtakestepsover50.csv",header=TRUE)
big<-subset(big, stepsover50 > 0)
names(big)
big$Release<-paste(big$release, big$trialsite)
unique(big$crossfrom)

names(big)
numsteps<-glm(stepspriortobig~crossfrom+Release, data=big, family = gaussian(link = "identity"))
emm <- emmeans(numsteps, c("crossfrom","Release"), type='response')
joint_tests(emm)

# SE6<- summarySE(big, measurevar="stepspriortobig", groupvars=c("trialsite"))
# SE6
# #SE6$Sig<- c("ab","a","b")
# SE6$sp2 <-c("Roadside without Resource-Rich","Mosaic","Roadside with Resource-Rich")
# #SE4$crossfrom2<-c("Crop Field", "Crop Field","Crop Field",
# #                 "Resource-Poor","Resource-Poor","Resource-Poor",
# #                "Resource-Rich","Resource-Rich")
# SE6$below<-c(3.067689,5.212121,4.190899)
# 
# SE6$sp2<-factor(SE5$sp2, levels=c("Mosaic","Roadside without Resource-Rich","Roadside with Resource-Rich"))
# #SE4$crossfrom2<-factor(SE4$crossfrom2, levels=c("Crop Field", "Resource-Poor", "Resource-Rich"))
# #SE$spatialconfig<-factor(SE$spatialconfig, levels=c("Mixed Land Use", "Roadside without Resource-Rich", "Roadside with Resource-Rich"))
# 
# library(ggplot2)
# #pal<-c("grey75","grey43","grey15")
# 
# ggplot(SE6, aes(y=stepspriortobig, x=sp2, width=.8))+
#   geom_bar(stat="identity", color="black",
#            position=position_dodge())+
#   xlab("Spatial Configuration") +
#   ylab("Steps Prior to > 50 m Step")+
#   theme_bw()+
#   geom_errorbar(aes(ymin=stepspriortobig-below, ymax=stepspriortobig+sd), width=.2, 
#                 position=position_dodge(.8))+
#   #geom_text(aes(label=Sig, y=3000),vjust=-1.5, size=5, position=position_dodge())+
#   scale_y_continuous(expand=c(0,0), limits=c(0,14))+
#   scale_x_discrete(labels=label_wrap_gen(width=12))+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"))+
#   theme(axis.text.x = element_text(size=12))+
#   theme(axis.text.x=element_text(colour="black"))+
#   theme(axis.text.y = element_text(size=12))+
#   theme(axis.text.y=element_text(colour="black"))+
#   theme(axis.title = element_text(size = 14, vjust=-5))+
#   theme(plot.title=element_text(size=18))+
#   theme(legend.position = "bottom")+
#   theme(legend.text = element_text(size=10), legend.title = element_text(size=12))+
#   scale_fill_discrete(name="Cross From Habitat Class")


##duration to big step
# big<-read.csv("1920_individtakestepsover50.csv",header=TRUE)
# big<-subset(big, stepsover50 > 0)
# names(big)

duration<-glm(durationtobigstep~crossfrom+Release, data=big, family = gaussian(link = "identity"))
emm <- emmeans(duration, c("crossfrom","Release"), type='response')
joint_tests(emm)
emm2<-emmeans(duration, c("trialsite"), type='response')
joint_tests(emm2)
pairs(emm2)
emm2
CLD(emm2)

# SE4<- summarySE(big, measurevar="durationtobigstep", groupvars=c("crossfrom","trialsite"))
# SE4
# SE4$Sig<- c("ab","","b","","","","a","")
# SE4$sp2 <-c("Roadside without Resource-Rich","Mosaic","Roadside with Resource-Rich",
#             "Roadside without Resource-Rich","Mosaic","Roadside with Resource-Rich",
#             "Mosaic","Roadside with Resource-Rich")
# SE4$crossfrom2<-c("Crop Field", "Crop Field","Crop Field",
#                   "Resource-Poor","Resource-Poor","Resource-Poor",
#                   "Resource-Rich","Resource-Rich")
# SE4
# SE4$sdbelow<-c(959.03,114.00,0,64.34,847.20,835.33,614.35,568.18)
# 
# SE4$sp2<-factor(SE4$sp2, levels=c("Mosaic","Roadside without Resource-Rich","Roadside with Resource-Rich"))
# SE4$crossfrom2<-factor(SE4$crossfrom2, levels=c("Crop Field", "Resource-Poor", "Resource-Rich"))
# #SE$spatialconfig<-factor(SE$spatialconfig, levels=c("Mixed Land Use", "Roadside without Resource-Rich", "Roadside with Resource-Rich"))
# 
# library(ggplot2)
# #pal<-c("grey75","grey43","grey15")
# 
# ggplot(SE4, aes(fill=crossfrom2, y=durationtobigstep, x=sp2, width=.8))+
#   geom_bar(stat="identity", color="black",
#            position=position_dodge())+
#   xlab("Habitat Configuration") +
#   ylab("Time Prior to > 50 m Step (sec)")+
#   theme_bw()+
#   geom_errorbar(aes(ymin=durationtobigstep-sdbelow, ymax=durationtobigstep+sd), width=.2, 
#                 position=position_dodge(.8))+
#   geom_text(aes(label=Sig, y=3000),vjust=-1.5, size=5, position=position_dodge())+
#   scale_y_continuous(expand=c(0,0), limits=c(0,3500))+
#   scale_x_discrete(labels=label_wrap_gen(width=12))+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"))+
#   theme(axis.text.x = element_text(size=12))+
#   theme(axis.text.x=element_text(colour="black"))+
#   theme(axis.text.y = element_text(size=12))+
#   theme(axis.text.y=element_text(colour="black"))+
#   theme(axis.title = element_text(size = 14, vjust=-5))+
#   theme(plot.title=element_text(size=18))+
#   theme(legend.position = "bottom")+
#   theme(legend.text = element_text(size=10), legend.title = element_text(size=12))+
#   scale_fill_discrete(name="Cross From Habitat Class")

# ### or without classes
# SE5<- summarySE(big, measurevar="durationtobigstep", groupvars=c("trialsite"))
# SE5
# SE5$Sig<- c("ab","a","b")
# SE5$sp2 <-c("Roadside without Resource-Rich","Mosaic","Roadside with Resource-Rich")
# #SE4$crossfrom2<-c("Crop Field", "Crop Field","Crop Field",
# #                 "Resource-Poor","Resource-Poor","Resource-Poor",
# #                "Resource-Rich","Resource-Rich")
# SE5
# 
# SE5$sp2<-factor(SE5$sp2, levels=c("Mosaic","Roadside without Resource-Rich","Roadside with Resource-Rich"))
# #SE4$crossfrom2<-factor(SE4$crossfrom2, levels=c("Crop Field", "Resource-Poor", "Resource-Rich"))
# #SE$spatialconfig<-factor(SE$spatialconfig, levels=c("Mixed Land Use", "Roadside without Resource-Rich", "Roadside with Resource-Rich"))
# 
# library(ggplot2)
# #pal<-c("grey75","grey43","grey15")
# 
# ggplot(SE5, aes(y=durationtobigstep, x=sp2, width=.8))+
#   geom_bar(stat="identity", color="black",
#            position=position_dodge())+
#   xlab("Spatial Configuration") +
#   ylab("Time Prior to > 50 m Step (sec)")+
#   theme_bw()+
#   geom_errorbar(aes(ymin=durationtobigstep-sd, ymax=durationtobigstep+sd), width=.2, 
#                 position=position_dodge(.8))+
#   geom_text(aes(label=Sig, y=3000),vjust=-1.5, size=5, position=position_dodge())+
#   scale_y_continuous(expand=c(0,0), limits=c(0,3500))+
#   scale_x_discrete(labels=label_wrap_gen(width=12))+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"))+
#   theme(axis.text.x = element_text(size=12))+
#   theme(axis.text.x=element_text(colour="black"))+
#   theme(axis.text.y = element_text(size=12))+
#   theme(axis.text.y=element_text(colour="black"))+
#   theme(axis.title = element_text(size = 14, vjust=-5))+
#   theme(plot.title=element_text(size=18))+
#   theme(legend.position = "bottom")+
#   theme(legend.text = element_text(size=10), legend.title = element_text(size=12))+
#   scale_fill_discrete(name="Cross From Habitat Class")
# 


# stepcat<-rbind(step50,step100,step250,step500,step1000)
# names(stepcat)
# unique(stepcat$release)
# 
# stepclass<-glm(durationtobigstep~crossfrom+release+bigstepclass, data=stepcat, family = gaussian(link = "identity"))
# emm <- emmeans(stepclass, c("crossfrom","release","bigstepclass"), type='response')
# joint_tests(emm)
# emm2<-emmeans(stepclass, c("release"), type='response')
# joint_tests(emm2)
# pairs(emm2)
# emm2
# CLD(emm2)

stepcat<-rbind(step50,step100,step250,step500,step1000)
names(stepcat)

stepclass<-glm(durationtobigstep~crossfrom+release+spatialconfig+bigstepclass, data=stepcat, family = gaussian(link = "identity"))
emm <- emmeans(stepclass, c("crossfrom","release","spatialconfig","bigstepclass"), type='response')
joint_tests(emm)
emm2<-emmeans(stepclass, c("spatialconfig"), type='response')
joint_tests(emm2)
pairs(emm2)
emm2
CLD(emm2)




###########################################################

#total distance
names(sumfly)
unique(sumfly$releaselandclass)

totaldist <- glm(total.distance ~ releaselandclass + trialsite, data=sumfly, family = gaussian(link = "identity"))
emm <- emmeans(totaldist, c("releaselandclass","trialsite"), type='response')
joint_tests(emm)

min(sumfly$total.distance)
max(sumfly$total.distance)
median(sumfly$total.distance)
mean(sumfly$total.distance)


eucdist <- glm(euclidiandistance ~ releaselandclass + trialsite, data=sumfly, family = gaussian(link = "identity"))
emm <- emmeans(eucdist, c("releaselandclass","trialsite"), type='response')
joint_tests(emm)

min(sumfly$euclidiandistance)
max(sumfly$euclidiandistance)
median(sumfly$euclidiandistance)
mean(sumfly$euclidiandistance)

#difference in total and euclidian?
comparedist<-read.csv("compareeuctotaldist.csv",header=TRUE)
names(comparedist)

t.test(distance~category,data=comparedist)
# t.test(distance~category,data=comparedist,paired=TRUE)


##################################################################
#turn angle
unique(step$turnangle)

turn<-step[!(step$turnangle=="0"),]
names(turn)
unique(turn$turnwithin)
turn<-turn[!is.na(turn$turnwithin),]
turn$turn360 <- ifelse(turn$turnangle <0, turn$turnangle +360, turn$turnangle)

unique(turn$spatialconfig)
m<-subset(turn, spatialconfig == "mixed")
rr<-subset(turn, spatialconfig == "rich-roadside")
pr<-subset(turn, spatialconfig == "poor-roadside")
r<-rbind(rr,pr)

unique(turn$pthabclass)
mrich<-subset(m, pthabclass == "rich")
mpoor<-subset(m, pthabclass == "poor")
mcrop<-subset(m, pthabclass == "crop")
medge<-subset(m, pthabclass == "edge")

rrich<-subset(r, pthabclass == "rich")
rpoor<-subset(r, pthabclass == "poor")
rcrop<-subset(r, pthabclass == "crop")
redge<-subset(r, pthabclass == "edge")

turnrich<-rbind(mrich,rrich)
turnpoor<-rbind(mpoor,rpoor)
turncrop<-rbind(mcrop,rcrop)
turnedge<-rbind(medge,redge)



mrichc<-circular(mrich$turn360, units="degrees", type = "angles", template="none", zero=pi/2, rotation="clock")
mpoorc<-circular(mpoor$turn360, units="degrees", type = "angles", template="none", zero=pi/2, rotation="clock")
mcropc<-circular(mcrop$turn360, units="degrees", type = "angles", template="none", zero=pi/2, rotation="clock")
medgec<-circular(medge$turn360, units="degrees", type = "angles", template="none", zero=pi/2, rotation="clock")
rrichc<-circular(rrich$turn360, units="degrees", type = "angles", template="none", zero=pi/2, rotation="clock")
rpoorc<-circular(rpoor$turn360, units="degrees", type = "angles", template="none", zero=pi/2, rotation="clock")
rcropc<-circular(rcrop$turn360, units="degrees", type = "angles", template="none", zero=pi/2, rotation="clock")
redgec<-circular(redge$turn360, units="degrees", type = "angles", template="none", zero=pi/2, rotation="clock")


rich<-circular(turnrich$turn360, units="degrees", type = "angles", template="none", zero=pi/2, rotation="clock")
poor<-circular(turnpoor$turn360, units="degrees", type = "angles", template="none", zero=pi/2, rotation="clock")
crop<-circular(turncrop$turn360, units="degrees", type = "angles", template="none", zero=pi/2, rotation="clock")
edge<-circular(turnedge$turn360, units="degrees", type = "angles", template="none", zero=pi/2, rotation="clock")


#homogeneity in spread?
watson.wheeler.test(list(mrichc,mpoorc,mcropc,medgec,rrichc,rpoorc,rcropc,redgec))

watson.wheeler.test(list(rich,poor,crop,edge))

#compare habitat configurations
#mixed
watson.wheeler.test(list(mrichc,mpoorc,mcropc,medgec))
#roadside
watson.wheeler.test(list(rrichc,rpoorc,rcropc,redgec))

#compare mosaic to roadside/linear habitat
#rich habitat
watson.wheeler.test(list(mrichc,rrichc))
#crop habitat
watson.wheeler.test(list(mcropc,rcropc))
#edge habitat
watson.wheeler.test(list(medgec,redgec))
#poor habitat
watson.wheeler.test(list(mpoorc,rpoorc))


#######################################################
medgeplot<-ggplot(data=medge, aes(x=turnangle)) +
  geom_histogram(breaks=seq(-180, 180, by=10),
                 col="black",
                 fill="black",
                 alpha = .2) +
  xlab("Turn Angle (Degrees)") +
  ylab("Count")+
  theme_bw()+
  #ggtitle("Roadside Resource-Rich Habitat")+
  #theme(plot.title=element_text(hjust = 0.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_y_continuous(expand=c(0,0), limits=c(0,20))+
  scale_x_continuous(expand=c(0,0), limits=c(-180,180))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.y=element_text(colour="black"))+
  ggtitle("G.")+
  theme(plot.title=element_text(size=18))
#theme(axis.title = element_text(size = 25))+
#annotate("text", x=90, y=30, label="Poor-Roadside, Crop")#,size=7)

mrichplot
mpoorplot
mcropplot
medgeplot
rrichplot
rpoorplot
rcropplot
redgeplot

grid.arrange(mrichplot, rrichplot,
             mcropplot, rcropplot,
             mpoorplot, rpoorplot,
             medgeplot, redgeplot, nrow=4)

nrow(redge)



####################################
sequential<-read.csv("sequentialhabitat.csv",header=TRUE)
names(sequential)

seqpts<-glm(sequentialpointsinhabitat ~ habitatclass, data=sequential, family= gaussian(link = "identity"))
emm <- emmeans(seqpts, c("habitatclass"), type='response')
joint_tests(emm)
pairs(emm)
emm
CLD(emm)

seqsteps<-glm(sequentialstepsinhabitat ~ habitatclass, data=sequential, family= gaussian(link = "identity"))
emm <- emmeans(seqpts, c("habitatclass"), type='response')
joint_tests(emm)
pairs(emm)
emm
CLD(emm)

seqtime<-glm(sequentialtime ~ habitatclass, data=sequential, family= gaussian(link = "identity"))
emm <- emmeans(seqpts, c("habitatclass"), type='response')
joint_tests(emm)
pairs(emm)
emm
CLD(emm)


SE<- summarySE(sequential, measurevar="sequentialpointsinhabitat", groupvars=c("habitatclass"))
SE
SE$Sig<- c("ab","b","a","a")
SE$class <-c("Crop Field","Edge Habitat","Resource-Poor","Resource-Rich")
SE
SE$below<-c(1.898592,1.350706,3.197368,3.824394)


#SE$class<-factor(SE$class, levels=c("Within Habitat","Cross Habitat Boundary"))

ggplot(SE, aes(y=sequentialpointsinhabitat, x=class, width=.8))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Habitat Class") +
  ylab("Number of Sequential Points within Habitat Class")+
  theme_bw()+
  scale_fill_brewer(palette="YlGnBu")+
  geom_errorbar(aes(ymin=sequentialpointsinhabitat-below, ymax=sequentialpointsinhabitat+sd), width=.2, 
                position=position_dodge(.8))+
  geom_text(aes(label=Sig, y=sequentialpointsinhabitat+sd),vjust=-1.5, size=5, position=position_dodge(.8))+
  scale_y_continuous(expand=c(0,0), limits=c(0,10.1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=14))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=14))+
  theme(axis.text.y=element_text(colour="black"))+
  scale_x_discrete(labels=label_wrap_gen(width=10))+
  theme(plot.title=element_text(size=18))+
  theme(axis.title = element_text(size = 16))+
  #ggtitle("A.")+
  theme(legend.position = "NULL")



