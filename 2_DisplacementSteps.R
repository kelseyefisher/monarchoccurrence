setwd("C:/Users/kefisher/Box/Publications/19&20 Habitat Utilization/Data Analysis")

summary<-read.csv("1920_trialsummary_5.csv", header=TRUE)
sumfly<-subset(summary, finaldud == "fly")
#displace<-subset(summary, displacement == "yes")

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


#######################################
#displacement
#####
##pca with displacement step vs no displacement with environmental stimuli

names(sumfly)
summvtenv<-subset(sumfly,select=c("displacement","dayscaptive","tempc","windkph"))
head(summvtenv)
#names(summvtenv)[1]<-"Step"

pcamvt<- prcomp(summvtenv[,2:4])
pcamvt

summary(pcamvt)

library(ggfortify)
pca.plot<-autoplot(pcamvt,data=summvtenv,colour='displacement')
pca.plot

comps<-pca.plot$data
comps
cor(comps[4:6], comps[,c(1:2,7)])

#figure in manuscript
library(ggfortify)
autoplot(pcamvt,data=summvtenv,colour='displacement',size=1.5)+
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
summvt<-subset(sumfly,select=c("displacement","euclidiandistance","total.distance","avgstep","turnstdev","numpoints"))
summvt<-summvt[!is.na(summvt$turnstdev),]
head(summvt)
#names(summvt)[1]<-"Step"

pcamvt<- prcomp(summvt[,2:6])
pcamvt

summary(pcamvt)

library(ggfortify)
pca.plot<-autoplot(pcamvt,data=summvt,colour='displacement')
pca.plot

comps<-pca.plot$data
comps
cor(comps[4:8], comps[,c(1:2,9:11)])

#figure in manuscript
library(ggfortify)
autoplot(pcamvt,data=summvt,colour='displacement',size=1.5)+
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


mrichc<-circular(mrich$turn360, units="degrees", type = "angles", template="none", zero=pi/2, rotation="clock")
mpoorc<-circular(mpoor$turn360, units="degrees", type = "angles", template="none", zero=pi/2, rotation="clock")
mcropc<-circular(mcrop$turn360, units="degrees", type = "angles", template="none", zero=pi/2, rotation="clock")
medgec<-circular(medge$turn360, units="degrees", type = "angles", template="none", zero=pi/2, rotation="clock")
rrichc<-circular(rrich$turn360, units="degrees", type = "angles", template="none", zero=pi/2, rotation="clock")
rpoorc<-circular(rpoor$turn360, units="degrees", type = "angles", template="none", zero=pi/2, rotation="clock")
rcropc<-circular(rcrop$turn360, units="degrees", type = "angles", template="none", zero=pi/2, rotation="clock")
redgec<-circular(redge$turn360, units="degrees", type = "angles", template="none", zero=pi/2, rotation="clock")


#homogeneity in spread?
watson.wheeler.test(list(mrichc,mpoorc,mcropc,medgec,rrichc,rpoorc,rcropc,redgec))

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



