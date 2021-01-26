#home laptop
#setwd("C:/Users/kelse/Box Sync/ISU Research/2020/Summer Telemetry Analyses/CTMM 19")
#lab19<-read.csv("19_label.csv", header=TRUE)
#setwd("C:/Users/kelse/Box Sync/Publications/19&20 Habitat Utilization/Data Analysis")

#work computer
setwd("C:/Users/kefisher/Box/ISU Research/2020/Summer Telemetry Analyses/CTMM 19")
lab19<-read.csv("19_label.csv", header=TRUE)
setwd("C:/Users/kefisher/Box/Publications/19&20 Habitat Utilization/Data Analysis")

sum19 <- read.csv("19_trialsummary.csv", header=TRUE)
sum20 <- read.csv("20_trialsummary.csv", header=TRUE)

sum19b <- merge(lab19, sum19, by="monarchrunid")

summary<-rbind(sum19b, sum20)
names(summary)

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


#################################################################################################

#2019 overall direction of movement relative to wind

names(sum19b)
bear<-merge(monarchs, sum19b, by="monarchrunid")
bear<-bear[!is.na(bear$euclidianbearing),]

winddirection <- data.frame("winddirection" = c("N","NNE", "NE", "ENE", "E", "SEE", "SE", "SSE", "S", "SSW", "SW", "SWW", "W", "NWW", "NW", "NNW"), "windbearing" = c(0, 22.5, 45, 67.5, 90, 112.5, 135, 157.5, 180, 202.5, 225, 247.5, 270, 292.5, 315, 337.5))
bear$windbearing <- winddirection$windbearing[match(bear$winddirection, winddirection$winddirection)]

bear$windcor <- (bear$euclidianbearing - bear$windbearing)
bear$adjeucbear <- ifelse(bear$windcor <0, bear$windcor +360, 
                          ifelse(bear$windcor >360, bear$windcor -360, bear$windcor)) 

library(circular)
bear$adjeucbeardeg<-circular(bear$adjeucbear, units="degrees", template="geographics")
mean(bear$adjeucbeardeg)
rayleigh.test(bear$adjeucbeardeg)

bear$adjwind<-c(180)
bear$adjwind<-circular(bear$adjwind, units="degrees", template="geographics")
data <- list(
  x = circular(bear$adjeucbeardeg, units="degrees", template="geographics"),
  xwind = circular(bear$adjwind, units="degrees", template="geographics")
)
watson.williams.test(data)

#pretty graph with relative direction in relation to resources
library(circular)
wind<- circular(180, units="degrees", template="geographics")
plot.circular(bear$adjeucbeardeg, stack=TRUE, col="darkslategrey")
#points.circular(bear2$adjeucbeardeg, stack=TRUE, col="cornflowerblue")
legend("bottom", legend=c("Mixed Land Use","Individual Effective Bearing", "Mean Monarch Movement", "Wind Blowing from N to S"), col=c(NA,"darkslategrey", "darkslategrey", "black"), lty=c(NA,NA,"solid","dotted"),lwd=c(NA,NA,2,3), pch=c(NA,16,NA,NA), xpd=TRUE, inset=c(0,-.2), text.font=c(2,1,1,1))
#legend("bottomleft", c("Rayleigh Test of Uniformity", "p = 0.0053", "n = 75"), xpd=TRUE, inset=c(0,-.2), bty="n")
#legend("bottomright", c("Watson-Williams Test for Homogeneity of Means", "p < 2.2e-16", ""), xpd=TRUE, inset=c(0,-.2), bty="n")
cirmean<-mean(bear$adjeucbeardeg)
#cirmean2<-mean(bear2$adjeucbeardeg)
arrows.circular(cirmean, col="darkslategrey", lwd=2)
#arrows.circular(cirmean2, col="cornflowerblue", lwd=2)
cirmeanwind<-mean(bear$adjwind)
arrows.circular(wind, col="black", lwd=3, lty="dotted")

###################
#2019 by release habitat classification

names(bear)
unique(bear$Release)

corn<-subset(bear, Release == "corn")
prairie<-subset(bear, Release == "prairie")
edge<-subset(bear, Release == "edge")
grass<-subset(bear, Release == "grass")


corn$adjeucbeardeg<-circular(corn$adjeucbear, units="degrees", template="geographics")
rayleigh.test(corn$adjeucbeardeg)
corn$euclidianbearingdeg<-circular(corn$euclidianbearing, units="degrees", template="geographics")
rayleigh.test(corn$euclidianbearingdeg)

prairie$adjeucbeardeg<-circular(prairie$adjeucbear, units="degrees", template="geographics")
rayleigh.test(prairie$adjeucbeardeg)
prairie$euclidianbearingdeg<-circular(prairie$euclidianbearing, units="degrees", template="geographics")
rayleigh.test(prairie$euclidianbearingdeg)

grass$adjeucbeardeg<-circular(grass$adjeucbear, units="degrees", template="geographics")
rayleigh.test(grass$adjeucbeardeg)
grass$euclidianbearingdeg<-circular(grass$euclidianbearing, units="degrees", template="geographics")
rayleigh.test(grass$euclidianbearingdeg)

edge$adjeucbeardeg<-circular(edge$adjeucbear, units="degrees", template="geographics")
rayleigh.test(edge$adjeucbeardeg)
edge$euclidianbearingdeg<-circular(edge$euclidianbearing, units="degrees", template="geographics")
rayleigh.test(edge$euclidianbearingdeg)

edge$adjwind<-c(180)
edge$adjwind<-circular(edge$adjwind, units="degrees", template="geographics")
data <- list(
  x = circular(edge$adjeucbeardeg, units="degrees", template="geographics"),
  xwind = circular(edge$adjwind, units="degrees", template="geographics")
)
watson.williams.test(data)




#pretty graph with relative direction in relation to resources
library(circular)
wind<- circular(180, units="degrees", template="geographics")
plot.circular(edge$euclidianbearingdeg, stack=TRUE, col="darkslategrey", shrink = 1.15)
points.circular(corn$adjeucbeardeg, stack=TRUE, col="cornflowerblue")
points.circular(grass$adjeucbeardeg, stack=TRUE, col="green")
points.circular(prairie$adjeucbeardeg, stack=TRUE, col="purple")

legend("bottom", legend=c("Mixed Land Use","Individual Effective Bearing", "Mean Monarch Movement", "Wind Blowing from N to S"), col=c(NA,"darkslategrey", "darkslategrey", "black"), lty=c(NA,NA,"solid","dotted"),lwd=c(NA,NA,2,3), pch=c(NA,16,NA,NA), xpd=TRUE, inset=c(0,-.2), text.font=c(2,1,1,1))
cirmean<-mean(edge$adjeucbeardeg)
arrows.circular(cirmean, col="darkslategrey", lwd=2)
cirmeanwind<-mean(edge$adjwind)
arrows.circular(wind, col="black", lwd=3, lty="dotted")





#2020 overall direction of movement relative to wind
names(sum20)
bear2<-sum20[!is.na(sum20$euclidianbearing),]

winddirection <- data.frame("winddirection" = c("N","NNE", "NE", "ENE", "E", "SEE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "NWW", "NW", "NNW"), "windbearing" = c(0, 22.5, 45, 67.5, 90, 112.5, 135, 157.5, 180, 202.5, 225, 247.5, 270, 292.5, 315, 337.5))
bear2$windbearing <- winddirection$windbearing[match(bear2$winddirection, winddirection$winddirection)]

bear2$windcor <- (bear2$euclidianbearing - bear2$windbearing)
bear2$adjeucbear <- ifelse(bear2$windcor <0, bear2$windcor +360, 
                          ifelse(bear2$windcor >360, bear2$windcor -360, bear2$windcor)) 

library(circular)
bear2$adjeucbeardeg<-circular(bear2$adjeucbear, units="degrees", template="geographics")
mean(bear2$adjeucbeardeg)
rayleigh.test(bear2$adjeucbeardeg)

bear2$adjwind<-c(180)
bear2$adjwind<-circular(bear2$adjwind, units="degrees", template="geographics")
data <- list(
  x = circular(bear2$adjeucbeardeg, units="degrees", template="geographics"),
  xwind = circular(bear2$adjwind, units="degrees", template="geographics")
)
watson.williams.test(data)

# 
#pretty graph with relative direction in relation to resources
library(circular)
wind<- circular(180, units="degrees", template="geographics")
#plot.circular(bear$adjeucbeardeg, stack=TRUE, col="darkslategrey")
plot.circular(bear2$adjeucbeardeg, stack=TRUE, col="cornflowerblue")
legend("bottom", legend=c("Roadside Corridors","Individual Effective Bearing", "Mean Monarch Movement", "Wind Blowing from N to S"), col=c(NA,"cornflowerblue", "cornflowerblue", "black"), lty=c(NA,NA,"solid","dotted"),lwd=c(NA,NA,2,3), pch=c(NA,16,NA,NA), xpd=TRUE, inset=c(0,-.2), text.font=c(2,1,1,1))
#legend("bottomleft", c("Rayleigh Test of Uniformity", "p = 0.0053", "n = 75"), xpd=TRUE, inset=c(0,-.2), bty="n")
#legend("bottomright", c("Watson-Williams Test for Homogeneity of Means", "p < 2.2e-16", ""), xpd=TRUE, inset=c(0,-.2), bty="n")
#cirmean<-mean(bear$adjeucbeardeg)
cirmean2<-mean(bear2$adjeucbeardeg)
#arrows.circular(cirmean, col="darkslategrey", lwd=2)
arrows.circular(cirmean2, col="cornflowerblue", lwd=2)
cirmeanwind<-mean(bear$adjwind)
arrows.circular(wind, col="black", lwd=3, lty="dotted")

#############################################################

#2020 overall direction of movement relative to NS road
names(bear2)

library(circular)
bear2$euclidianbearing<-circular(bear2$euclidianbearing, units="degrees", template="geographics")
rayleigh.test(bear2$euclidianbearing)

# 
library(circular)
south<- circular(180, units="degrees", template="geographics")
north<- circular(0, units="degrees", template="geographics")
#plot.circular(bear$adjeucbeardeg, stack=TRUE, col="darkslategrey")
plot.circular(bear2$euclidianbearing, stack=TRUE, col="cornflowerblue")
legend("bottom", legend=c("Roadside Corridors","Individual Effective Bearing", "Mean Monarch Movement", "N-S Road"), col=c(NA,"cornflowerblue", "cornflowerblue", "black"), lty=c(NA,NA,"solid","dotted"),lwd=c(NA,NA,2,3), pch=c(NA,16,NA,NA), xpd=TRUE, inset=c(-.06,-.2), text.font=c(2,1,1,1))
#legend("bottomleft", c("Rayleigh Test of Uniformity", "p = 0.0053", "n = 75"), xpd=TRUE, inset=c(0,-.2), bty="n")
#legend("bottomright", c("Watson-Williams Test for Homogeneity of Means", "p < 2.2e-16", ""), xpd=TRUE, inset=c(0,-.2), bty="n")
#cirmean<-mean(bear$adjeucbeardeg)
#cirmean2<-mean(bear2$euclidianbearing)
#arrows.circular(cirmean, col="darkslategrey", lwd=2)
#arrows.circular(cirmean2, col="cornflowerblue", lwd=2)
arrows.circular(south, col="black", lwd=3, lty="dotted")
arrows.circular(north, col="black", lwd=3, lty="dotted")







################################################################################################

#2019 turn angle by landcover classification
turnwithin<-read.csv("19_locationsonly_turnanglewithinhabitat.csv", header=TRUE)
turnwithin<- merge(monarchs, turnwithin, by="monarchrunid")

hist(turnwithin$turnangle)
unique(turnwithin$landcoverclassification)
turnwithin$turn360 <- ifelse(turnwithin$turnangle <0, turnwithin$turnangle +360, turnwithin$turnangle)

library(dplyr)
turncrop<-turnwithin %>% filter(landcoverclassification == "crop")
hist(turncrop$turnangle)
hist(turncrop$turn360)
turnrich<-turnwithin %>% filter(landcoverclassification == "resource-rich")
hist(turnrich$turnangle)
turnpoor<-turnwithin %>% filter(landcoverclassification == "resource-poor")
hist(turnpoor$turnangle)

library(circular)
rich<-circular(turnrich$turn360, units="degrees", type = "angles", template="none", zero=pi/2, rotation="clock")
crop<-circular(turncrop$turn360, units="degrees", type = "angles", template="none", zero=pi/2, rotation="clock")
poor<-circular(turnpoor$turn360, units="degrees", type = "angles", template="none", zero=pi/2, rotation="clock")

#homogeneity in spread?
watson.wheeler.test(list(rich, crop, poor))

#plot.circular(rich, stack=TRUE, col="cornflowerblue", main = "2019 - Turn Angles Within Habitat Types")
#points.circular(poor, stack=TRUE, col="goldenrod2")
#points.circular(crop, stack=TRUE, col="chocolate")
#straight<-circular(0, units="degrees", type = "angles", template="none", zero=pi/2, rotation="clock")
#turnaround<-circular(180, units="degrees", type = "angles", template="none", zero=pi/2, rotation="clock")
#arrows.circular(straight, col="black", lwd=1, lty="longdash")
#arrows.circular(turnaround, col="black", lwd=1, lty="dotted")
#legend("topright", legend=c("Straight", "Turn Around", "Prairie", "Grass", "Crop"), col=c("black","black", "cornflowerblue", "goldenrod2", "chocolate"), lty=c("longdash","dotted","solid","solid","solid"),lwd=c(1,1,2,2,2), xpd=TRUE, inset=c(-.05,0))
#legend("bottomleft", c("Rayleigh Test of Uniformity", "Prairie: p = 0; n = 122", "Grass: p = 0; n = 96", "Crop: p = 0.5393; n = 32"), xpd=TRUE, inset=c(-.1,-.2), bty="n", text.font=c(2,1,1,1))
#legend("bottomright", c("Watson-Wheeler Test for Homogeneity in Spread", "p = 0.4809", "", ""), xpd=TRUE, inset=c(0,-.2), bty="n", text.font=c(2,1,1,1))


library(ggplot2)
richplot<-ggplot(data=turnrich, aes(x=turnangle)) +
  geom_histogram(breaks=seq(-180, 180, by=10),
                 col="black",
                 fill="black",
                 alpha = .2) +
  xlab("") +
  ylab("")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_y_continuous(expand=c(0,0), limits=c(0,15.5))+
  scale_x_continuous(expand=c(0,0), limits=c(-180,180))+
  theme(axis.text.x = element_text(size=20))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=20))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 25))+
  annotate("text", x=125, y=14, label="Resource-Rich", size=7)

library(ggplot2)
poorplot<-ggplot(data=turnpoor, aes(x=turnangle)) +
  geom_histogram(breaks=seq(-180, 180, by=10),
                 col="black",
                 fill="black",
                 alpha = .2) +
  xlab("") +
  ylab("Count")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_y_continuous(expand=c(0,0), limits=c(0,15.5))+
  scale_x_continuous(expand=c(0,0), limits=c(-180,180))+
  theme(axis.text.x = element_text(size=20))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=20))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 25))+
  annotate("text", x=125, y=14, label="Resource-Poor", size=7)

library(ggplot2)
cropplot<-ggplot(data=turncrop, aes(x=turnangle)) +
  geom_histogram(breaks=seq(-180, 180, by=10),
                 col="black",
                 fill="black",
                 alpha = .2) +
  xlab("Turn Angle (Degrees)") +
  ylab("")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_y_continuous(expand=c(0,0), limits=c(0,15.5))+
  scale_x_continuous(expand=c(0,0), limits=c(-180,180))+
  theme(axis.text.x = element_text(size=20))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=20))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 25))+
  annotate("text", x=138, y=14, label="Crop Field", size=7)

library(ggplot2)
library(grid)
library(dplyr)
library(lubridate)

grid.newpage()
grid.draw(rbind(ggplotGrob(richplot), ggplotGrob(poorplot), ggplotGrob(cropplot), size = "last"))

######################################################################

#2019 step length by landcover classification and crossing edges

#steps within vs cross
stepcross<-read.csv("19_locationsonly_forflightsteps.csv", header=TRUE)
stepcross<- merge(monarchs, stepcross, by="monarchrunid")
stepcross<-stepcross[!is.na(stepcross$steplength),]
names(stepcross)

stepcross2 <- glm(steplength ~ crosswithin, data=stepcross, family = gaussian(link = "identity"))

library(emmeans)
dtm.emm2 <- emmeans(stepcross2, c("crosswithin"), type='response')
joint_tests(dtm.emm2)
pairs(dtm.emm2)
dtm.emm2
CLD(dtm.emm2)
pwpp(dtm.emm2)


library(lattice)
library(Rmisc)
SE2<- summarySE(stepcross, measurevar="steplength", groupvars=c("crosswithin"))
SE2
SE2$Sig<- NA
SE2$Sig<- c("b", "a")
SE2$class <-c("Cross Habitat Boundary", "Within Habitat")
#SE2$avg<-c("77.5m", "10.3m")
#SE2$p <-c("p < 0.0001", "")
SE2$n<- c("n=82", "n=432")
SE2

library(ggplot2)
crosswithinplot<-ggplot(SE2, aes(x=reorder(class, +steplength), y=steplength, width=.6))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Step Classification") +
  ylab("Step Length (m)")+
  theme_bw()+
  geom_errorbar(aes(ymin=steplength-steplength, ymax=steplength+sd), width=.2,
                position = position_dodge(.9))+
  geom_text(aes(label=Sig, y=steplength+(sd)),vjust=-1.5, size=5)+
  scale_y_continuous(expand=c(0,0), limits=c(0,360))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=10))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=10))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 14, vjust=-5))+
  #geom_text(aes(label = p), vjust=-13, hjust=2.75,size=7)+
  geom_text(aes(label=n, y=steplength),vjust=-0.6, hjust=-.25,size=4)+
  ggtitle("a.")+
  theme(plot.title=element_text(size=18))
crosswithinplot


#####within by habitat
step<-read.csv("19_locationsonly_forflightstepswithinhabclass.csv", header=TRUE)
step<- merge(monarchs, step, by="monarchrunid")
step<-step[!is.na(step$steplength),]
unique(step$classification)

step2 <- glm(steplength ~ classification, data=step, family = gaussian(link = "identity"))

library(emmeans)
dtm.emm2 <- emmeans(step2, c("classification"), type='response')
joint_tests(dtm.emm2)
pairs(dtm.emm2)
dtm.emm2
CLD(dtm.emm2)
pwpp(dtm.emm2)

library(lattice)
library(Rmisc)
SE3<- summarySE(step, measurevar="steplength", groupvars=c("classification"))
SE3
SE3$class <-c("Within Crop Field", "Within Resource-Poor", "Within Resource-Rich")
SE3$n<- c("n=44", "n=123", "n=251")
SE3

library(ggplot2)
withinplot<-ggplot(SE3, aes(x=reorder(class, +steplength), y=steplength, width=.6))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  aes(stringr::str_wrap(class, 10))+
  xlab("") +
  ylab("Step Length (m)")+
  theme_bw()+
  geom_errorbar(aes(ymin=steplength-steplength, ymax=steplength+sd), width=.2,
                position = position_dodge(.9))+
  #geom_text(aes(label=Sig, y=steplength+(sd)),vjust=-1.5, size=8)+
  scale_y_continuous(expand=c(0,0), limits=c(0,55))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=10))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=10))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 14, vjust=-5))+
  #geom_text(aes(label = p), vjust=-13, hjust=2.75,size=7)+
  geom_text(aes(label=n, y=steplength),vjust=-0.6, hjust=-.5,size=4)+
  annotate("text", x=1, y=52, label="Within Habitat", size=4.5)+
  ggtitle("b.")+
  theme(plot.title=element_text(size=18))
withinplot


###cross to habitats
names(stepcross)
unique(stepcross$crossto)
crossto<-stepcross[!is.na(stepcross$crossto),]
crossto<-crossto[!(crossto$crossto==""),]
unique(crossto$crossto)

cross2 <- glm(steplength ~ crossto, data=crossto, family = gaussian(link = "identity"))

library(emmeans)
dtm.emm2 <- emmeans(cross2, c("crossto"), type='response')
joint_tests(dtm.emm2)
pairs(dtm.emm2)
dtm.emm2
CLD(dtm.emm2)
pwpp(dtm.emm2)

sum(stepcross$crossto == "lost")
sum(stepcross$crossto == "rich")

library(lattice)
library(Rmisc)
SE4<- summarySE(crossto, measurevar="steplength", groupvars=c("crossto"))
SE4
SE4$class <-c("Enter Crop Field", "Exit Survey Area", "Enter Resource-Poor", "Enter Resource-Rich")
SE4$n<- c("n=12", "n=30", "n=8", "n=32")
SE4$Sig<- c("ab", "b", "ab", "a")
SE4

library(ggplot2)
crossplot<-ggplot(SE4, aes(x=reorder(class, +steplength), y=steplength, label = "c", width=.6))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  aes(stringr::str_wrap(class, 10))+
  xlab("Step Classification") +
  ylab("Step Length (m)")+
  theme_bw()+
  geom_errorbar(aes(ymin=steplength-steplength, ymax=steplength+sd), width=.2,
                position = position_dodge(.9))+
  geom_text(aes(label=Sig, y=steplength+(sd)),vjust=-1.5, size=5)+
  scale_y_continuous(expand=c(0,0), limits=c(0,650))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=10))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=10))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 14, vjust=-5))+
  #geom_text(aes(label = p), vjust=-13, hjust=2.75,size=7)+
  geom_text(aes(label=n, y=steplength),vjust=-0.6, hjust=-.5,size=4)+
  annotate("text", x=1.5, y=600, label="Cross Habitat Boundary", size=4.5)+
  ggtitle("c.")+
  theme(plot.title=element_text(size=18))
crossplot


crosswithinplot
withinplot
crossplot

library(gridExtra)
grid.arrange(crosswithinplot, arrangeGrob(withinplot,crossplot, nrow=2), nrow=1)

########################
head(sum19b)
head(stepcross)








