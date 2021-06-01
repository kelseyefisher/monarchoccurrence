#orientation
setwd("C:/Users/kefisher/Box/Publications/19&20 Habitat Utilization/Data Analysis")
summary<-read.csv("1920_trialsummary_4.csv", header=TRUE)
sumfly<-subset(summary, finaldud == "fly")
step<-read.csv("1920_locations_steplengths_6.csv", header=TRUE)
step2<-step[!(step$landonsteplength=="0"),]
step2<-step2[!is.na(step2$monarchrunid),]
step<-step[!(step$steplength=="0"),]
step<-step[!is.na(step$monarchrunid),]

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

#################################################################################################

#orientation by release site

orient<-summary[!is.na(summary$euclidianbearing),]
names(orient)
orient$euclidianbearingdeg<-circular(orient$euclidianbearing, units="degrees", template="geographics")

orient$windcor <- (orient$euclidianbearing - orient$windbearing)
orient$adjeucbear <- ifelse(orient$windcor <0, orient$windcor +360, 
                            ifelse(orient$windcor >360, orient$windcor -360, orient$windcor)) 
orient2<-orient[!is.na(orient$adjeucbear),]
orient2$adjeucbeardeg<-circular(orient2$adjeucbear, units="degrees", template=NULL)
names(orient2)
unique(orient2$releasesiteid_repid)

i<-subset(orient2, releasesiteid_repid == "1")
ii<-subset(orient2, releasesiteid_repid == "2")
iii<-subset(orient2, releasesiteid_repid == "3")
iv<-subset(orient2, releasesiteid_repid == "4")
v<-subset(orient2, releasesiteid_repid == "5")
vi<-subset(orient2, releasesiteid_repid == "6")
vii<-subset(orient2, releasesiteid_repid == "7")
viii<-subset(orient2, releasesiteid_repid == "8")
ix<-subset(orient2, releasesiteid_repid == "9")
x<-subset(orient2, releasesiteid_repid == "10")
xi<-subset(orient2, releasesiteid_repid == "11")
xii<-subset(orient2, releasesiteid_repid == "12")
xiii<-subset(orient2, releasesiteid_repid == "13")

nrow(iii)
nrow(v)
nrow(ix)
nrow(x)

#are they directional?
plot.circular(i$euclidianbearingdeg, stack=TRUE, col="darkslategrey")
rayleigh.test(i$euclidianbearingdeg)
plot.circular(ii$euclidianbearingdeg, stack=TRUE, col="darkslategrey")
rayleigh.test(ii$euclidianbearingdeg)
plot.circular(iv$euclidianbearingdeg, stack=TRUE, col="darkslategrey")
rayleigh.test(iv$euclidianbearingdeg)
plot.circular(vii$euclidianbearingdeg, stack=TRUE, col="darkslategrey")
rayleigh.test(vii$euclidianbearingdeg)
plot.circular(xi$euclidianbearingdeg, stack=TRUE, col="darkslategrey")
rayleigh.test(xi$euclidianbearingdeg)

#8 - edge of corn and prairie
plot.circular(viii$euclidianbearingdeg, stack=TRUE, col="darkslategrey")
cirmean<-mean(viii$euclidianbearingdeg)
cirmean
arrows.circular(cirmean, col="darkslategrey", lwd=2)
rayleigh.test(viii$euclidianbearingdeg)
#6 - resource rich release south of corn field
plot.circular(vi$euclidianbearingdeg, stack=TRUE, col="darkslategrey")
cirmean<-mean(vi$euclidianbearingdeg)
cirmean
arrows.circular(cirmean, col="darkslategrey", lwd=2)
rayleigh.test(vi$euclidianbearingdeg)

######directional on roadsides?
plot.circular(xii$euclidianbearingdeg, stack=TRUE, col="darkslategrey")
rayleigh.test(xii$euclidianbearingdeg)
plot.circular(xiii$euclidianbearingdeg, stack=TRUE, col="darkslategrey")
rayleigh.test(xiii$euclidianbearingdeg)


#############################
#relative to wind??
  
plot.circular(orient2$adjeucbeardeg, stack=TRUE, col="darkslategrey")
rayleigh.test(orient2$adjeucbeardeg)

orient2$adjwind<-c(0)
orient2$adjwind<-circular(orient2$adjwind, units="degrees", template=NULL)
data <- list(
  a = circular(orient2$adjeucbeardeg, units="degrees", template=NULL),
  awind = circular(orient2$adjwind, units="degrees", template=NULL)
)
watson.williams.test(data)

plot.circular(orient2$adjeucbeardeg, stack=TRUE, col="darkslategrey")
cirmean<-circular(5.473292,units="degrees", template=NULL)
arrows.circular(cirmean, col="darkslategrey", lwd=2)
wind<- circular(0, units="degrees", template=NULL)
arrows.circular(wind, col="black", lwd=3, lty="dotted")

#were big steps into the wind?

names(step)
step$bearingdeg<-circular(step$bearing, units="degrees", template=NULL)
merge<-merge(x=step,y=summary[,c("monarchrunid","windbearing")], by="monarchrunid", all.x=TRUE)

merge$windcor <- (merge$bearing - merge$windbearing)
merge$adjstepbear <- ifelse(merge$windcor <0, merge$windcor +360, 
                            ifelse(merge$windcor >360, merge$windcor -360, merge$windcor)) 
merge<-merge[!is.na(merge$adjstepbear),]
merge$adjstepbeardeg<-circular(merge$adjstepbear, units="degrees", template=NULL)
names(merge)

big<-merge[(merge[,19]>50),]

plot.circular(big$adjstepbeardeg, stack=TRUE, col="darkslategrey")
rayleigh.test(big$adjstepbeardeg)

big$adjwind<-c(0)
big$adjwind<-circular(big$adjwind, units="degrees", template=NULL)
data <- list(
  a = circular(big$adjstepbeardeg, units="degrees", template=NULL),
  awind = circular(big$adjwind, units="degrees", template=NULL)
)
watson.williams.test(data)

plot.circular(big$adjstepbeardeg, stack=TRUE, col="darkslategrey", shrink=1.5)
cirmean<-circular(-4.009707,units="degrees", template=NULL)
arrows.circular(cirmean, col="darkslategrey", lwd=2)
wind<- circular(0, units="degrees", template=NULL)
arrows.circular(wind, col="black", lwd=3, lty="dotted")
legend("bottomright", legend=c("Upwind", "Monarch Movement"), col=c("black", "darkslategrey"), lwd=c(3,2), title="Mean Bearing", lty=c("dotted", "solid"))


plot.circular(bear5$adjeucbeardeg, stack=TRUE, col="cornflowerblue", shrink = 1.3)
points.circular(bear25$adjeucbeardeg, stack=TRUE, col="chocolate")
points.circular(bear50$adjeucbeardeg, stack=TRUE, col="darkslategrey")
#points.circular(bear75$adjeucbeardeg, stack=TRUE, col="tan4")
arrows.circular(cirmean5, col="cornflowerblue", lwd=2)
arrows.circular(cirmean25, col="chocolate", lwd=2)
arrows.circular(cirmean50, col="darkslategrey", lwd=2)
#arrows.circular(cirmean75, col="tan4", lwd=2)
arrows.circular(cirmeanres, col="palegreen4", lwd=3, lty="twodash")
legend("bottom", legend=c("Resources", "5 m", "25 m", "50 m"), col=c("palegreen4", "cornflowerblue", "chocolate", "darkslategrey"), lwd=2, title="Mean Bearing", lty=c("twodash", "solid", "solid", "solid"), horiz = TRUE)






# #are they directional relative to the wind?
# plot.circular(i$adjeucbeardeg, stack=TRUE, col="darkslategrey")
# rayleigh.test(i$adjeucbeardeg)
# plot.circular(ii$adjeucbeardeg, stack=TRUE, col="darkslategrey")
# rayleigh.test(ii$adjeucbeardeg)
# plot.circular(iv$adjeucbeardeg, stack=TRUE, col="darkslategrey")
# rayleigh.test(iv$adjeucbeardeg)
# plot.circular(vii$adjeucbeardeg, stack=TRUE, col="darkslategrey")
# rayleigh.test(vii$adjeucbeardeg)
# plot.circular(xi$adjeucbeardeg, stack=TRUE, col="darkslategrey")
# rayleigh.test(xi$adjeucbeardeg)
# plot.circular(xii$adjeucbeardeg, stack=TRUE, col="darkslategrey")
# rayleigh.test(xii$adjeucbeardeg)
# 
# plot.circular(xiii$adjeucbeardeg, stack=TRUE, col="darkslategrey")
# cirmean<-mean(xiii$adjeucbeardeg)
# cirmean
# arrows.circular(cirmean, col="darkslategrey", lwd=2)
# rayleigh.test(xiii$adjeucbeardeg)
# 
# xiii$adjwind<-c(180)
# xiii$adjwind<-circular(xiii$adjwind, units="degrees", template="geographics")
# data <- list(
#   a = circular(xiii$adjeucbeardeg, units="degrees", template="geographics"),
#   awind = circular(xiii$adjwind, units="degrees", template="geographics")
# )
# watson.williams.test(data)
# 
# plot.circular(xiii$adjeucbeardeg, stack=TRUE, col="darkslategrey")
# cirmean<-circular(1.332466,units="degrees", template="geographics")
# arrows.circular(cirmean, col="darkslategrey", lwd=2)
# wind<- circular(180, units="degrees", template="geographics")
# arrows.circular(wind, col="black", lwd=3, lty="dotted")









################################################################################
#perceptual range

#all steps that land on milkweed or nectar plant
names(step2)
unique(step2$landon)

forbs<-subset(step2,landon=="forb")
unique(forbs$surface)
mw<-subset(step2,landon=="milkweed")
resource<-rbind(forbs,mw)
unique(resource$monarchrunid)

unique(forbs$surface)
a<-subset(forbs,surface=="black eyed susan")
b<-subset(forbs,surface=="brown eyed susan")
c<-subset(forbs,surface=="bull thistle")
d<-subset(forbs,surface=="chickery")
e<-subset(forbs,surface=="compass plant")
f<-subset(forbs,surface=="dandilion")
g<-subset(forbs,surface=="golden alexander")
h<-subset(forbs,surface=="grey headed coneflower")
i<-subset(forbs,surface=="hoary vervane")
j<-subset(forbs,surface=="morning glory")
k<-subset(forbs,surface=="mountain mint")
l<-subset(forbs,surface=="oxeye sunflower")
m<-subset(forbs,surface=="pale purple coneflower")
n<-subset(forbs,surface=="queen anne's lace")
o<-subset(forbs,surface=="red clover")
p<-subset(forbs,surface=="white clover")
q<-subset(forbs,surface=="white sweet clover")
r<-subset(forbs,surface=="wild bergamot")
s<-subset(forbs,surface=="yarrow")
unique(mw$surface)
t<-subset(mw,surface=="butterfly milkweed")
u<-subset(mw,surface=="common milkweed")
v<-subset(mw,surface=="swamp milkweed")
w<-subset(mw,surface=="whorled milkweed")

nrow(w)
min(w$landonsteplength)
median(w$landonsteplength)
max(w$landonsteplength)



unique(resource$monarchrunid)
unique(mw$surface)

min(forbs$landonsteplength)
max(forbs$landonsteplength)
median(forbs$landonsteplength)

min(mw$landonsteplength)
max(mw$landonsteplength)
median(mw$landonsteplength)


b<-ggplot(data=forbs, aes(x=landonsteplength)) +
  geom_histogram(breaks=seq(0,225, by=1),
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
  scale_y_continuous(expand=c(0,0), limits=c(0,45))+
  scale_x_continuous(expand=c(0,0), limits=c(0,255))+
  theme(axis.text.x = element_text(size=14))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=14))+
  theme(axis.text.y=element_text(colour="black"))+
  ggtitle("B.")+
  theme(plot.title=element_text(size=26))+
  theme(axis.title = element_text(size = 15))
#annotate("text", x=90, y=30, label="Poor-Roadside, Crop")#,size=7)
b

a<-ggplot(data=mw, aes(x=landonsteplength)) +
  geom_histogram(breaks=seq(0,250, by=1),
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
  scale_y_continuous(expand=c(0,0), limits=c(0,45))+
  scale_x_continuous(expand=c(0,0), limits=c(0,255))+
  theme(axis.text.x = element_text(size=14))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=14))+
  theme(axis.text.y=element_text(colour="black"))+
  ggtitle("A.")+
  theme(plot.title=element_text(size=26))+
  theme(axis.title = element_text(size = 15))
#annotate("text", x=90, y=30, label="Poor-Roadside, Crop")#,size=7)
a
grid.arrange(a,b)








#### over 30 m into wind land on resource
names(resource)
percep<-resource[(resource[,15]>30),]
unique(percep$perception)
percep<-subset(percep,perception=="yes")
min(percep$landonsteplength)

percep<-merge(x = percep, y = sumfly[ , c("monarchrunid","windbearing")], by = "monarchrunid", all.x=TRUE)
unique(percep$windbearing)
percep<-percep[!is.na(percep$windbearing),]

percep$windcor <- (percep$bearing - percep$windbearing)
percep$adjeucbear <- ifelse(percep$windcor <0, percep$windcor +360, 
                          ifelse(percep$windcor >360, percep$windcor -360, percep$windcor)) 
unique(percep$adjeucbear)
percep2<-percep %>% filter((adjeucbear >=315) | (adjeucbear <=45))

min(percep2$landonsteplength)
max(percep2$landonsteplength)
median(percep2$landonsteplength)

forbs2<-subset(percep2,landon=="forb")
mw2<-subset(percep2,landon=="milkweed")

min(forbs2$landonsteplength)
max(forbs2$landonsteplength)
median(forbs2$landonsteplength)

min(mw2$landonsteplength)
max(mw2$landonsteplength)
median(mw2$landonsteplength)


unique(forbs2$surface)
a2<-subset(forbs2,surface=="black eyed susan")
b2<-subset(forbs2,surface=="brown eyed susan")
c2<-subset(forbs2,surface=="bull thistle")
d2<-subset(forbs2,surface=="chickery")
e2<-subset(forbs2,surface=="compass plant")
f2<-subset(forbs2,surface=="dandilion")
g2<-subset(forbs2,surface=="golden alexander")
h2<-subset(forbs2,surface=="grey headed coneflower")
i2<-subset(forbs2,surface=="hoary vervane")
j2<-subset(forbs2,surface=="morning glory")
k2<-subset(forbs2,surface=="mountain mint")
l2<-subset(forbs2,surface=="oxeye sunflower")
m2<-subset(forbs2,surface=="pale purple coneflower")
n2<-subset(forbs2,surface=="queen anne's lace")
o2<-subset(forbs2,surface=="red clover")
p2<-subset(forbs2,surface=="white clover")
q2<-subset(forbs2,surface=="white sweet clover")
r2<-subset(forbs2,surface=="wild bergamot")
s2<-subset(forbs2,surface=="yarrow")
unique(mw2$surface)
t2<-subset(mw2,surface=="butterfly milkweed")
u2<-subset(mw2,surface=="common milkweed")
v2<-subset(mw2,surface=="swamp milkweed")
w2<-subset(mw2,surface=="whorled milkweed")

nrow(w2)
min(w2$landonsteplength)
median(w2$landonsteplength)
max(w2$landonsteplength)




d<-ggplot(data=forbs2, aes(x=landonsteplength)) +
  geom_histogram(breaks=seq(0,250, by=1),
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
  scale_y_continuous(expand=c(0,0), limits=c(0,1.5))+
  scale_x_continuous(expand=c(0,0), limits=c(0,255))+
  theme(axis.text.x = element_text(size=14))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=14))+
  theme(axis.text.y=element_text(colour="black"))+
  ggtitle("D.")+
  theme(plot.title=element_text(size=26))+
  theme(axis.title = element_text(size = 15))
#annotate("text", x=90, y=30, label="Poor-Roadside, Crop")#,size=7)
d

c<-ggplot(data=mw2, aes(x=landonsteplength)) +
  geom_histogram(breaks=seq(0,250, by=1),
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
  scale_y_continuous(expand=c(0,0), limits=c(0,1.5))+
  scale_x_continuous(expand=c(0,0), limits=c(0,255))+
  theme(axis.text.x = element_text(size=14))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=14))+
  theme(axis.text.y=element_text(colour="black"))+
  ggtitle("C.")+
  theme(plot.title=element_text(size=26))+
  theme(axis.title = element_text(size = 15))
#annotate("text", x=90, y=30, label="Poor-Roadside, Crop")#,size=7)
c
grid.arrange(a,b,c,d,nrow=4)



grid.arrange(a,b,c,d,nrow=2)




#how many monarchs flew into the wind?
names(step)
intowind<-subset(step,perception=="yes")

percep<-merge(x = intowind, y = sumfly[ , c("monarchrunid","windbearing")], by = "monarchrunid", all.x=TRUE)
unique(percep$windbearing)
percep<-percep[!is.na(percep$windbearing),]

percep$windcor <- (percep$bearing - percep$windbearing)
percep$adjeucbear <- ifelse(percep$windcor <0, percep$windcor +360, 
                            ifelse(percep$windcor >360, percep$windcor -360, percep$windcor)) 
unique(percep$adjeucbear)
percep2<-percep %>% filter((adjeucbear >=315) | (adjeucbear <=45))






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


##
#what did they land on after a step >50m
landon<-read.csv("1920_locations_steplengths_2.csv", header=TRUE)
landonbig<-subset(landon, steplength>= 50)
unique(landonbig$landon)
table(landonbig$landon)


########################################################################################

step<-read.csv("1920_locations_steplengths_2.csv", header=TRUE)
sum<-read.csv("1920_trialsummary_4.csv",header=TRUE)

merge<-merge(x=step,y=sum[,c("monarchrunid","windbearing")], by="monarchrunid", all.x=TRUE)

merge$windcor <- (merge$bearing - merge$windbearing)
merge$adjstepbear <- ifelse(merge$windcor <0, merge$windcor +360, 
                            ifelse(merge$windcor >360, merge$windcor -360, merge$windcor)) 


##steps land on milkweed or forb
merge2<-subset(merge, landon == c("forb","milkweed"))

min(merge2$steplength, na.rm=T)
max(merge2$steplength, na.rm=T)


library(ggplot2)
ggplot(data=merge2, aes(x=steplength)) +
  geom_histogram(breaks=seq(0,240, by=10),
                 col="black",
                 fill="black",
                 alpha = .2) +
  xlab("Steplength to Resource")+ 
  ylab("Count")+
  theme_bw()+
  #ggtitle("Crop-Field")+
  #theme(plot.title=element_text(hjust = 0.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_y_continuous(expand=c(0,0), limits=c(0,130))+
  scale_x_continuous(expand=c(0,0), limits=c(0,240))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.y=element_text(colour="black"))+
  #ggtitle("a.")+
  theme(plot.title=element_text(size=14))+
  theme(axis.title = element_text(size = 13))

############
#to resource into wind and to resources with wind
library(circular)
library(dplyr)
names(merge2)
merge2$adjstepbeardeg<-circular(merge2$adjstepbear, units="degrees", template="geographics")
toreswind<-merge2 %>% filter((adjstepbeardeg >=315) | (adjstepbeardeg <=45))
toreswind5<-subset(toreswind, steplength <= 5)
toreswowind<-merge2 %>% filter(between(adjstepbeardeg,135,225))
toreswowind5<-subset(toreswowind, steplength <= 5)


##all steps into wind and land on resources
min(toreswind$steplength)
max(toreswind$steplength)

library(ggplot2)
ggplot(data=toreswind, aes(x=steplength)) +
  geom_histogram(breaks=seq(0,240, by=10),
                 col="black",
                 fill="black",
                 alpha = .2) +
  xlab("Steplength into Wind to Resource")+ 
  ylab("Count")+
  theme_bw()+
  #ggtitle("Crop-Field")+
  #theme(plot.title=element_text(hjust = 0.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_y_continuous(expand=c(0,0), limits=c(0,20))+
  scale_x_continuous(expand=c(0,0), limits=c(0,240))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.y=element_text(colour="black"))+
  #ggtitle("a.")+
  theme(plot.title=element_text(size=14))+
  theme(axis.title = element_text(size = 13))

library(ggplot2)
ggplot(data=toreswowind, aes(x=steplength)) +
  geom_histogram(breaks=seq(0,240, by=5),
                 col="black",
                 fill="black",
                 alpha = .2) +
  xlab("Steplength with Wind to Resource")+ 
  ylab("Count")+
  theme_bw()+
  #ggtitle("Crop-Field")+
  #theme(plot.title=element_text(hjust = 0.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_y_continuous(expand=c(0,0), limits=c(0,60))+
  scale_x_continuous(expand=c(0,0), limits=c(0,240))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.y=element_text(colour="black"))+
  #ggtitle("a.")+
  theme(plot.title=element_text(size=14))+
  theme(axis.title = element_text(size = 13))



#############
#how many monarchs flew into the wind or with the wind?
merge$adjstepbeardeg<-circular(merge$adjstepbear, units="degrees", template="geographics")
intowind<-merge %>% filter((adjstepbeardeg >=315) | (adjstepbeardeg <=45))
withwind<-merge %>% filter(between(adjstepbeardeg,135,225))


##########
#are big steps into wind?
merge3<-subset(merge, steplength >= 50)
merge3$adjstepbeardeg<-circular(merge3$adjstepbear, units="degrees", template="geographics")
intowindbig<-merge3 %>% filter((adjstepbeardeg >=315) | (adjstepbeardeg <=45))
withwindbig<-merge3 %>% filter(between(adjstepbeardeg,135,225))

table(intowindbig$landon)
table(withwindbig$landon)




#only olfactory cues?
merge4<-subset(merge, steplength >= 30)
merge4$adjstepbeardeg<-circular(merge4$adjstepbear, units="degrees", template="geographics")
intowindolf<-merge4 %>% filter((adjstepbeardeg >=315) | (adjstepbeardeg <=45))
withwindolf<-merge4 %>% filter(between(adjstepbeardeg,135,225))
#land on res
merge5<-subset(merge2, steplength >= 30)
merge5$adjstepbeardeg<-circular(merge5$adjstepbear, units="degrees", template="geographics")
intowindolfr<-merge5 %>% filter((adjstepbeardeg >=315) | (adjstepbeardeg <=45))
withwindolfr<-merge5 %>% filter(between(adjstepbeardeg,135,225))

#both olfaction and vision
merge6<-subset(merge, steplength <= 30)
merge6$adjstepbeardeg<-circular(merge6$adjstepbear, units="degrees", template="geographics")
intowindmix<-merge6 %>% filter((adjstepbeardeg >=315) | (adjstepbeardeg <=45))
withwindmix<-merge6 %>% filter(between(adjstepbeardeg,135,225))
#land on res
merge7<-subset(merge2, steplength <= 30)
merge7$adjstepbeardeg<-circular(merge7$adjstepbear, units="degrees", template="geographics")
intowindmixr<-merge7 %>% filter((adjstepbeardeg >=315) | (adjstepbeardeg <=45))
withwindmixr<-merge7 %>% filter(between(adjstepbeardeg,135,225))









######
#regression between euclidean distance and wind speed?

scatter.smooth(x=sumfly$euclidiandistance, y=sumfly$windkph)
cor(sumfly$windkph,sumfly$euclidiandistance)

