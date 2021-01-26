setwd("C:/Users/kefisher/Box/Publications/19&20 Habitat Utilization/Data Analysis")
turn<-read.csv("1920_locations_turnangles.csv", header=TRUE)
monarchs<-read.csv("1920_trialmonarchs.csv", header=TRUE)
turn<-merge(monarchs,turn,by="monarchrunid")
turn<-turn[!is.na(turn$turnwithin),]
turn$turn360 <- ifelse(turn$turnangle <0, turn$turnangle +360, turn$turnangle)

unique(turn$turnwithin)

library(ggplot2)
library(grid)
library(dplyr)
library(lubridate)
library(circular)

################################################################################################
unique(turn$spatialconfig)
m<-subset(turn, spatialconfig == "mixed")
rr<-subset(turn, spatialconfig == "rich-roadside")
pr<-subset(turn, spatialconfig == "poor-roadside")

mrich<-subset(m, landcoverclassification == "rich")
mpoor<-subset(m, landcoverclassification == "poor")
mcrop<-subset(m, landcoverclassification == "crop")
rrrich<-subset(rr, landcoverclassification == "rich")
rrpoor<-subset(rr, landcoverclassification == "poor")
rrcrop<-subset(rr, landcoverclassification == "crop")
prpoor<-subset(pr, landcoverclassification == "poor")
prcrop<-subset(pr, landcoverclassification == "crop")
rpoor<-rbind(rrpoor,prpoor)

mrichc<-circular(mrich$turn360, units="degrees", type = "angles", template="none", zero=pi/2, rotation="clock")
mpoorc<-circular(mpoor$turn360, units="degrees", type = "angles", template="none", zero=pi/2, rotation="clock")
mcropc<-circular(mcrop$turn360, units="degrees", type = "angles", template="none", zero=pi/2, rotation="clock")
rrrichc<-circular(rrrich$turn360, units="degrees", type = "angles", template="none", zero=pi/2, rotation="clock")
rrpoorc<-circular(rrpoor$turn360, units="degrees", type = "angles", template="none", zero=pi/2, rotation="clock")
rrcropc<-circular(rrcrop$turn360, units="degrees", type = "angles", template="none", zero=pi/2, rotation="clock")
prpoorc<-circular(prpoor$turn360, units="degrees", type = "angles", template="none", zero=pi/2, rotation="clock")
prcropc<-circular(prcrop$turn360, units="degrees", type = "angles", template="none", zero=pi/2, rotation="clock")
rpoorc<-circular(rpoor$turn360, units="degrees", type = "angles", template="none", zero=pi/2, rotation="clock")

#homogeneity in spread?
watson.wheeler.test(list(mrichc,mpoorc,mcropc,rrrichc,rrpoorc,rrcropc,prcropc,prpoorc))

#rich habitat
watson.wheeler.test(list(mrichc,rrrichc))

#crop habitat
watson.wheeler.test(list(mcropc,rrcropc,prcropc))

#poor habitat
watson.wheeler.test(list(mpoorc,rrpoorc,prpoorc))
#compare poor linear
#compare both poor linear to poor mixed
watson.wheeler.test(list(mpoorc,rpoorc))

#mixed
watson.wheeler.test(list(mrichc,mpoorc,mcropc))
#rich roadside
watson.wheeler.test(list(rrrichc,rrpoorc,rrcropc))
#poor roadside
watson.wheeler.test(list(prcropc,prpoorc))


#######################################################
mcropplot<-ggplot(data=mcrop, aes(x=turnangle)) +
  geom_histogram(breaks=seq(-180, 180, by=10),
                 col="black",
                 fill="black",
                 alpha = .2) +
  xlab("") +
  ylab("")+
  theme_bw()+
  #ggtitle("Crop-Field")+
  #theme(plot.title=element_text(hjust = 0.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_y_continuous(expand=c(0,0), limits=c(0,18))+
  scale_x_continuous(expand=c(0,0), limits=c(-180,180))+
  #theme(axis.text.x = element_text(size=20))+
  theme(axis.text.x=element_text(colour="black"))+
  #theme(axis.text.y = element_text(size=20))+
  theme(axis.text.y=element_text(colour="black"))+
  #ggtitle("b.")+
  #theme(plot.title=element_text(size=12))
  #theme(axis.title = element_text(size = 25))+
  #annotate("text", x=90, y=30, label="Poor-Roadside, Crop")#,size=7)

mrichplot
mpoorplot
mcropplot
rrrichplot
rrpoorplot
rrcropplot
prpoorplot
prcropplot

test<-as.data.frame(NA)
placeholder<-ggplot(test)

grid.arrange(mrichplot, mcropplot, mpoorplot,
             rrrichplot, rrcropplot, rrpoorplot,
             placeholder, prcropplot, prpoorplot)



nrow(prcrop)

  
