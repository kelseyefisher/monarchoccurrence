setwd("C:/Users/kelse/Box Sync/Publications/19&20 Habitat Utilization/Data Analysis/CTMM")

ctmm19<-read.csv("19_raster_occurrencebycell.csv", header=TRUE)
head(ctmm19)

ctmm19_2<-aggregate(Count~Release+Occu+Classification,ctmm19,sum)
head(ctmm19_2)
write.csv(ctmm19_2,"19_raster_occurrencebycell_sum.csv")

sumctmm19<-read.csv("19_occu_sumbyrelease3.csv",header=TRUE)
sumctmm19

#they are not even across release sites
chisq.test(sumctmm19$propcrop)
chisq.test(sumctmm19$propedge)
chisq.test(sumctmm19$proppoor)
chisq.test(sumctmm19$proprich)

sumctmm19_2<-read.csv("19_occu_sumbyrelease4.csv",header=TRUE)
sumctmm19_2


#they are not even across release sites
chisq.test(sumctmm19_2$prop, sumctmm19_2$crop)
chisq.test(sumctmm19_2$edge)
chisq.test(sumctmm19_2$poor)
chisq.test(sumctmm19_2$rich)





#allrelease<-subset(sumctmm19, Release == "all")
#croprelease<-subset(sumctmm19, Release == "crop")
#edgerelease<-subset(sumctmm19, Release == "edge")
#poorrelease<-subset(sumctmm19, Release == "poor")
#richrelease<-subset(sumctmm19, Release == "rich")

#chisq.test(allrelease$Occu, allrelease$Prop)
#chisq.test(croprelease$Occu, croprelease$Prop)
#chisq.test(edgerelease$Occu, edgerelease$Prop)
#chisq.test(poorrelease$Occu, poorrelease$Prop)
#chisq.test(richrelease$Occu, richrelease$Prop)


sumctmm19_3<-read.csv("19_occu_sumbyrelease2.csv",header=TRUE)
sumctmm19_3$Perc<-sumctmm19_3$Prop*100
sumctmm19_3

library(ggplot2)
pal<-c("white","grey75","grey43","grey15")

ggplot(sumctmm19_3, aes(fill=Occu, y=Prop, x=Release, width=.8))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  scale_fill_manual(values=pal,
                    name="Occurrence in Habitat",
                    breaks=c("crop","edge","poor","rich"),
                    labels=c("Crop","Edge","Poor","Rich"))+
  xlab("Release Habitiat") +
  ylab("Prop Occurrence")+
  theme_bw()+
  #geom_errorbar(aes(ymin=Freq-sd, ymax=Freq+sd), width=.2, 
  #position=position_dodge(.8))+
  #geom_text(aes(label=Sig, y=Freq+(sd)),vjust=-1.5, size=5, position=position_dodge(.8))+
  scale_y_continuous(expand=c(0,0), limits=c(0,1.10))+
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
  theme(legend.position = "bottom")



######################################################################################
setwd("C:/Users/kefisher/Box/Publications/19&20 Habitat Utilization/Data Analysis")

summary<-read.csv("1920_trialsummary_2.csv", header=TRUE)
a<-read.csv("2019_Occurrence_All.csv", header=TRUE)
aa<-merge(a,summary,by="ctmmlabel")
b<-read.csv("2020_Occurrence_East.csv", header=TRUE)
c<-read.csv("2020_Occurrence_West.csv", header=TRUE)
bb<-merge(b,summary,by="ctmmlabel")
cc<-merge(c,summary,by="ctmmlabel")

ctmm<-rbind(aa,bb,cc)
head(ctmm)

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

names(ctmm)

ctmm2<-aggregate(count~ctmmlabel+class+occu+Release,ctmm,sum)
ctmm3<-aggregate(count~ctmmlabel+class+Release,ctmm,sum)

write.csv(ctmm2,"1920_aggregrateoccurrence.csv")
ctmm4<-read.csv("1920_aggregrateoccurrence.csv",header=TRUE)

totalcells<-aggregate(count~ctmmlabel,ctmm,sum)
totalcells






<-glm(Freq~Release,data=timescross,family = gaussian(link = "identity"))
emm<-emmeans(freqcross,"Release",type="response")
joint_tests(emm)
pairs(emm)
emm
CLD(emm)
#pwpp(emm)


SE<- summarySE(ctmm2, measurevar="count", groupvars=c("Release","class","occu"))
SE

library(ggplot2)
pal<-c("grey75","grey43","grey15","white")

ggplot(SE, aes(fill=class, y=count, x=Release, width=.8))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  scale_fill_manual(values=pal,
                    name="Occurrence in Habitat",
                    breaks=c("crop","edge","poor","rich"),
                    labels=c("Crop","Edge","Poor","Rich"))+
  xlab("Release Habitiat") +
  ylab("Count Occurrence")+
  theme_bw()
  #geom_errorbar(aes(ymin=Freq-sd, ymax=Freq+sd), width=.2, 
  #position=position_dodge(.8))+
  #geom_text(aes(label=Sig, y=Freq+(sd)),vjust=-1.5, size=5, position=position_dodge(.8))+
  scale_y_continuous(expand=c(0,0), limits=c(0,1.10))+
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
  theme(legend.position = "bottom")
