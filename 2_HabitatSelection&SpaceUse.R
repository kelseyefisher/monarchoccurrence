setwd("C:/Users/kefisher/Box/Publications/19&20 Habitat Utilization/Data Analysis")
summary<-read.csv("1920_trialsummary_4.csv", header=TRUE)
step<-read.csv("1920_locations_steplengths_6.csv", header=TRUE)

unique(summary$Release)

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
library(car)


#################################################################################
#monarchs to remain in habitat vs cross a boundary

#WITHOUT EDGE should equal 95; 74 cross;21 remain
table(summary$Release, summary$remaincross)
releasehab<-c("MZD","RZD","MLD","MHD","RHD")
releaseclass<-c("crop","poor","poor","rich","poor")
spatialconfig<-c("mixed","road","mixed","mixed","road")
released<-c(18,19,20,18,20)
cross<-c(16,16,15,10,17)
remain<-c(2,3,5,8,3)
monarchscross<-data.frame(releasehab,releaseclass,spatialconfig,released,cross,remain)
monarchscross
monarchscross$propremain<-(monarchscross$remain/monarchscross$released)
monarchscross$percremain<-(monarchscross$propremain*100)

sum(monarchscross$remain)
sum(monarchscross$cross)

#corn, east, grass, prairie, west
chisq.test(c(11.11111,15.78947,25.00000,44.44444,15.00000))
chisq.test(c(11.11111,15.78947))
chisq.test(c(11.11111,25.00000))
chisq.test(c(11.11111,44.44444))#
chisq.test(c(11.11111,15.00000))
chisq.test(c(15.78947,25.00000))
chisq.test(c(15.78947,44.44444))#
chisq.test(c(15.78947,15.00000))
chisq.test(c(25.00000,44.44444))
chisq.test(c(25.00000,15.00000))
chisq.test(c(44.44444,15.00000))#

0.05/11

sig<-c("b","b","ab","a","b")

monarchscross$releasehab<-factor(monarchscross$releasehab, levels=c("MHD","MLD","MZD","RHD","RZD"))

#for manuscript
ggplot(monarchscross, aes(y=percremain, x=releasehab, width=.8))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Release Habitat Class") +
  ylab("% Monarchs Remaining in Release Habitat")+
  theme_bw()+
  scale_fill_brewer(palette="YlGnBu")+
  scale_y_continuous(expand=c(0,0), limits=c(0,102))+
  geom_text(aes(label=sig, y=(percremain+2)),colour="black",vjust=0, size=5, position=position_dodge(.8),fontface="plain")+
  #geom_text(aes(label=sigexit, y=(count+2)),colour="deepskyblue3",vjust=-1.5, size=5, position=position_dodge(.8),fontface="italic")+
  #geom_text(aes(label=sigenter, y=(count+2)),colour="darkslategray",vjust=-1.5, size=5, position=position_dodge(.8),fontface="plain")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=14))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=14))+
  theme(axis.text.y=element_text(colour="black"))+
  scale_x_discrete(labels=label_wrap_gen(width=10))+
  theme(plot.title=element_text(size=18))+
  theme(axis.title = element_text(size = 18))+
  #ggtitle("c.")+
  theme(legend.position = "bottom")+
  guides(fill=guide_legend(title="Habitat Class"))+
  theme(legend.text = element_text(size = 14))+
  theme(legend.title = element_text(size = 14))+
  theme(legend.box.background=element_rect(colour = "black"),
        legend.background = element_blank())




######################
#time elapsed prior to first time crossing boundary
#time before crossing
timebeforecross<-summary[!is.na(summary$timebeforecross),]
timebeforecross<-subset(timebeforecross, Release == "corn" | Release == "prairie" | Release == "grass" | Release == "east" | Release =="west")
names(timebeforecross)
unique(timebeforecross$Release)

min(timebeforecross$timebeforecross)
max(timebeforecross$timebeforecross)
median(timebeforecross$timebeforecross)

library(emmeans)
time<-glm(timebeforecross~Release,data=timebeforecross,family = gaussian(link = "identity"))
emm<-emmeans(time,c("Release"),type="response")
joint_tests(emm)
pairs(emm)
emm
CLD(emm)
#pwpp(emm)\


##########################
#monarchs released on edge - 12 went to rich, 5 went to crop first, 2 exited the focal research area
edge<-prop.test(x=c(12,5),n=c(19,19))
edge

#chisq.test(c(12,5))
chisq.test(c(63.158,26.316))

###############################################################################

# ##################################################################################################
# ####################################################################################################
# #Space Use
# 
# #############################################
# #Occurrence
# 
# occu<-read.csv("1920_Occurrence_All.csv",header=TRUE)
# names(occu)
# unique(occu$spatialconfig)
# 
# occumosaic<-subset(occu, spatialconfig == "mosaic")
# occuwith<-subset(occu, spatialconfig == "roadsidewith")
# occuwithout<-subset(occu, spatialconfig == "roadsidewithout")
# occuroad<-rbind(occuwith,occuwithout)
# occu2<-rbind(occumosaic,occuwith,occuwithout)
# 
# occumosaichab<-aggregate(count~class,occumosaic,sum)
# occumosaichab
# sum(occumosaichab$count)
# occumosaichab$totaloccu<-c(23332483,23332483,23332483,23332483)
# occumosaichab$occuprop<-(occumosaichab$count/occumosaichab$totaloccu)
# occumosaichab$occuprop100<-(occumosaichab$occuprop*100)
# occumosaichab
# 
# chisq.test(c(8.495680,5.592847,16.502937,69.408537))
# chisq.test(c(8.495680,5.592847))
# chisq.test(c(8.495680,16.502937))
# chisq.test(c(8.495680,69.408537))#
# chisq.test(c(5.592847,16.502937))
# chisq.test(c(5.592847,69.408537))#
# chisq.test(c(16.502937,69.408537))#
# 
# ##
# 
# occuroadhab<-aggregate(count~class,occuroad,sum)
# occuroadhab
# sum(occuroadhab$count)
# occuroadhab$totaloccu<-c(654799,654799,654799,654799)
# occuroadhab$occuprop<-(occuroadhab$count/occuroadhab$totaloccu)
# occuroadhab$occuprop100<-(occuroadhab$occuprop*100)
# occuroadhab
# 
# chisq.test(c(65.630522,15.322870,17.930082,1.116526))
# chisq.test(c(65.630522,15.322870))
# chisq.test(c(65.630522,17.930082))
# chisq.test(c(65.630522,1.116526))
# chisq.test(c(15.322870,17.930082))
# chisq.test(c(15.322870,1.116526))
# chisq.test(c(17.930082,1.116526))
# 
# 
# ##
#   
# occuwithhab<-aggregate(count~class,occuwith,sum)
# occuwithhab
# sum(occuwithhab$count)
# occuwithhab$totaloccu<-c(98957,98957,98957,98957)
# occuwithhab$occuprop<-(occuwithhab$count/occuwithhab$totaloccu)
# occuwithhab$occuprop100<-(occuwithhab$occuprop*100)
# occuwithhab
# 
# chisq.test(c(34.304799,23.363683,36.887739,5.443779))
# chisq.test(c(34.304799,23.363683))
# chisq.test(c(34.304799,36.887739))
# chisq.test(c(34.304799,5.443779))#
# chisq.test(c(23.363683,36.887739))
# chisq.test(c(23.363683,5.443779))#
# chisq.test(c(36.887739,5.443779))#
# 
# ##
# 
# occuwithouthab<-aggregate(count~class,occuwithout,sum)
# occuwithouthab
# sum(occuwithouthab$count)
# occuwithouthab$totaloccu<-c(555842,555842,555842,555842)
# occuwithouthab$occuprop<-(occuwithouthab$count/occuwithouthab$totaloccu)
# occuwithouthab$occuprop100<-(occuwithouthab$occuprop*100)
# occuwithouthab
# 
# chisq.test(c(71.2074654,13.8913576,14.5550354,0.3461415))
# chisq.test(c(71.2074654,13.8913576))
# chisq.test(c(71.2074654,14.5550354))
# chisq.test(c(71.2074654,0.3461415))
# chisq.test(c(13.8913576,14.5550354))
# chisq.test(c(13.8913576,0.3461415))
# chisq.test(c(14.5550354,0.3461415))
# 
# ###
# 
# spconfig<-c("Mosaic","Mosaic","Mosaic","Mosaic",
#            "Roadside With Resource-Rich","Roadside With Resource-Rich","Roadside With Resource-Rich","Roadside With Resource-Rich",
#            "Roadside Without Resource-Rich","Roadside Without Resource-Rich","Roadside Without Resource-Rich","Roadside Without Resource-Rich")
# habitat<-c("Crop","Edge","Resource-Poor","Resource-Rich",
#            "Crop","Edge","Resource-Poor","Resource-Rich",
#            "Crop","Edge","Resource-Poor","Resource-Rich")
# occuperc<-c(8.495680,5.592847,16.502937,69.408537,
#             34.304799,23.363683,36.887739,5.443779,
#             71.2074654,13.8913576,14.5550354,0.3461415)
# msig<-c("a","a","a","b","","","","","","","","")
# wsig<-c("","","","","a","a","a","b","","","","")
# wosig<-c("","","","","","","","","a","b","b","c")
# 
# forpropgraph<-data.frame(spconfig,habitat,occuperc)
# names(forpropgraph)
# 
# #for manuscript
# a<-ggplot(forpropgraph, aes(fill=habitat, y=occuperc, x=spconfig, width=.8))+
#   geom_bar(stat="identity", color="black",
#            position=position_dodge())+
#   xlab("Habitat Configuration") +
#   ylab("Percent of Cells with \nNon-zero Occurrence Probability")+
#   theme_bw()+
#   scale_fill_brewer(palette="YlGnBu")+
#   scale_y_continuous(expand=c(0,0), limits=c(0,100))+
#   geom_text(aes(label=msig, y=(occuperc+2)),colour="darkolivegreen4",vjust=-1.5, size=5, position=position_dodge(.8),fontface="bold")+
#   geom_text(aes(label=wsig, y=(occuperc+2)),colour="deepskyblue3",vjust=-1.5, size=5, position=position_dodge(.8),fontface="italic")+
#   geom_text(aes(label=wosig, y=(occuperc+2)),colour="darkslategray",vjust=-1.5, size=5, position=position_dodge(.8),fontface="plain")+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"))+
#   theme(axis.text.x = element_text(size=14))+
#   theme(axis.text.x=element_text(colour="black"))+
#   theme(axis.text.y = element_text(size=14))+
#   theme(axis.text.y=element_text(colour="black"))+
#   scale_x_discrete(labels=label_wrap_gen(width=10))+
#   theme(plot.title=element_text(size=18))+
#   theme(axis.title = element_text(size = 16))+
#   ggtitle("a.")+
#   theme(legend.position = "none")
#   # guides(fill=guide_legend(title="Habitat Class"))+
#   # theme(legend.text = element_text(size = 14))+
#   # theme(legend.title = element_text(size = 14))+
#   # theme(legend.box.background=element_rect(colour = "black"),
#   #       legend.background = element_blank())
# 
# 
# #############################################
# #HIGH Occurrence
# 
# occu<-read.csv("1920_Occurrence_All.csv",header=TRUE)
# names(occu)
# unique(occu$spatialconfig)
# unique(occu$occu)
# 
# hoccumosaic<-subset(occu, spatialconfig == "mosaic"&occu=="high")
# hoccuwith<-subset(occu, spatialconfig == "roadsidewith"&occu=="high")
# hoccuwithout<-subset(occu, spatialconfig == "roadsidewithout"&occu=="high")
# hoccuroad<-rbind(hoccuwith,hoccuwithout)
# hoccu2<-rbind(hoccumosaic,hoccuwith,occuwithout)
# 
# hoccumosaichab<-aggregate(count~class,hoccumosaic,sum)
# hoccumosaichab
# sum(hoccumosaichab$count)
# hoccumosaichab$totaloccu<-c(962,962,962,962)
# hoccumosaichab$occuprop<-(hoccumosaichab$count/hoccumosaichab$totaloccu)
# hoccumosaichab$occuprop100<-(hoccumosaichab$occuprop*100)
# hoccumosaichab
# 
# chisq.test(c(2.079002,3.846154,59.043659,35.031185))
# chisq.test(c(2.079002,3.846154))
# chisq.test(c(2.079002,59.043659))
# chisq.test(c(2.079002,35.031185))
# chisq.test(c(3.846154,59.043659))
# chisq.test(c(3.846154,35.031185))
# chisq.test(c(59.043659,35.031185))
# 
# ##
# 
# hoccuwithhab<-aggregate(count~class,hoccuwith,sum)
# hoccuwithhab
# sum(hoccuwithhab$count)
# hoccuwithhab$totaloccu<-c(66,66,66,66)
# hoccuwithhab$occuprop<-(hoccuwithhab$count/hoccuwithhab$totaloccu)
# hoccuwithhab$occuprop100<-(hoccuwithhab$occuprop*100)
# hoccuwithhab
# 
# chisq.test(c(4.545455,13.636364,48.484848,33.333333))
# chisq.test(c(4.545455,13.636364))
# chisq.test(c(4.545455,48.484848))
# chisq.test(c(4.545455,33.333333))
# chisq.test(c(13.636364,48.484848))
# chisq.test(c(13.636364,33.333333))
# chisq.test(c(48.484848,33.333333))
# 
# 
# ##
# 
# hoccuwithouthab<-aggregate(count~class,hoccuwithout,sum)
# hoccuwithouthab
# sum(hoccuwithouthab$count)
# hoccuwithouthab$totaloccu<-c(44,44)
# hoccuwithouthab$occuprop<-(hoccuwithouthab$count/hoccuwithouthab$totaloccu)
# hoccuwithouthab$occuprop100<-(hoccuwithouthab$occuprop*100)
# hoccuwithouthab
# 
# chisq.test(c(56.81818,0,43.18182,0))
# chisq.test(c(56.81818,0))
# chisq.test(c(56.81818,43.18182))
# chisq.test(c(56.81818,0))
# chisq.test(c(0,43.18182))
# chisq.test(c(0,0))
# chisq.test(c(43.18182,0))
# 
# ###
# 
# spconfig<-c("Mosaic","Mosaic","Mosaic","Mosaic",
#             "Roadside With Resource-Rich","Roadside With Resource-Rich","Roadside With Resource-Rich","Roadside With Resource-Rich",
#             "Roadside Without Resource-Rich","Roadside Without Resource-Rich","Roadside Without Resource-Rich","Roadside Without Resource-Rich")
# habitat<-c("Crop","Edge","Resource-Poor","Resource-Rich",
#            "Crop","Edge","Resource-Poor","Resource-Rich",
#            "Crop","Edge","Resource-Poor","Resource-Rich")
# occuperc<-c(2.079002,3.846154,59.043659,35.031185,
#             4.545455,13.636364,48.484848,33.333333,
#             56.81818,0,43.18182,0)
# msig2<-c("a","a","b","b","","","","","","","","")
# wsig2<-c("","","","","a","a","b","b","","","","")
# wosig2<-c("","","","","","","","","a","b","a","b")
# 
# forpropgraph2<-data.frame(spconfig,habitat,occuperc)
# names(forpropgraph2)
# 
# #for manuscript
# b<-ggplot(forpropgraph2, aes(fill=habitat, y=occuperc, x=spconfig, width=.8))+
#   geom_bar(stat="identity", color="black",
#            position=position_dodge())+
#   xlab("Habitat Configuration") +
#   ylab("Percent of Cells with High \nOccurrence Probability (> 0.67)")+
#   theme_bw()+
#   scale_fill_brewer(palette="YlGnBu")+
#   scale_y_continuous(expand=c(0,0), limits=c(0,100))+
#   geom_text(aes(label=msig2, y=(occuperc+2)),colour="darkolivegreen4",vjust=-1.5, size=5, position=position_dodge(.8),fontface="bold")+
#   geom_text(aes(label=wsig2, y=(occuperc+2)),colour="deepskyblue3",vjust=-1.5, size=5, position=position_dodge(.8),fontface="italic")+
#   geom_text(aes(label=wosig2, y=(occuperc+2)),colour="darkslategray",vjust=-1.5, size=5, position=position_dodge(.8),fontface="plain")+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"))+
#   theme(axis.text.x = element_text(size=14))+
#   theme(axis.text.x=element_text(colour="black"))+
#   theme(axis.text.y = element_text(size=14))+
#   theme(axis.text.y=element_text(colour="black"))+
#   scale_x_discrete(labels=label_wrap_gen(width=10))+
#   theme(plot.title=element_text(size=18))+
#   theme(axis.title = element_text(size = 16))+
#   ggtitle("b.")+
#   theme(legend.position = "bottom")+
#   guides(fill=guide_legend(title="Habitat Class"))+
#   theme(legend.text = element_text(size = 14))+
#   theme(legend.title = element_text(size = 14))+
#   theme(legend.box.background=element_rect(colour = "black"),
#         legend.background = element_blank())
# 
# grid.arrange(a,b)


#############################################################
##################################################################################################
####################################################################################################
#Space Use
#############################################
#Occurrence total, then by release; sum of all cells for all individuals

occu<-read.csv("1920_Occurrence_All.csv",header=TRUE)
names(occu)
unique(occu$spatialconfig)
unique(occu$release)

occumosaic<-subset(occu, spatialconfig == "mosaic")
occuwith<-subset(occu, spatialconfig == "roadsidewith")
occuwithout<-subset(occu, spatialconfig == "roadsidewithout")
occuroad<-rbind(occuwith,occuwithout)
mcrop<-subset(occu,release=="crop")
medge<-subset(occu,release=="edge")
mpoor<-subset(occu,release=="poor")
mrich<-subset(occu,release=="rich")
rw<-subset(occu,release=="poor-with")
rwo<-subset(occu,release=="poor-without")

#mosaic all
occumosaichab<-aggregate(count~class,occumosaic,sum)
occumosaichab
sum(occumosaichab$count)
occumosaichab$totaloccu<-c(23332483,23332483,23332483,23332483)
occumosaichab$occuprop<-(occumosaichab$count/occumosaichab$totaloccu)
occumosaichab$occuprop100<-(occumosaichab$occuprop*100)
occumosaichab

chisq.test(c(8.495680,5.592847,16.502937,69.408537))
chisq.test(c(8.495680,5.592847))
chisq.test(c(8.495680,16.502937))
chisq.test(c(8.495680,69.408537))#
chisq.test(c(5.592847,16.502937))
chisq.test(c(5.592847,69.408537))#
chisq.test(c(16.502937,69.408537))#


#roadside all
occuroadhab<-aggregate(count~class,occuroad,sum)
occuroadhab
sum(occuroadhab$count)
occuroadhab$totaloccu<-c(654799,654799,654799,654799)
occuroadhab$occuprop<-(occuroadhab$count/occuroadhab$totaloccu)
occuroadhab$occuprop100<-(occuroadhab$occuprop*100)
occuroadhab

chisq.test(c(65.630522,15.322870,17.930082,1.116526))
chisq.test(c(65.630522,15.322870))
chisq.test(c(65.630522,17.930082))
chisq.test(c(65.630522,1.116526))
chisq.test(c(15.322870,17.930082))
chisq.test(c(15.322870,1.116526))
chisq.test(c(17.930082,1.116526))


#mcrop
mcrophab<-aggregate(count~class,mcrop,sum)
mcrophab
sum(mcrophab$count)
mcrophab$totaloccu<-c(1123707,1123707,1123707,1123707)
mcrophab$occuprop<-(mcrophab$count/mcrophab$totaloccu)
mcrophab$occuprop100<-(mcrophab$occuprop*100)
mcrophab

chisq.test(c(9.356443,6.997109,12.975891,70.670557))
chisq.test(c(9.356443,6.997109))
chisq.test(c(9.356443,12.975891))
chisq.test(c(9.356443,70.670557))
chisq.test(c(6.997109,12.975891))
chisq.test(c(6.997109,70.670557))
chisq.test(c(12.975891,70.670557))


#medge
medgehab<-aggregate(count~class,medge,sum)
medgehab
sum(medgehab$count)
medgehab$totaloccu<-c(11282776,11282776,11282776,11282776)
medgehab$occuprop<-(medgehab$count/medgehab$totaloccu)
medgehab$occuprop100<-(medgehab$occuprop*100)
medgehab

chisq.test(c(16.558230,5.276981,7.082300,71.082489))
chisq.test(c(16.558230,5.276981))
chisq.test(c(16.558230,7.082300))
chisq.test(c(16.558230,71.082489))
chisq.test(c(5.276981,7.082300))
chisq.test(c(5.276981,71.082489))
chisq.test(c(7.082300,71.082489))


#mrich
mrichhab<-aggregate(count~class,mrich,sum)
mrichhab
sum(mrichhab$count)
mrichhab$totaloccu<-c(9670639,9670639,9670639,9670639)
mrichhab$occuprop<-(mrichhab$count/mrichhab$totaloccu)
mrichhab$occuprop100<-(mrichhab$occuprop*100)
mrichhab

chisq.test(c(0.09188638,5.83123825,19.12573719,74.95113818))
chisq.test(c(0.09188638,5.83123825))
chisq.test(c(0.09188638,19.12573719))
chisq.test(c(0.09188638,74.95113818))
chisq.test(c(5.83123825,19.12573719))
chisq.test(c(5.83123825,74.95113818))
chisq.test(c(19.12573719,74.95113818))


#mpoor
mpoorhab<-aggregate(count~class,mpoor,sum)
mpoorhab
sum(mpoorhab$count)
mpoorhab$totaloccu<-c(1255361,1255361,1255361)
mpoorhab$occuprop<-(mpoorhab$count/mpoorhab$totaloccu)
mpoorhab$occuprop100<-(mpoorhab$occuprop*100)
mpoorhab

chisq.test(c(0,5.338305,84.125045,10.536650))
chisq.test(c(0,5.338305))
chisq.test(c(0,84.125045))
chisq.test(c(0,10.536650))
chisq.test(c(5.338305,84.125045))
chisq.test(c(5.338305,10.536650))
chisq.test(c(84.125045,10.536650))


#rw
rwhab<-aggregate(count~class,rw,sum)
rwhab
sum(rwhab$count)
rwhab$totaloccu<-c(98957,98957,98957,98957)
rwhab$occuprop<-(rwhab$count/rwhab$totaloccu)
rwhab$occuprop100<-(rwhab$occuprop*100)
rwhab

chisq.test(c(34.304799,23.363683,36.887739,5.443779))
chisq.test(c(34.304799,23.363683))
chisq.test(c(34.304799,36.887739))
chisq.test(c(34.304799,5.443779))
chisq.test(c(23.363683,36.887739))
chisq.test(c(23.363683,5.443779))
chisq.test(c(36.887739,5.443779))


#rwo
rwohab<-aggregate(count~class,rwo,sum)
rwohab
sum(rwohab$count)
rwohab$totaloccu<-c(555842,555842,555842,555842)
rwohab$occuprop<-(rwohab$count/rwohab$totaloccu)
rwohab$occuprop100<-(rwohab$occuprop*100)
rwohab

chisq.test(c(71.2074654,13.8913576,14.5550354,0.3461415))
chisq.test(c(71.2074654,13.8913576))
chisq.test(c(71.2074654,14.5550354))
chisq.test(c(71.2074654,0.3461415))
chisq.test(c(13.8913576,14.5550354))
chisq.test(c(13.8913576,0.3461415))
chisq.test(c(14.5550354,0.3461415))

# ###
# #graph
# ###
# ##non-zero occurrence graph
# 
# release<-c("Mosaic","Mosaic","Mosaic","Mosaic",
#            "Roadside","Roadside","Roadside","Roadside",
#            "Crop","Crop","Crop","Crop",
#            "Edge","Edge","Edge","Edge",
#            "Resource- Rich","Resource- Rich","Resource- Rich","Resource- Rich",
#            "Resource- Poor","Resource- Poor","Resource- Poor","Resource- Poor",
#            "With Resource- Rich","With Resource- Rich","With Resource- Rich","With Resource- Rich",
#            "Without Resource- Rich","Without Resource- Rich","Without Resource- Rich","Without Resource- Rich")
# habitat<-c("Crop","Edge","Resource-Poor","Resource-Rich",
#            "Crop","Edge","Resource-Poor","Resource-Rich",
#            "Crop","Edge","Resource-Poor","Resource-Rich",
#            "Crop","Edge","Resource-Poor","Resource-Rich",
#            "Crop","Edge","Resource-Poor","Resource-Rich",
#            "Crop","Edge","Resource-Poor","Resource-Rich",
#            "Crop","Edge","Resource-Poor","Resource-Rich",
#            "Crop","Edge","Resource-Poor","Resource-Rich")
# occuperc<-c(8.495680,5.592847,16.502937,69.408537,
#             65.630522,15.322870,17.930082,1.116526,
#             9.356443,6.997109,12.975891,70.670557,
#             16.558230,5.276981,7.082300,71.082489,
#             0.09188638,5.83123825,19.12573719,74.95113818,
#             0,5.338305,84.125045,10.536650,
#             34.304799,23.363683,36.887739,5.443779,
#             71.2074654,13.8913576,14.5550354,0.3461415)
# 
# byrelease<-data.frame(release,habitat,occuperc)
# names(byrelease)
# 
# byrelease$release<-factor(byrelease$release, levels=c("Mosaic","Roadside","Crop","Edge","Resource- Rich","Resource- Poor","With Resource- Rich","Without Resource- Rich"))
# 
# sig1<-c("a","a","a","b","","","","","","","","","","","","","","","","","","","","","","","","","","","","")
# sig2<-c("","","","","a","b","b","c","","","","","","","","","","","","","","","","","","","","","","","","")
# sig3<-c("","","","","","","","","a","a","a","b","","","","","","","","","","","","","","","","","","","","")
# sig4<-c("","","","","","","","","","","","","a","a","a","b","","","","","","","","","","","","","","","","")
# sig5<-c("","","","","","","","","","","","","","","","","a","ab","b","c","","","","","","","","","","","","")
# sig6<-c("","","","","","","","","","","","","","","","","","","","","a","ab","c","b","","","","","","","","")
# sig7<-c("","","","","","","","","","","","","","","","","","","","","","","","","a","a","a","b","","","","")
# sig8<-c("","","","","","","","","","","","","","","","","","","","","","","","","","","","","a","b","b","c")
# 
# #for manuscript
# A<-ggplot(byrelease, aes(fill=habitat, y=occuperc, x=release, width=.8))+
#   geom_bar(stat="identity", color="black",
#            position=position_dodge())+
#   xlab("Release") +
#   ylab("% Raster Cells with \nNon-Zero Occurrence Probability")+
#   theme_bw()+
#   scale_fill_brewer(palette="YlGnBu")+
#   scale_y_continuous(expand=c(0,0), limits=c(0,111))+
#   annotate("text",x=1.5,y=107,label="Pooled",size=6)+
#   annotate("text",x=4.5,y=107,label="Mosaic",size=6)+
#   annotate("text",x=7.5,y=107,label="Roadside",size=6)+
#   geom_text(aes(label=sig1, y=(occuperc+2)),colour="chocolate4",vjust=0, size=4.5, position=position_dodge(.8),fontface="plain")+
#   geom_text(aes(label=sig2, y=(occuperc+2)),colour="darkgoldenrod3",vjust=0, size=4.5, position=position_dodge(.8),fontface="plain")+
#   geom_text(aes(label=sig3, y=(occuperc+2)),colour="darkolivegreen4",vjust=0, size=4.5, position=position_dodge(.8),fontface="plain")+
#   geom_text(aes(label=sig4, y=(occuperc+2)),colour="darkgreen",vjust=0, size=4.5, position=position_dodge(.8),fontface="plain")+
#   geom_text(aes(label=sig5, y=(occuperc+2)),colour="deepskyblue3",vjust=0, size=4.5, position=position_dodge(.8),fontface="plain")+
#   geom_text(aes(label=sig6, y=(occuperc+2)),colour="steelblue",vjust=0, size=4.5, position=position_dodge(.8),fontface="plain")+
#   geom_text(aes(label=sig7, y=(occuperc+2)),colour="darkblue",vjust=0, size=4.5, position=position_dodge(.8),fontface="plain")+
#   geom_text(aes(label=sig8, y=(occuperc+2)),colour="darkslategray",vjust=0, size=4.5, position=position_dodge(.8),fontface="plain")+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"))+
#   theme(axis.text.x = element_text(size=12))+
#   theme(axis.text.x=element_text(colour="black"))+
#   theme(axis.text.y = element_text(size=14))+
#   theme(axis.text.y=element_text(colour="black"))+
#   scale_x_discrete(labels=label_wrap_gen(width=10))+
#   theme(plot.title=element_text(size=18))+
#   theme(axis.title = element_text(size = 16))+
#   ggtitle("A.")+
#   theme(legend.position = "bottom")+
#   guides(fill=guide_legend(title="Habitat Class"))+
#   theme(legend.text = element_text(size = 14))+
#   theme(legend.title = element_text(size = 14))+
#   theme(legend.box.background=element_rect(colour = "black"),
#         legend.background = element_blank())+
#   geom_vline(aes(xintercept=2.5),
#              linetype="dashed",colour="black",size=1)+
#   geom_vline(aes(xintercept=6.5),
#              linetype="dashed",colour="black",size=1)
# A

###
#graph
###
##non-zero occurrence graph

release<-c("Pooled Mosaic","Pooled Mosaic","Pooled Mosaic","Pooled Mosaic",
           "Pooled Roadside","Pooled Roadside","Pooled Roadside","Pooled Roadside",
           "MZD","MZD","MZD","MZD",
           "MHE","MHE","MHE","MHE",
           "MHD","MHD","MHD","MHD",
           "MLD","MLD","MLD","MLD",
           "RHD","RHD","RHD","RHD",
           "RLD","RLD","RLD","RLD")
habitat<-c("ZD","HE","LD","HD",
           "ZD","HE","LD","HD",
           "ZD","HE","LD","HD",
           "ZD","HE","LD","HD",
           "ZD","HE","LD","HD",
           "ZD","HE","LD","HD",
           "ZD","HE","LD","HD",
           "ZD","HE","LD","HD")
occuperc<-c(8.495680,5.592847,16.502937,69.408537,
            65.630522,15.322870,17.930082,1.116526,
            9.356443,6.997109,12.975891,70.670557,
            16.558230,5.276981,7.082300,71.082489,
            0.09188638,5.83123825,19.12573719,74.95113818,
            0,5.338305,84.125045,10.536650,
            34.304799,23.363683,36.887739,5.443779,
            71.2074654,13.8913576,14.5550354,0.3461415)

byrelease<-data.frame(release,habitat,occuperc)
names(byrelease)

byrelease$release<-factor(byrelease$release, levels=c("Pooled Mosaic","MHD","MLD","MZD","MHE","Pooled Roadside","RHD","RLD"))
byrelease$habitat<-factor(byrelease$habitat, levels=c("HD","LD","ZD","HE"))

sig1<-c("a","a","a","b","","","","","","","","","","","","","","","","","","","","","","","","","","","","")
sig2<-c("","","","","a","b","b","c","","","","","","","","","","","","","","","","","","","","","","","","")
sig3<-c("","","","","","","","","a","a","a","b","","","","","","","","","","","","","","","","","","","","")
sig4<-c("","","","","","","","","","","","","a","a","a","b","","","","","","","","","","","","","","","","")
sig5<-c("","","","","","","","","","","","","","","","","a","ab","b","c","","","","","","","","","","","","")
sig6<-c("","","","","","","","","","","","","","","","","","","","","a","ab","c","b","","","","","","","","")
sig7<-c("","","","","","","","","","","","","","","","","","","","","","","","","a","a","a","b","","","","")
sig8<-c("","","","","","","","","","","","","","","","","","","","","","","","","","","","","a","b","b","c")

#for manuscript
A<-ggplot(byrelease, aes(fill=habitat, y=occuperc, x=release, width=.8))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Release Habitat Class") +
  ylab("% Raster Cells with \nNon-Zero Occurrence Probability")+
  theme_bw()+
  scale_fill_brewer(palette="YlGnBu")+
  scale_y_continuous(expand=c(0,0), limits=c(0,119))+
  #annotate("text",x=1.5,y=107,label="Sum",size=6)+
  annotate("text",x=3,y=115,label="Mosaic Site",size=6)+
  annotate("text",x=7,y=115,label="Roadside Sites",size=6)+
  geom_text(aes(label=sig1, y=(occuperc+2)),colour="chocolate4",vjust=0, size=4.5, position=position_dodge(.8),fontface="plain")+
  geom_text(aes(label=sig2, y=(occuperc+2)),colour="darkgoldenrod3",vjust=0, size=4.5, position=position_dodge(.8),fontface="plain")+
  geom_text(aes(label=sig3, y=(occuperc+2)),colour="darkolivegreen4",vjust=0, size=4.5, position=position_dodge(.8),fontface="plain")+
  geom_text(aes(label=sig4, y=(occuperc+2)),colour="darkgreen",vjust=0, size=4.5, position=position_dodge(.8),fontface="plain")+
  geom_text(aes(label=sig5, y=(occuperc+2)),colour="deepskyblue3",vjust=0, size=4.5, position=position_dodge(.8),fontface="plain")+
  geom_text(aes(label=sig6, y=(occuperc+2)),colour="steelblue",vjust=0, size=4.5, position=position_dodge(.8),fontface="plain")+
  geom_text(aes(label=sig7, y=(occuperc+2)),colour="darkblue",vjust=0, size=4.5, position=position_dodge(.8),fontface="plain")+
  geom_text(aes(label=sig8, y=(occuperc+2)),colour="darkslategray",vjust=0, size=4.5, position=position_dodge(.8),fontface="plain")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=14))+
  theme(axis.text.y=element_text(colour="black"))+
  scale_x_discrete(labels=label_wrap_gen(width=10))+
  theme(plot.title=element_text(size=18))+
  theme(axis.title = element_text(size = 16))+
  ggtitle("A.")+
  theme(legend.position = "bottom")+
  guides(fill=guide_legend(title="Habitat Class"))+
  theme(legend.text = element_text(size = 14))+
  theme(legend.title = element_text(size = 14))+
  theme(legend.box.background=element_rect(colour = "black"),
        legend.background = element_blank())+
  geom_vline(aes(xintercept=5.5),
             linetype="dashed",colour="black",size=1)
  #geom_vline(aes(xintercept=6.5),
   #          linetype="dashed",colour="black",size=1)
A


###roadside comparisons
#rw chisq.test(c(34.304799,23.363683,36.887739,5.443779))
#rwo chisq.test(c(71.2074654,13.8913576,14.5550354,0.3461415))

chisq.test(c(34.304799,71.2074654))
chisq.test(c(23.363683,13.8913576))
chisq.test(c(36.887739,14.5550354))
chisq.test(c(5.443779,0.3461415))


# #############################################
# #HIGH Occurrence total, then by release; sum of all cells for all individuals
# 
# occu<-read.csv("1920_Occurrence_All.csv",header=TRUE)
# names(occu)
# unique(occu$spatialconfig)
# unique(occu$release)
# unique(occu$occu)
# 
# occumosaic<-subset(occu, spatialconfig == "mosaic"&occu=="high")
# occuwith<-subset(occu, spatialconfig == "roadsidewith"&occu=="high")
# occuwithout<-subset(occu, spatialconfig == "roadsidewithout"&occu=="high")
# occuroad<-rbind(occuwith,occuwithout)
# mcrop<-subset(occu,release=="crop"&occu=="high")
# medge<-subset(occu,release=="edge"&occu=="high")
# mpoor<-subset(occu,release=="poor"&occu=="high")
# mrich<-subset(occu,release=="rich"&occu=="high")
# rw<-subset(occu,release=="poor-with"&occu=="high")
# rwo<-subset(occu,release=="poor-without"&occu=="high")
# 
# #mosaic all
# occumosaichab<-aggregate(count~class,occumosaic,sum)
# occumosaichab
# sum(occumosaichab$count)
# occumosaichab$totaloccu<-c(962,962,962,962)
# occumosaichab$occuprop<-(occumosaichab$count/occumosaichab$totaloccu)
# occumosaichab$occuprop100<-(occumosaichab$occuprop*100)
# occumosaichab
# 
# chisq.test(c(2.079002,3.846154,59.043659,35.031185))
# chisq.test(c(2.079002,3.846154))
# chisq.test(c(2.079002,59.043659))
# chisq.test(c(2.079002,35.031185))
# chisq.test(c(3.846154,59.043659))
# chisq.test(c(3.846154,35.031185))
# chisq.test(c(59.043659,35.031185))
# 
# 
# #roadside all
# occuroadhab<-aggregate(count~class,occuroad,sum)
# occuroadhab
# sum(occuroadhab$count)
# occuroadhab$totaloccu<-c(110,110,110,110)
# occuroadhab$occuprop<-(occuroadhab$count/occuroadhab$totaloccu)
# occuroadhab$occuprop100<-(occuroadhab$occuprop*100)
# occuroadhab
# 
# chisq.test(c(25.454545,8.181818,46.363636,20.000000))
# chisq.test(c(25.454545,8.181818))
# chisq.test(c(25.454545,46.363636))
# chisq.test(c(25.454545,20.000000))
# chisq.test(c(8.181818,46.363636))
# chisq.test(c(8.181818,20.000000))
# chisq.test(c(46.363636,20.000000))
# 
# 
# #mcrop
# mcrophab<-aggregate(count~class,mcrop,sum)
# mcrophab
# sum(mcrophab$count)
# mcrophab$totaloccu<-c(83,83,83)
# mcrophab$occuprop<-(mcrophab$count/mcrophab$totaloccu)
# mcrophab$occuprop100<-(mcrophab$occuprop*100)
# mcrophab
# 
# chisq.test(c(24.09639,24.09639,0,51.80723))
# chisq.test(c(24.09639,24.09639))
# chisq.test(c(24.09639,0))
# chisq.test(c(24.09639,51.80723))
# chisq.test(c(24.09639,0))
# chisq.test(c(24.09639,51.80723))
# chisq.test(c(0,51.80723))
# 
# 
# 
# #medge
# medgehab<-aggregate(count~class,medge,sum)
# medgehab
# sum(medgehab$count)
# medgehab$totaloccu<-c(20,20,20)
# medgehab$occuprop<-(medgehab$count/medgehab$totaloccu)
# medgehab$occuprop100<-(medgehab$occuprop*100)
# medgehab
# 
# chisq.test(c(0,80,5,15))
# chisq.test(c(0,80))
# chisq.test(c(0,5))
# chisq.test(c(0,15))
# chisq.test(c(80,5))
# chisq.test(c(80,15))
# chisq.test(c(5,15))
# 
# 
# 
# #mrich
# mrichhab<-aggregate(count~class,mrich,sum)
# mrichhab
# sum(mrichhab$count)
# mrichhab$totaloccu<-c(274,274)
# mrichhab$occuprop<-(mrichhab$count/mrichhab$totaloccu)
# mrichhab$occuprop100<-(mrichhab$occuprop*100)
# mrichhab
# 
# chisq.test(c(0,0,1.824818,98.175182))
# chisq.test(c(0,0))
# chisq.test(c(0,1.824818))
# chisq.test(c(0,98.175182))
# chisq.test(c(0,1.824818))
# chisq.test(c(0,98.175182))
# chisq.test(c(1.824818,98.175182))
# 
# 
# #mpoor
# mpoorhab<-aggregate(count~class,mpoor,sum)
# mpoorhab
# sum(mpoorhab$count)
# mpoorhab$totaloccu<-c(585,585,585)
# mpoorhab$occuprop<-(mpoorhab$count/mpoorhab$totaloccu)
# mpoorhab$occuprop100<-(mpoorhab$occuprop*100)
# mpoorhab
# 
# chisq.test(c(0,0.1709402,96.0683761,3.7606838))
# chisq.test(c(0,0.1709402))
# chisq.test(c(0,96.0683761))
# chisq.test(c(0,3.7606838))
# chisq.test(c(0.1709402,96.0683761))
# chisq.test(c(0.1709402,3.7606838))
# chisq.test(c(96.0683761,3.7606838))
# 
# 
# 
# #rw
# rwhab<-aggregate(count~class,rw,sum)
# rwhab
# sum(rwhab$count)
# rwhab$totaloccu<-c(66,66,66,66)
# rwhab$occuprop<-(rwhab$count/rwhab$totaloccu)
# rwhab$occuprop100<-(rwhab$occuprop*100)
# rwhab
# 
# chisq.test(c(4.545455,13.636364,48.484848,33.333333))
# chisq.test(c(4.545455,13.636364))
# chisq.test(c(4.545455,48.484848))
# chisq.test(c(4.545455,33.333333))
# chisq.test(c(13.636364,48.484848))
# chisq.test(c(13.636364,33.333333))
# chisq.test(c(48.484848,33.333333))
# 
# 
# 
# #rwo
# rwohab<-aggregate(count~class,rwo,sum)
# rwohab
# sum(rwohab$count)
# rwohab$totaloccu<-c(44,44)
# rwohab$occuprop<-(rwohab$count/rwohab$totaloccu)
# rwohab$occuprop100<-(rwohab$occuprop*100)
# rwohab
# 
# chisq.test(c(56.81818,0,43.18182,0))
# chisq.test(c(56.81818,0))
# chisq.test(c(56.81818,43.18182))
# chisq.test(c(56.81818,0))
# chisq.test(c(0,43.18182))
# chisq.test(c(0,0))
# chisq.test(c(43.18182,0))
# 
# 
# ###
# #graph
# ###
# ##non-zero occurrence graph
# 
# release<-c("Mosaic","Mosaic","Mosaic","Mosaic",
#            "Roadside","Roadside","Roadside","Roadside",
#            "Crop","Crop","Crop","Crop",
#            "Edge","Edge","Edge","Edge",
#            "Resource- Rich","Resource- Rich","Resource- Rich","Resource- Rich",
#            "Resource- Poor","Resource- Poor","Resource- Poor","Resource- Poor",
#            "With Resource- Rich","With Resource- Rich","With Resource- Rich","With Resource- Rich",
#            "Without Resource- Rich","Without Resource- Rich","Without Resource- Rich","Without Resource- Rich")
# habitat<-c("Crop","Edge","Resource-Poor","Resource-Rich",
#            "Crop","Edge","Resource-Poor","Resource-Rich",
#            "Crop","Edge","Resource-Poor","Resource-Rich",
#            "Crop","Edge","Resource-Poor","Resource-Rich",
#            "Crop","Edge","Resource-Poor","Resource-Rich",
#            "Crop","Edge","Resource-Poor","Resource-Rich",
#            "Crop","Edge","Resource-Poor","Resource-Rich",
#            "Crop","Edge","Resource-Poor","Resource-Rich")
# occuperc<-c(2.079002,3.846154,59.043659,35.031185,
#             25.454545,8.181818,46.363636,20.000000,
#             24.09639,24.09639,0,51.80723,
#             0,80,5,15,
#             0,0,1.824818,98.175182,
#             0,0.1709402,96.0683761,3.7606838,
#             4.545455,13.636364,48.484848,33.333333,
#             56.81818,0,43.18182,0)
# 
# byrelease<-data.frame(release,habitat,occuperc)
# names(byrelease)
# 
# byrelease$release<-factor(byrelease$release, levels=c("Mosaic","Roadside","Crop","Edge","Resource- Rich","Resource- Poor","With Resource- Rich","Without Resource- Rich"))
# 
# sig1<-c("a","a","b","b","","","","","","","","","","","","","","","","","","","","","","","","","","","","")
# sig2<-c("","","","","a","b","a","a","","","","","","","","","","","","","","","","","","","","","","","","")
# sig3<-c("","","","","","","","","a","a","b","c","","","","","","","","","","","","","","","","","","","","")
# sig4<-c("","","","","","","","","","","","","a","b","ac","c","","","","","","","","","","","","","","","","")
# sig5<-c("","","","","","","","","","","","","","","","","a","a","a","b","","","","","","","","","","","","")
# sig6<-c("","","","","","","","","","","","","","","","","","","","","a","a","b","a","","","","","","","","")
# sig7<-c("","","","","","","","","","","","","","","","","","","","","","","","","a","a","b","b","","","","")
# sig8<-c("","","","","","","","","","","","","","","","","","","","","","","","","","","","","a","b","a","b")
# 
# #for manuscript
# B<-ggplot(byrelease, aes(fill=habitat, y=occuperc, x=release, width=.8))+
#   geom_bar(stat="identity", color="black",
#            position=position_dodge())+
#   xlab("Release") +
#   ylab("Percent of Cells with \nHigh Occurrence Probability (>0.67)")+
#   theme_bw()+
#   scale_fill_brewer(palette="YlGnBu")+
#   scale_y_continuous(expand=c(0,0), limits=c(0,111))+
#   annotate("text",x=1.5,y=107,label="Sum",size=6)+
#   annotate("text",x=4.5,y=107,label="Mosaic",size=6)+
#   annotate("text",x=7.5,y=107,label="Roadside",size=6)+
#   geom_text(aes(label=sig1, y=(occuperc+2)),colour="chocolate4",vjust=0, size=4.5, position=position_dodge(.8),fontface="plain")+
#   geom_text(aes(label=sig2, y=(occuperc+2)),colour="darkgoldenrod3",vjust=0, size=4.5, position=position_dodge(.8),fontface="plain")+
#   geom_text(aes(label=sig3, y=(occuperc+2)),colour="darkolivegreen4",vjust=0, size=4.5, position=position_dodge(.8),fontface="plain")+
#   geom_text(aes(label=sig4, y=(occuperc+2)),colour="darkgreen",vjust=0, size=4.5, position=position_dodge(.8),fontface="plain")+
#   geom_text(aes(label=sig5, y=(occuperc+2)),colour="deepskyblue3",vjust=0, size=4.5, position=position_dodge(.8),fontface="plain")+
#   geom_text(aes(label=sig6, y=(occuperc+2)),colour="steelblue",vjust=0, size=4.5, position=position_dodge(.8),fontface="plain")+
#   geom_text(aes(label=sig7, y=(occuperc+2)),colour="darkblue",vjust=0, size=4.5, position=position_dodge(.8),fontface="plain")+
#   geom_text(aes(label=sig8, y=(occuperc+2)),colour="darkslategray",vjust=0, size=4.5, position=position_dodge(.8),fontface="plain")+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"))+
#   theme(axis.text.x = element_text(size=12))+
#   theme(axis.text.x=element_text(colour="black"))+
#   theme(axis.text.y = element_text(size=14))+
#   theme(axis.text.y=element_text(colour="black"))+
#   scale_x_discrete(labels=label_wrap_gen(width=10))+
#   theme(plot.title=element_text(size=18))+
#   theme(axis.title = element_text(size = 16))+
#   ggtitle("B.")+
#   theme(legend.position = "bottom")+
#   guides(fill=guide_legend(title="Habitat Class"))+
#   theme(legend.text = element_text(size = 14))+
#   theme(legend.title = element_text(size = 14))+
#   theme(legend.box.background=element_rect(colour = "black"),
#         legend.background = element_blank())+
#   geom_vline(aes(xintercept=2.5),
#              linetype="dashed",colour="black",size=1)+
#   geom_vline(aes(xintercept=6.5),
#              linetype="dashed",colour="black",size=1)
# 
# 
# grid.arrange(A,B)


#########################################################################################
###################################################################################################
##
#average by individual
##
occu<-read.csv("1920_Occurrence_All.csv",header=TRUE)
names(occu)
unique(occu$spatialconfig)
unique(occu$release)

occumosaic<-subset(occu, spatialconfig == "mosaic")
occuwith<-subset(occu, spatialconfig == "roadsidewith")
occuwithout<-subset(occu, spatialconfig == "roadsidewithout")
occuroad<-rbind(occuwith,occuwithout)
mcrop<-subset(occu,release=="crop")
medge<-subset(occu,release=="edge")
mpoor<-subset(occu,release=="poor")
mrich<-subset(occu,release=="rich")
rw<-subset(occu,release=="poor-with")
rwo<-subset(occu,release=="poor-without")

#ALL mosaic
#allcells<-aggregate(count~ctmmlabel+class+release,occumosaic,sum)
#write.csv(allcells,"allcells.csv")
allcells<-read.csv("allcells.csv",header=TRUE)
totalcells<-aggregate(count~ctmmlabel,occumosaic,sum)
totalcells
names(totalcells)[2]<-"totalcount"
allcells<-merge(allcells,totalcells,by="ctmmlabel")
allcells$prop<-(allcells$count/allcells$totalcount)
head(allcells)
library(car)
allcells$logit2<-logit(allcells$prop)  
alllog<-glm(logit2~class,data=allcells,family = gaussian(link = "identity"))  
emm2<-emmeans(alllog,c("class"),type="response")
joint_tests(emm2)
pairs(emm2)
emm2
CLD(emm2)

SEmosaic<-summarySE(allcells, measurevar="prop", groupvars=c("class"))
SEmosaic$group<-"allmosaic"
#SEmosaic$sig<-c("ab","a","bc","c")
SEmosaic
SEmosaic$sdb<-c(0.1068861  ,0.1442926  ,0.26  ,0.3385864  )


#mcrop
mcrop<-subset(allcells,release=="crop")
# mcropcells<-aggregate(count~ctmmlabel+class,mcrop,sum)
# mcroptotalcells<-aggregate(count~ctmmlabel,mcrop,sum)
# mcroptotalcells
# names(mcroptotalcells)[2]<-"totalcount"
# mcropcells<-merge(mcropcells,mcroptotalcells,by="ctmmlabel")
# mcropcells$prop<-(mcropcells$count/mcropcells$totalcount)
# head(mcropcells)
# mcropcells$logit2<-logit(mcropcells$prop)  
mcroplog<-glm(logit2~class,data=mcrop,family = gaussian(link = "identity"))  
emm2<-emmeans(mcroplog,c("class"),type="response")
joint_tests(emm2)
pairs(emm2)
emm2
CLD(emm2)
#pwpp(emm)

SEmcrop<-summarySE(mcrop, measurevar="prop", groupvars=c("class"))
SEmcrop
SEmcrop$group<-"mcrop"
#SEmcrop$sig<-c("a","a","a","a")
SEmcrop
SEmcrop$sdb<-c(0.3671260  ,0.1309641  ,0.1369914  ,0.3304791  )


#medge
medge<-subset(allcells,release=="edge")
# medgecells<-aggregate(count~ctmmlabel+class+release,medge,sum)
# medgetotalcells<-aggregate(count~ctmmlabel,medge,sum)
# medgetotalcells
# names(medgetotalcells)[2]<-"totalcount"
# medgecells<-merge(medgecells,medgetotalcells,by="ctmmlabel")
# medgecells$prop<-(medgecells$count/medgecells$totalcount)
# head(medgecells)
# medgecells$logit2<-logit(medgecells$prop)  
medgelog<-glm(logit2~class,data=medge,family = gaussian(link = "identity"))  
emm2<-emmeans(medgelog,c("class"),type="response")
joint_tests(emm2)
pairs(emm2)
emm2
CLD(emm2)
#pwpp(emm)

SEmedge<-summarySE(medge, measurevar="prop", groupvars=c("class"))
SEmedge
SEmedge$group<-"medge"
#SEmedge$sig<-c("a","a","a","a")
SEmedge
SEmedge$sdb<-c(0.03156425  ,0.27184392  ,0.06  ,0.23217674  )

#mrich
mrich<-subset(allcells,release=="rich")
# mrichcells<-aggregate(count~ctmmlabel+class+release,mrich,sum)
# mrichtotalcells<-aggregate(count~ctmmlabel,mrich,sum)
# mrichtotalcells
# names(mrichtotalcells)[2]<-"totalcount"
# mrichcells<-merge(mrichcells,mrichtotalcells,by="ctmmlabel")
# mrichcells$prop<-(mrichcells$count/mrichcells$totalcount)
# head(mrichcells)
# mrichcells$logit2<-logit(mrichcells$prop)  
mrichlog<-glm(logit2~class,data=mrich,family = gaussian(link = "identity"))  
emm2<-emmeans(mrichlog,c("class"),type="response")
joint_tests(emm2)
pairs(emm2)
emm2
CLD(emm2)
#pwpp(emm)

SEmrich<-summarySE(mrich, measurevar="prop", groupvars=c("class"))
SEmrich
SEmrich$group<-"mrich"
SEmrich
SEmrich$sdb<-c(0.03004424  ,0.1077756  ,0.07  ,0.1694054  )


#mpoor
mpoor<-subset(allcells,release=="poor")
# mpoorcells<-aggregate(count~ctmmlabel+class+release,mpoor,sum)
# mpoortotalcells<-aggregate(count~ctmmlabel,mpoor,sum)
# mpoortotalcells
# names(mpoortotalcells)[2]<-"totalcount"
# mpoorcells<-merge(mpoorcells,mpoortotalcells,by="ctmmlabel")
# mpoorcells$prop<-(mpoorcells$count/mpoorcells$totalcount)
# head(mpoorcells)
# mpoorcells$logit2<-logit(mpoorcells$prop)  
mpoorlog<-glm(logit2~class,data=mpoor,family = gaussian(link = "identity"))  
emm2<-emmeans(mpoorlog,c("class"),type="response")
joint_tests(emm2)
pairs(emm2)
emm2
CLD(emm2)
#pwpp(emm)

SEmpoor<-summarySE(mpoor, measurevar="prop", groupvars=c("class"))
SEmpoor$group<-"mpoor"
SEmpoor
SEmpoor$sdb<-c(0,0.06534229     ,0.3967517     ,0.29713137   )


#########

#ALL roadside
#allcells2<-aggregate(count~ctmmlabel+class+release,occuroad,sum)
#write.csv(allcells2,"allcells2.csv")
allcells2<-read.csv("allcells2.csv",header=TRUE)
allcells2<-aggregate(count~ctmmlabel+class+release,occuroad,sum)
totalcells<-aggregate(count~ctmmlabel,occuroad,sum)
totalcells
names(totalcells)[2]<-"totalcount"
allcells2<-merge(allcells2,totalcells,by="ctmmlabel")
allcells2$prop<-(allcells2$count/allcells2$totalcount)
head(allcells2)
library(car)
allcells2$logit2<-logit(allcells2$prop)  
alllog<-glm(logit2~class,data=allcells2,family = gaussian(link = "identity"))  
emm2<-emmeans(alllog,c("class"),type="response")
joint_tests(emm2)
pairs(emm2)
emm2
CLD(emm2)

SEroad<-summarySE(allcells2, measurevar="prop", groupvars=c("class"))
SEroad$group<-"allroad"
#SEroad$sig<-c("a","a","a","a")
SEroad
SEroad$sdb<-c(0.3091704  ,0.2681168  ,0.3834904 ,0.2002983 )


#rw
rwcells<-subset(allcells2,release=="poor-with")
# rwcells<-aggregate(count~ctmmlabel+class+release,rw,sum)
# rwtotalcells<-aggregate(count~ctmmlabel,rw,sum)
# rwtotalcells
# names(rwtotalcells)[2]<-"totalcount"
# rwcells<-merge(rwcells,rwtotalcells,by="ctmmlabel")
# rwcells$prop<-(rwcells$count/rwcells$totalcount)
# head(rwcells)
# rwcells$logit2<-logit(rwcells$prop)  
rwlog<-glm(logit2~class,data=rwcells,family = gaussian(link = "identity"))  
emm2<-emmeans(rwlog,c("class"),type="response")
joint_tests(emm2)
pairs(emm2)
emm2
CLD(emm2)
#pwpp(emm)

SErw<-summarySE(rwcells, measurevar="prop", groupvars=c("class"))
SErw
SErw$group<-"rw"
SErw
SErw$sdb<-c(0.2940113 ,0.2584485 ,0.3368794 ,0.2028434 )

#rwo
rwocells<-subset(allcells2,release=="poor-without")
# rwocells<-aggregate(count~ctmmlabel+class+release,rwo,sum)
# rwototalcells<-aggregate(count~ctmmlabel,rwo,sum)
# rwototalcells
# names(rwototalcells)[2]<-"totalcount"
# rwocells<-merge(rwocells,rwototalcells,by="ctmmlabel")
# rwocells$prop<-(rwocells$count/rwocells$totalcount)
# head(rwocells)
# rwocells$logit2<-logit(rwocells$prop)  
rwolog<-glm(logit2~class,data=rwocells,family = gaussian(link = "identity"))  
emm2<-emmeans(rwolog,c("class"),type="response")
joint_tests(emm2)
pairs(emm2)
emm2
CLD(emm2)
#pwpp(emm)

SErwo<-summarySE(rwocells, measurevar="prop", groupvars=c("class"))
SErwo
SErwo$group<-"rwo"
SErwo[is.na(SErwo)]<-0
SErwo
SErwo$sdb<-c(0.3267688 ,0.25 ,0.4324514 ,0)


SE<-rbind(SEmosaic,SEroad,SEmcrop,SEmedge,SEmrich,SEmpoor,SErw,SErwo)
SE$prop100<-(SE$prop*100)
SE$sd100<-(SE$sd*100)
SE$sdb100<-(SE$sdb*100)
SE


SE$release<-c("Pooled Mosaic","Pooled Mosaic","Pooled Mosaic","Pooled Mosaic",
           "Pooled Roadside","Pooled Roadside","Pooled Roadside","Pooled Roadside",
           "MZD","MZD","MZD","MZD",
           "MHE","MHE","MHE","MHE",
           "MHD","MHD","MHD","MHD",
           "MLD","MLD","MLD","MLD",
           "RHD","RHD","RHD","RHD",
           "RLD","RLD","RLD","RLD")
SE$habitat<-c("ZD","HE","LD","HD",
              "ZD","HE","LD","HD",
              "ZD","HE","LD","HD",
              "ZD","HE","LD","HD",
              "ZD","HE","LD","HD",
              "ZD","HE","LD","HD",
              "ZD","HE","LD","HD",
              "ZD","HE","LD","HD")

SE$release<-factor(SE$release, levels=c("Pooled Mosaic","MHD","MLD","MZD","MHE","Pooled Roadside","RHD","RLD"))
SE$habitat<-factor(SE$habitat, levels=c("HD","LD","ZD","HE"))

sig1b<-c("a","ab","b","c","","","","","","","","","","","","","","","","","","","","","","","","","","","","")
sig2b<-c("","","","","a","a","a","a","","","","","","","","","","","","","","","","","","","","","","","","")
sig3b<-c("","","","","","","","","a","ab","b","ab","","","","","","","","","","","","","","","","","","","","")
sig4b<-c("","","","","","","","","","","","","a","b","a","b","","","","","","","","","","","","","","","","")
sig5b<-c("","","","","","","","","","","","","","","","","a","b","ab","c","","","","","","","","","","","","")
sig6b<-c("","","","","","","","","","","","","","","","","","","","","a","ab","c","b","","","","","","","","")
sig7b<-c("","","","","","","","","","","","","","","","","","","","","","","","","a","a","a","a","","","","")
sig8b<-c("","","","","","","","","","","","","","","","","","","","","","","","","","","","","a","a","a","a")

SE

B<-ggplot(SE, aes(fill=habitat, y=prop100, x=release, width=.8))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Release Habitat Class") +
  ylab("Average % Raster Cells with \nNon-Zero Occurrence Probability")+
  theme_bw()+
  scale_fill_brewer(palette="YlGnBu")+
  scale_y_continuous(expand=c(0,0), limits=c(0,119))+
  #annotate("text",x=1.5,y=107,label="Sum",size=6)+
  annotate("text",x=3,y=115,label="Mosaic Site",size=6)+
  annotate("text",x=7,y=115,label="Roadside Sites",size=6)+
  geom_errorbar(aes(ymin=prop100-sdb100, ymax=prop100+sd100), width=.2, 
                position=position_dodge(.8))+
  geom_text(aes(label=sig1b, y=(prop100+sd100)),colour="chocolate4",vjust=-0.5, size=4.5, position=position_dodge(.8),fontface="plain")+
  geom_text(aes(label=sig1b, y=(prop100+sd100)),colour="chocolate4",vjust=-0.5, size=4.5, position=position_dodge(.8),fontface="plain")+
  geom_text(aes(label=sig2b, y=(prop100+sd100)),colour="darkgoldenrod3",vjust=-0.5, size=4.5, position=position_dodge(.8),fontface="plain")+
  geom_text(aes(label=sig3b, y=(prop100+sd100)),colour="darkolivegreen4",vjust=-0.5, size=4.5, position=position_dodge(.8),fontface="plain")+
  geom_text(aes(label=sig4b, y=(prop100+sd100)),colour="darkgreen",vjust=-0.5, size=4.5, position=position_dodge(.8),fontface="plain")+
  geom_text(aes(label=sig5b, y=(prop100+sd100)),colour="deepskyblue3",vjust=-0.5, size=4.5, position=position_dodge(.8),fontface="plain")+
  geom_text(aes(label=sig6b, y=(prop100+sd100)),colour="steelblue",vjust=-0.5, size=4.5, position=position_dodge(.8),fontface="plain")+
  geom_text(aes(label=sig7b, y=(prop100+sd100)),colour="darkblue",vjust=-0.5, size=4.5, position=position_dodge(.8),fontface="plain")+
  geom_text(aes(label=sig8b, y=(prop100+sd100)),colour="darkslategray",vjust=-0.5, size=4.5, position=position_dodge(.8),fontface="plain")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=12))+
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
        legend.background = element_blank())+
  geom_vline(aes(xintercept=5.5),
             linetype="dashed",colour="black",size=1)
  #geom_vline(aes(xintercept=6.5),
   #          linetype="dashed",colour="black",size=1)

grid.arrange(A,B)

B


# ###################################################################################################
# ##
# #HIGH average by individual
# ##
# occu<-read.csv("1920_Occurrence_All.csv",header=TRUE)
# names(occu)
# unique(occu$spatialconfig)
# unique(occu$release)
# 
# occumosaic<-subset(occu, spatialconfig == "mosaic"&occu=="high")
# occuwith<-subset(occu, spatialconfig == "roadsidewith"&occu=="high")
# occuwithout<-subset(occu, spatialconfig == "roadsidewithout"&occu=="high")
# occuroad<-rbind(occuwith,occuwithout)
# mcrop<-subset(occu,release=="crop"&occu=="high")
# medge<-subset(occu,release=="edge"&occu=="high")
# mpoor<-subset(occu,release=="poor"&occu=="high")
# mrich<-subset(occu,release=="rich"&occu=="high")
# rw<-subset(occu,release=="poor-with"&occu=="high")
# rwo<-subset(occu,release=="poor-without"&occu=="high")
# 
# #ALL mosaic
# #allcells3<-aggregate(count~ctmmlabel+class+release,occumosaic,sum)
# #write.csv(allcells3,"allcells3.csv")
# unique(allcells3$ctmmlabel)
# allcells3<-read.csv("allcells3.csv",header=TRUE)
# totalcells<-aggregate(count~ctmmlabel,occumosaic,sum)
# totalcells
# names(totalcells)[2]<-"totalcount"
# allcells3<-merge(allcells3,totalcells,by="ctmmlabel")
# allcells3$prop<-(allcells3$count/allcells3$totalcount)
# head(allcells3)
# library(car)
# allcells3$logit2<-logit(allcells3$prop)  
# alllog<-glm(logit2~class,data=allcells3,family = gaussian(link = "identity"))  
# emm2<-emmeans(alllog,c("class"),type="response")
# joint_tests(emm2)
# pairs(emm2)
# emm2
# CLD(emm2)
# 
# SEmosaic<-summarySE(allcells3, measurevar="prop", groupvars=c("class"))
# SEmosaic$group<-"allmosaic"
# #SEmosaic$sig<-c("ab","a","bc","c")
# SEmosaic
# SEmosaic$sdb<-c(0.067 ,0.17753623 ,0.247 ,0.4583608 )
# 
# 
# #mcrop
# mcrop<-subset(allcells3,release=="crop")
# # mcropcells<-aggregate(count~ctmmlabel+class,mcrop,sum)
# # mcroptotalcells<-aggregate(count~ctmmlabel,mcrop,sum)
# # mcroptotalcells
# # names(mcroptotalcells)[2]<-"totalcount"
# # mcropcells<-merge(mcropcells,mcroptotalcells,by="ctmmlabel")
# # mcropcells$prop<-(mcropcells$count/mcropcells$totalcount)
# # head(mcropcells)
# # mcropcells$logit2<-logit(mcropcells$prop)  
# mcroplog<-glm(logit2~class,data=mcrop,family = gaussian(link = "identity"))  
# emm2<-emmeans(mcroplog,c("class"),type="response")
# joint_tests(emm2)
# pairs(emm2)
# emm2
# CLD(emm2)
# #pwpp(emm)
# 
# SEmcrop<-summarySE(mcrop, measurevar="prop", groupvars=c("class"))
# SEmcrop
# SEmcrop$group<-"mcrop"
# #SEmcrop$sig<-c("a","a","a","a")
# SEmcrop
# SEmcrop$sdb<-c(0.4075776   ,0.1485507   ,0  ,0.3475981   )
# 
# 
# #medge
# medge<-subset(allcells3,release=="edge")
# # medgecells<-aggregate(count~ctmmlabel+class+release,medge,sum)
# # medgetotalcells<-aggregate(count~ctmmlabel,medge,sum)
# # medgetotalcells
# # names(medgetotalcells)[2]<-"totalcount"
# # medgecells<-merge(medgecells,medgetotalcells,by="ctmmlabel")
# # medgecells$prop<-(medgecells$count/medgecells$totalcount)
# # head(medgecells)
# # medgecells$logit2<-logit(medgecells$prop)  
# medgelog<-glm(logit2~class,data=medge,family = gaussian(link = "identity"))  
# emm2<-emmeans(medgelog,c("class"),type="response")
# joint_tests(emm2)
# pairs(emm2)
# emm2
# CLD(emm2)
# #pwpp(emm)
# 
# SEmedge<-summarySE(medge, measurevar="prop", groupvars=c("class"))
# SEmedge
# SEmedge$group<-"medge"
# #SEmedge$sig<-c("a","a","a","a")
# SEmedge
# SEmedge$sdb<-c(0  ,0.4784717   ,0.16   ,0.2361111   )
# SEmedge$sd<-c(0,0.30,0.4082483 ,0.3958699 )
# 
# #mrich
# mrich<-subset(allcells3,release=="rich")
# # mrichcells<-aggregate(count~ctmmlabel+class+release,mrich,sum)
# # mrichtotalcells<-aggregate(count~ctmmlabel,mrich,sum)
# # mrichtotalcells
# # names(mrichtotalcells)[2]<-"totalcount"
# # mrichcells<-merge(mrichcells,mrichtotalcells,by="ctmmlabel")
# # mrichcells$prop<-(mrichcells$count/mrichcells$totalcount)
# # head(mrichcells)
# # mrichcells$logit2<-logit(mrichcells$prop)  
# mrichlog<-glm(logit2~class,data=mrich,family = gaussian(link = "identity"))  
# emm2<-emmeans(mrichlog,c("class"),type="response")
# joint_tests(emm2)
# pairs(emm2)
# emm2
# CLD(emm2)
# #pwpp(emm)
# 
# SEmrich<-summarySE(mrich, measurevar="prop", groupvars=c("class"))
# SEmrich
# SEmrich$group<-"mrich"
# SEmrich
# SEmrich$sdb<-c(0  ,0  ,0.06  ,0.1851852   )
# SEmrich$sd<-c(0,0,0.18,0.07)
# 
# #mpoor
# mpoor<-subset(allcells3,release=="poor")
# # mpoorcells<-aggregate(count~ctmmlabel+class+release,mpoor,sum)
# # mpoortotalcells<-aggregate(count~ctmmlabel,mpoor,sum)
# # mpoortotalcells
# # names(mpoortotalcells)[2]<-"totalcount"
# # mpoorcells<-merge(mpoorcells,mpoortotalcells,by="ctmmlabel")
# # mpoorcells$prop<-(mpoorcells$count/mpoorcells$totalcount)
# # head(mpoorcells)
# # mpoorcells$logit2<-logit(mpoorcells$prop)  
# mpoorlog<-glm(logit2~class,data=mpoor,family = gaussian(link = "identity"))  
# emm2<-emmeans(mpoorlog,c("class"),type="response")
# joint_tests(emm2)
# pairs(emm2)
# emm2
# CLD(emm2)
# #pwpp(emm)
# 
# SEmpoor<-summarySE(mpoor, measurevar="prop", groupvars=c("class"))
# SEmpoor$group<-"mpoor"
# SEmpoor
# SEmpoor$sdb<-c(0,0.1000,0.5070681,0.3125)
# SEmpoor$sd<-c(0,0.3162278 ,0.41,0.4759858 )
# 
# #########
# 
# #ALL roadside
# #allcells4<-aggregate(count~ctmmlabel+class+release,occuroad,sum)
# #write.csv(allcells4,"allcells4.csv")
# #unique(allcells4$ctmmlabel)
# allcells4<-read.csv("allcells4.csv",header=TRUE)
# totalcells<-aggregate(count~ctmmlabel,occuroad,sum)
# totalcells
# names(totalcells)[2]<-"totalcount"
# allcells4<-merge(allcells4,totalcells,by="ctmmlabel")
# allcells4$prop<-(allcells4$count/allcells4$totalcount)
# head(allcells4)
# library(car)
# allcells4$logit2<-logit(allcells4$prop)  
# alllog<-glm(logit2~class,data=allcells4,family = gaussian(link = "identity"))  
# emm2<-emmeans(alllog,c("class"),type="response")
# joint_tests(emm2)
# pairs(emm2)
# emm2
# CLD(emm2)
# 
# SEroad<-summarySE(allcells4, measurevar="prop", groupvars=c("class"))
# SEroad$group<-"allroad"
# #SEroad$sig<-c("a","a","a","a")
# SEroad
# SEroad$sdb<-c(0.06597222   ,0.14781746   ,0.4721541  ,0.24801587  )
# 
# 
# #rw
# rwcells<-subset(allcells4,release=="poor-with")
# # rwcells<-aggregate(count~ctmmlabel+class+release,rw,sum)
# # rwtotalcells<-aggregate(count~ctmmlabel,rw,sum)
# # rwtotalcells
# # names(rwtotalcells)[2]<-"totalcount"
# # rwcells<-merge(rwcells,rwtotalcells,by="ctmmlabel")
# # rwcells$prop<-(rwcells$count/rwcells$totalcount)
# # head(rwcells)
# # rwcells$logit2<-logit(rwcells$prop)  
# rwlog<-glm(logit2~class,data=rwcells,family = gaussian(link = "identity"))  
# emm2<-emmeans(rwlog,c("class"),type="response")
# joint_tests(emm2)
# pairs(emm2)
# emm2
# CLD(emm2)
# #pwpp(emm)
# 
# SErw<-summarySE(rwcells, measurevar="prop", groupvars=c("class"))
# SErw
# SErw$group<-"rw"
# SErw
# SErw$sdb<-c(0.01  ,0.18192918  ,0.47922989  ,0.305  )
# 
# #rwo
# rwocells<-subset(allcells4,release=="poor-without")
# # rwocells<-aggregate(count~ctmmlabel+class+release,rwo,sum)
# # rwototalcells<-aggregate(count~ctmmlabel,rwo,sum)
# # rwototalcells
# # names(rwototalcells)[2]<-"totalcount"
# # rwocells<-merge(rwocells,rwototalcells,by="ctmmlabel")
# # rwocells$prop<-(rwocells$count/rwocells$totalcount)
# # head(rwocells)
# # rwocells$logit2<-logit(rwocells$prop)  
# rwolog<-glm(logit2~class,data=rwocells,family = gaussian(link = "identity"))  
# emm2<-emmeans(rwolog,c("class"),type="response")
# joint_tests(emm2)
# pairs(emm2)
# emm2
# CLD(emm2)
# #pwpp(emm)
# 
# SErwo<-summarySE(rwocells, measurevar="prop", groupvars=c("class"))
# SErwo
# SErwo$group<-"rwo"
# SErwo$sdb<-c(0.27  ,0 ,0.4811252  ,0)
# SErwo$sd<-c(0.48,0,0.28,0)
# 
# SE<-rbind(SEmosaic,SEroad,SEmcrop,SEmedge,SEmrich,SEmpoor,SErw,SErwo)
# SE$prop100<-(SE$prop*100)
# SE$sd100<-(SE$sd*100)
# SE$sdb100<-(SE$sdb*100)
# SE
# 
# 
# SE$release<-c("Mosaic","Mosaic","Mosaic","Mosaic",
#               "Roadside","Roadside","Roadside","Roadside",
#               "Crop","Crop","Crop","Crop",
#               "Edge","Edge","Edge","Edge",
#               "Resource- Rich","Resource- Rich","Resource- Rich","Resource- Rich",
#               "Resource- Poor","Resource- Poor","Resource- Poor","Resource- Poor",
#               "With Resource- Rich","With Resource- Rich","With Resource- Rich","With Resource- Rich",
#               "Without Resource- Rich","Without Resource- Rich","Without Resource- Rich","Without Resource- Rich")
# SE$habitat<-c("Crop","Edge","Resource-Poor","Resource-Rich",
#               "Crop","Edge","Resource-Poor","Resource-Rich",
#               "Crop","Edge","Resource-Poor","Resource-Rich",
#               "Crop","Edge","Resource-Poor","Resource-Rich",
#               "Crop","Edge","Resource-Poor","Resource-Rich",
#               "Crop","Edge","Resource-Poor","Resource-Rich",
#               "Crop","Edge","Resource-Poor","Resource-Rich",
#               "Crop","Edge","Resource-Poor","Resource-Rich")
# 
# SE$release<-factor(SE$release, levels=c("Mosaic","Roadside","Crop","Edge","Resource- Rich","Resource- Poor","With Resource- Rich","Without Resource- Rich"))
# 
# sig1<-c("a","a","a","b","","","","","","","","","","","","","","","","","","","","","","","","","","","","")
# sig2<-c("","","","","a","a","b","ab","","","","","","","","","","","","","","","","","","","","","","","","")
# sig3<-c("","","","","","","","","a","a","a","a","","","","","","","","","","","","","","","","","","","","")
# sig4<-c("","","","","","","","","","","","","a","b","ab","ab","","","","","","","","","","","","","","","","")
# sig5<-c("","","","","","","","","","","","","","","","","a","a","a","b","","","","","","","","","","","","")
# sig6<-c("","","","","","","","","","","","","","","","","","","","","a","a","b","ab","","","","","","","","")
# sig7<-c("","","","","","","","","","","","","","","","","","","","","","","","","a","ab","b","ab","","","","")
# sig8<-c("","","","","","","","","","","","","","","","","","","","","","","","","","","","","ab","a","b","a")
# 
# SE
# 
# D<-ggplot(SE, aes(fill=habitat, y=prop100, x=release, width=.8))+
#   geom_bar(stat="identity", color="black",
#            position=position_dodge())+
#   xlab("Release") +
#   ylab("Average Percent of Cells with \nHigh Occurrence Probability (>0.67)")+
#   theme_bw()+
#   scale_fill_brewer(palette="YlGnBu")+
#   scale_y_continuous(expand=c(0,0), limits=c(0,111))+
#   annotate("text",x=1.5,y=107,label="Sum",size=6)+
#   annotate("text",x=4.5,y=107,label="Mosaic",size=6)+
#   annotate("text",x=7.5,y=107,label="Roadside",size=6)+
#   geom_errorbar(aes(ymin=prop100-sdb100, ymax=prop100+sd100), width=.2, 
#                 position=position_dodge(.8))+
#   geom_text(aes(label=sig1, y=(prop100+sd100)),colour="chocolate4",vjust=-0.5, size=4.5, position=position_dodge(.8),fontface="plain")+
#   geom_text(aes(label=sig1, y=(prop100+sd100)),colour="chocolate4",vjust=-0.5, size=4.5, position=position_dodge(.8),fontface="plain")+
#   geom_text(aes(label=sig2, y=(prop100+sd100)),colour="darkgoldenrod3",vjust=-0.5, size=4.5, position=position_dodge(.8),fontface="plain")+
#   geom_text(aes(label=sig3, y=(prop100+sd100)),colour="darkolivegreen4",vjust=-0.5, size=4.5, position=position_dodge(.8),fontface="plain")+
#   geom_text(aes(label=sig4, y=(prop100+sd100)),colour="darkgreen",vjust=-0.5, size=4.5, position=position_dodge(.8),fontface="plain")+
#   geom_text(aes(label=sig5, y=(prop100+sd100)),colour="deepskyblue3",vjust=-0.5, size=4.5, position=position_dodge(.8),fontface="plain")+
#   geom_text(aes(label=sig6, y=(prop100+sd100)),colour="steelblue",vjust=-0.5, size=4.5, position=position_dodge(.8),fontface="plain")+
#   geom_text(aes(label=sig7, y=(prop100+sd100)),colour="darkblue",vjust=-0.5, size=4.5, position=position_dodge(.8),fontface="plain")+
#   geom_text(aes(label=sig8, y=(prop100+sd100)),colour="darkslategray",vjust=-0.5, size=4.5, position=position_dodge(.8),fontface="plain")+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"))+
#   theme(axis.text.x = element_text(size=12))+
#   theme(axis.text.x=element_text(colour="black"))+
#   theme(axis.text.y = element_text(size=14))+
#   theme(axis.text.y=element_text(colour="black"))+
#   scale_x_discrete(labels=label_wrap_gen(width=10))+
#   theme(plot.title=element_text(size=18))+
#   theme(axis.title = element_text(size = 16))+
#   ggtitle("D.")+
#   theme(legend.position = "bottom")+
#   guides(fill=guide_legend(title="Habitat Class"))+
#   theme(legend.text = element_text(size = 14))+
#   theme(legend.title = element_text(size = 14))+
#   theme(legend.box.background=element_rect(colour = "black"),
#         legend.background = element_blank())+
#   geom_vline(aes(xintercept=2.5),
#              linetype="dashed",colour="black",size=1)+
#   geom_vline(aes(xintercept=6.5),
#              linetype="dashed",colour="black",size=1)
# A
# B
# C
# D

#########################################################################################################

#time?
#georeferenced locations?


####################################################
##time in each habitat type

count<-read.csv("1920_timeandptsinhab2.csv", header=TRUE)
names(count)
unique(count$spatialconfig)
unique(count$Release)

head(count)

timemosaic<-subset(count, spatialconfig == "mosaic")
timeroad<-subset(count, spatialconfig == "roadside")


#ALL mosaic
allmtime<-aggregate(habtime~monarchrunid+habitatclass+Release+totaltime,timemosaic,sum)
head(allmtime)
allmtime$prop<-(allmtime$habtime/allmtime$totaltime)
allmtime$logit2<-logit(allmtime$prop)  
alllog<-glm(logit2~habitatclass,data=allmtime,family = gaussian(link = "identity"))  
emm2<-emmeans(alllog,c("habitatclass"),type="response")
joint_tests(emm2)
pairs(emm2)
emm2
CLD(emm2)

SEmosaic<-summarySE(allmtime, measurevar="prop", groupvars=c("habitatclass"))
SEmosaic$group<-"allmosaic"
SEmosaic
SEmosaic$sdb<-c(0.16  ,0.228 ,0.216  ,0.394  )

#ALL roadside
allrtime<-aggregate(habtime~monarchrunid+habitatclass+Release+totaltime,timeroad,sum)
head(allrtime)
allrtime$prop<-(allrtime$habtime/allrtime$totaltime)
allrtime$logit2<-logit(allrtime$prop)  
alllog<-glm(logit2~habitatclass,data=allrtime,family = gaussian(link = "identity"))  
emm2<-emmeans(alllog,c("habitatclass"),type="response")
joint_tests(emm2)
pairs(emm2)
emm2
CLD(emm2)

SEroad<-summarySE(allrtime, measurevar="prop", groupvars=c("habitatclass"))
SEroad$group<-"allroad"
SEroad
SEroad$sdb<-c(0.182  ,0.149  ,0.328 ,0.089)


#mcrop
mcrop<-subset(allmtime,Release=="crop")
mcroplog<-glm(logit2~habitatclass,data=mcrop,family = gaussian(link = "identity"))  
emm2<-emmeans(mcroplog,c("habitatclass"),type="response")
joint_tests(emm2)
pairs(emm2)
emm2
CLD(emm2)

SEmcrop<-summarySE(mcrop, measurevar="prop", groupvars=c("habitatclass"))
SEmcrop
SEmcrop$group<-"mcrop"
SEmcrop
SEmcrop$sdb<-c(0.366  ,0.0614  ,0.0744  ,0.233  )


#medge
medge<-subset(allmtime,Release=="edge")
medgelog<-glm(logit2~habitatclass,data=medge,family = gaussian(link = "identity"))  
emm2<-emmeans(medgelog,c("habitatclass"),type="response")
joint_tests(emm2)
pairs(emm2)
emm2
CLD(emm2)

SEmedge<-summarySE(medge, measurevar="prop", groupvars=c("habitatclass"))
SEmedge
SEmedge$group<-"medge"
#SEmedge$sig<-c("a","a","a","a")
SEmedge
SEmedge$sdb<-c(0.0174  ,0.4028 ,0.0157  ,0.2882  )
SEmedge$sd<-c(0.04717010  ,0.322 ,0.06873703  ,0.40270340  )

#mrich
mrich<-subset(allmtime,Release=="rich")
mrichlog<-glm(logit2~habitatclass,data=mrich,family = gaussian(link = "identity"))  
emm2<-emmeans(mrichlog,c("habitatclass"),type="response")
joint_tests(emm2)
pairs(emm2)
emm2
CLD(emm2)
#pwpp(emm)

SEmrich<-summarySE(mrich, measurevar="prop", groupvars=c("habitatclass"))
SEmrich
SEmrich$group<-"mrich"
SEmrich
SEmrich$sdb<-c(0.0084 ,0.1056  ,0.0354  ,0.2716686  )
SEmrich$sd<-c(0.0358739 ,0.2637452  ,0.1149361  ,0.15  )

#mpoor
mpoor<-subset(allmtime,Release=="poor")
mpoorlog<-glm(logit2~habitatclass,data=mpoor,family = gaussian(link = "identity"))  
emm2<-emmeans(mpoorlog,c("habitatclass"),type="response")
joint_tests(emm2)
pairs(emm2)
emm2
CLD(emm2)
#pwpp(emm)

SEmpoor<-summarySE(mpoor, measurevar="prop", groupvars=c("habitatclass"))
SEmpoor$group<-"mpoor"
SEmpoor
SEmpoor$sdb<-c(0.0106,0.0630    ,0.370    ,0.2300   )
SEmpoor$sd<-c(0.04769673,0.15266562    ,0.304    ,0.32282072   )

#rw
rwcells<-subset(allrtime,Release=="west")
rwlog<-glm(logit2~habitatclass,data=rwcells,family = gaussian(link = "identity"))  
emm2<-emmeans(rwlog,c("habitatclass"),type="response")
joint_tests(emm2)
pairs(emm2)
emm2
CLD(emm2)
#pwpp(emm)

SErw<-summarySE(rwcells, measurevar="prop", groupvars=c("habitatclass"))
SErw
SErw$group<-"rw"
SErw
SErw$sdb<-c(0.0675,0.1655,0.289 ,0.174 )

#rwo
rwocells<-subset(allrtime,Release=="east")
rwolog<-glm(logit2~habitatclass,data=rwocells,family = gaussian(link = "identity"))  
emm2<-emmeans(rwolog,c("habitatclass"),type="response")
joint_tests(emm2)
pairs(emm2)
emm2
CLD(emm2)
#pwpp(emm)

SErwo<-summarySE(rwocells, measurevar="prop", groupvars=c("habitatclass"))
SErwo
SErwo$group<-"rwo"
SErwo$sdb<-c(3.028328e-01 ,0.132 ,0.373 ,7.847706e-05)


SE<-rbind(SEmosaic,SEroad,SEmcrop,SEmedge,SEmrich,SEmpoor,SErw,SErwo)
SE$prop100<-(SE$prop*100)
SE$sd100<-(SE$sd*100)
SE$sdb100<-(SE$sdb*100)
SE


SE$release<-c("Pooled Mosaic","Pooled Mosaic","Pooled Mosaic","Pooled Mosaic",
              "Pooled Roadside","Pooled Roadside","Pooled Roadside","Pooled Roadside",
              "MZD","MZD","MZD","MZD",
              "MHE","MHE","MHE","MHE",
              "MHD","MHD","MHD","MHD",
              "MLD","MLD","MLD","MLD",
              "RHD","RHD","RHD","RHD",
              "RLD","RLD","RLD","RLD")
SE$habitat<-c("ZD","HE","LD","HD",
              "ZD","HE","LD","HD",
              "ZD","HE","LD","HD",
              "ZD","HE","LD","HD",
              "ZD","HE","LD","HD",
              "ZD","HE","LD","HD",
              "ZD","HE","LD","HD",
              "ZD","HE","LD","HD")

SE$release<-factor(SE$release, levels=c("Pooled Mosaic","MHD","MLD","MZD","MHE","Pooled Roadside","RHD","RLD"))
SE$habitat<-factor(SE$habitat, levels=c("HD","LD","ZD","HE"))

sig1c<-c("a","a","a","b","","","","","","","","","","","","","","","","","","","","","","","","","","","","")
sig2c<-c("","","","","a","a","b","a","","","","","","","","","","","","","","","","","","","","","","","","")
sig3c<-c("","","","","","","","","a","b","b","b","","","","","","","","","","","","","","","","","","","","")
sig4c<-c("","","","","","","","","","","","","a","c","a","b","","","","","","","","","","","","","","","","")
sig5c<-c("","","","","","","","","","","","","","","","","a","a","a","c","","","","","","","","","","","","")
sig6c<-c("","","","","","","","","","","","","","","","","","","","","a","ab","c","b","","","","","","","","")
sig7c<-c("","","","","","","","","","","","","","","","","","","","","","","","","a","a","b","a","","","","")
sig8c<-c("","","","","","","","","","","","","","","","","","","","","","","","","","","","","b","ab","c","a")

head(SE)

A<-ggplot(SE, aes(fill=habitat, y=prop100, x=release, width=.8))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Release Habitat Class") +
  ylab("% Time")+
  theme_bw()+
  scale_fill_brewer(palette="YlGnBu")+
  scale_y_continuous(expand=c(0,0), limits=c(0,119))+
  #annotate("text",x=1.5,y=107,label="Sum",size=6)+
  annotate("text",x=3,y=115,label="Mosaic Site",size=6)+
  annotate("text",x=7,y=115,label="Roadside Sites",size=6)+
  geom_errorbar(aes(ymin=prop100-sdb100, ymax=prop100+sd100), width=.2, 
                position=position_dodge(.8))+
  geom_text(aes(label=sig1c, y=(prop100+sd100)),colour="chocolate4",vjust=-0.5, size=4.5, position=position_dodge(.8),fontface="plain")+
  geom_text(aes(label=sig1c, y=(prop100+sd100)),colour="chocolate4",vjust=-0.5, size=4.5, position=position_dodge(.8),fontface="plain")+
  geom_text(aes(label=sig2c, y=(prop100+sd100)),colour="darkgoldenrod3",vjust=-0.5, size=4.5, position=position_dodge(.8),fontface="plain")+
  geom_text(aes(label=sig3c, y=(prop100+sd100)),colour="darkolivegreen4",vjust=-0.5, size=4.5, position=position_dodge(.8),fontface="plain")+
  geom_text(aes(label=sig4c, y=(prop100+sd100)),colour="darkgreen",vjust=-0.5, size=4.5, position=position_dodge(.8),fontface="plain")+
  geom_text(aes(label=sig5c, y=(prop100+sd100)),colour="deepskyblue3",vjust=-0.5, size=4.5, position=position_dodge(.8),fontface="plain")+
  geom_text(aes(label=sig6c, y=(prop100+sd100)),colour="steelblue",vjust=-0.5, size=4.5, position=position_dodge(.8),fontface="plain")+
  geom_text(aes(label=sig7c, y=(prop100+sd100)),colour="darkblue",vjust=-0.5, size=4.5, position=position_dodge(.8),fontface="plain")+
  geom_text(aes(label=sig8c, y=(prop100+sd100)),colour="darkslategray",vjust=-0.5, size=4.5, position=position_dodge(.8),fontface="plain")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=14))+
  theme(axis.text.y=element_text(colour="black"))+
  scale_x_discrete(labels=label_wrap_gen(width=10))+
  theme(plot.title=element_text(size=18))+
  theme(axis.title = element_text(size = 16))+
  ggtitle("A.")+
  theme(legend.position = "bottom")+
  guides(fill=guide_legend(title="Habitat Class"))+
  theme(legend.text = element_text(size = 14))+
  theme(legend.title = element_text(size = 14))+
  theme(legend.box.background=element_rect(colour = "black"),
        legend.background = element_blank())+
  geom_vline(aes(xintercept=5.5),
             linetype="dashed",colour="black",size=1)
  #geom_vline(aes(xintercept=6.5),
   #          linetype="dashed",colour="black",size=1)
A


####points

#ALL mosaic
allmpts<-aggregate(habpts~monarchrunid+habitatclass+Release+totalpts,timemosaic,sum)
head(allmpts)
allmpts$prop<-(allmpts$habpts/allmpts$totalpts)
allmpts$logit2<-logit(allmpts$prop)  
alllog<-glm(logit2~habitatclass,data=allmpts,family = gaussian(link = "identity"))  
emm2<-emmeans(alllog,c("habitatclass"),type="response")
joint_tests(emm2)
pairs(emm2)
emm2
CLD(emm2)

SEmosaic<-summarySE(allmpts, measurevar="prop", groupvars=c("habitatclass"))
SEmosaic$group<-"allmosaic"
SEmosaic
SEmosaic$sdb<-c(0.1414170  ,0.194 ,0.235  ,0.3766412  )

#ALL roadside
allrpts<-aggregate(habpts~monarchrunid+habitatclass+Release+totalpts,timeroad,sum)
head(allrpts)
allrpts$prop<-(allrpts$habpts/allrpts$totalpts)
allrpts$logit2<-logit(allrpts$prop)  
alllog<-glm(logit2~habitatclass,data=allrpts,family = gaussian(link = "identity"))  
emm2<-emmeans(alllog,c("habitatclass"),type="response")
joint_tests(emm2)
pairs(emm2)
emm2
CLD(emm2)

SEroad<-summarySE(allrpts, measurevar="prop", groupvars=c("habitatclass"))
SEroad$group<-"allroad"
SEroad
SEroad$sdb<-c(0.25678719  ,0.2088427  ,0.2813472 ,0.093)


#mcrop
mcrop<-subset(allmpts,Release=="crop")
mcroplog<-glm(logit2~habitatclass,data=mcrop,family = gaussian(link = "identity"))  
emm2<-emmeans(mcroplog,c("habitatclass"),type="response")
joint_tests(emm2)
pairs(emm2)
emm2
CLD(emm2)

SEmcrop<-summarySE(mcrop, measurevar="prop", groupvars=c("habitatclass"))
SEmcrop
SEmcrop$group<-"mcrop"
SEmcrop
SEmcrop$sdb<-c(0.3397671  ,0.059  ,0.12089947  ,0.2794702  )


#medge
medge<-subset(allmpts,Release=="edge")
medgelog<-glm(logit2~habitatclass,data=medge,family = gaussian(link = "identity"))  
emm2<-emmeans(medgelog,c("habitatclass"),type="response")
joint_tests(emm2)
pairs(emm2)
emm2
CLD(emm2)

SEmedge<-summarySE(medge, measurevar="prop", groupvars=c("habitatclass"))
SEmedge
SEmedge$group<-"medge"
#SEmedge$sig<-c("a","a","a","a")
SEmedge
SEmedge$sdb<-c(0.035087719  ,0.33850520 ,0.0029  ,0.35397202  )
#SEmedge$sd<-c(0.04717010  ,0.322 ,0.06873703  ,0.40270340  )

#mrich
mrich<-subset(allmpts,Release=="rich")
mrichlog<-glm(logit2~habitatclass,data=mrich,family = gaussian(link = "identity"))  
emm2<-emmeans(mrichlog,c("habitatclass"),type="response")
joint_tests(emm2)
pairs(emm2)
emm2
CLD(emm2)
#pwpp(emm)

SEmrich<-summarySE(mrich, measurevar="prop", groupvars=c("habitatclass"))
SEmrich
SEmrich$group<-"mrich"
SEmrich
SEmrich$sdb<-c(0.0263 ,0.086  ,0.065  ,0.236  )
SEmrich$sd<-c(0.08813615 ,0.15745768  ,0.15608013  ,0.179  )

#mpoor
mpoor<-subset(allmpts,Release=="poor")
mpoorlog<-glm(logit2~habitatclass,data=mpoor,family = gaussian(link = "identity"))  
emm2<-emmeans(mpoorlog,c("habitatclass"),type="response")
joint_tests(emm2)
pairs(emm2)
emm2
CLD(emm2)
#pwpp(emm)

SEmpoor<-summarySE(mpoor, measurevar="prop", groupvars=c("habitatclass"))
SEmpoor$group<-"mpoor"
SEmpoor
SEmpoor$sdb<-c(0.007142857,0.067094017    ,0.35712541     ,0.215195360   )
SEmpoor$sd<-c(0.03194383,0.11761339    ,0.289    ,0.32271421   )

#rw
rwcells<-subset(allrpts,Release=="west")
rwlog<-glm(logit2~habitatclass,data=rwcells,family = gaussian(link = "identity"))  
emm2<-emmeans(rwlog,c("habitatclass"),type="response")
joint_tests(emm2)
pairs(emm2)
emm2
CLD(emm2)
#pwpp(emm)

SErw<-summarySE(rwcells, measurevar="prop", groupvars=c("habitatclass"))
SErw
SErw$group<-"rw"
SErw
SErw$sdb<-c(0.138,0.2245998,0.2773218 ,0.1697745 )

#rwo
rwocells<-subset(allrpts,Release=="east")
rwolog<-glm(logit2~habitatclass,data=rwocells,family = gaussian(link = "identity"))  
emm2<-emmeans(rwolog,c("habitatclass"),type="response")
joint_tests(emm2)
pairs(emm2)
emm2
CLD(emm2)
#pwpp(emm)

SErwo<-summarySE(rwocells, measurevar="prop", groupvars=c("habitatclass"))
SErwo
SErwo$group<-"rwo"
SErwo$sdb<-c(0.24871710 ,0.16733500 ,0.29313332 ,0.01374269)


SE<-rbind(SEmosaic,SEroad,SEmcrop,SEmedge,SEmrich,SEmpoor,SErw,SErwo)
SE$prop100<-(SE$prop*100)
SE$sd100<-(SE$sd*100)
SE$sdb100<-(SE$sdb*100)
SE


SE$release<-c("Pooled Mosaic","Pooled Mosaic","Pooled Mosaic","Pooled Mosaic",
              "Pooled Roadside","Pooled Roadside","Pooled Roadside","Pooled Roadside",
              "MZD","MZD","MZD","MZD",
              "MHE","MHE","MHE","MHE",
              "MHD","MHD","MHD","MHD",
              "MLD","MLD","MLD","MLD",
              "RHD","RHD","RHD","RHD",
              "RLD","RLD","RLD","RLD")
SE$habitat<-c("ZD","HE","LD","HD",
              "ZD","HE","LD","HD",
              "ZD","HE","LD","HD",
              "ZD","HE","LD","HD",
              "ZD","HE","LD","HD",
              "ZD","HE","LD","HD",
              "ZD","HE","LD","HD",
              "ZD","HE","LD","HD")

SE$release<-factor(SE$release, levels=c("Pooled Mosaic","MHD","MLD","MZD","MHE","Pooled Roadside","RHD","RLD"))
SE$habitat<-factor(SE$habitat, levels=c("HD","LD","ZD","HE"))

sig1d<-c("a","a","a","b","","","","","","","","","","","","","","","","","","","","","","","","","","","","")
sig2d<-c("","","","","a","a","b","c","","","","","","","","","","","","","","","","","","","","","","","","")
sig3d<-c("","","","","","","","","a","c","bc","b","","","","","","","","","","","","","","","","","","","","")
sig4d<-c("","","","","","","","","","","","","a","b","a","b","","","","","","","","","","","","","","","","")
sig5d<-c("","","","","","","","","","","","","","","","","a","a","a","b","","","","","","","","","","","","")
sig6d<-c("","","","","","","","","","","","","","","","","","","","","a","ab","c","b","","","","","","","","")
sig7d<-c("","","","","","","","","","","","","","","","","","","","","","","","","a","ab","b","a","","","","")
sig8d<-c("","","","","","","","","","","","","","","","","","","","","","","","","","","","","a","b","a","c")

head(SE)

B<-ggplot(SE, aes(fill=habitat, y=prop100, x=release, width=.8))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Release Habitat Class") +
  ylab("% Georeferenced Locations")+
  theme_bw()+
  scale_fill_brewer(palette="YlGnBu")+
  scale_y_continuous(expand=c(0,0), limits=c(0,119))+
  #annotate("text",x=1.5,y=107,label="Sum",size=6)+
  annotate("text",x=3,y=115,label="Mosaic Site",size=6)+
  annotate("text",x=7,y=115,label="Roadside Sites",size=6)+
  geom_errorbar(aes(ymin=prop100-sdb100, ymax=prop100+sd100), width=.2, 
                position=position_dodge(.8))+
  geom_text(aes(label=sig1d, y=(prop100+sd100)),colour="chocolate4",vjust=-0.5, size=4.5, position=position_dodge(.8),fontface="plain")+
  geom_text(aes(label=sig1d, y=(prop100+sd100)),colour="chocolate4",vjust=-0.5, size=4.5, position=position_dodge(.8),fontface="plain")+
  geom_text(aes(label=sig2d, y=(prop100+sd100)),colour="darkgoldenrod3",vjust=-0.5, size=4.5, position=position_dodge(.8),fontface="plain")+
  geom_text(aes(label=sig3d, y=(prop100+sd100)),colour="darkolivegreen4",vjust=-0.5, size=4.5, position=position_dodge(.8),fontface="plain")+
  geom_text(aes(label=sig4d, y=(prop100+sd100)),colour="darkgreen",vjust=-0.5, size=4.5, position=position_dodge(.8),fontface="plain")+
  geom_text(aes(label=sig5d, y=(prop100+sd100)),colour="deepskyblue3",vjust=-0.5, size=4.5, position=position_dodge(.8),fontface="plain")+
  geom_text(aes(label=sig6d, y=(prop100+sd100)),colour="steelblue",vjust=-0.5, size=4.5, position=position_dodge(.8),fontface="plain")+
  geom_text(aes(label=sig7d, y=(prop100+sd100)),colour="darkblue",vjust=-0.5, size=4.5, position=position_dodge(.8),fontface="plain")+
  geom_text(aes(label=sig8d, y=(prop100+sd100)),colour="darkslategray",vjust=-0.5, size=4.5, position=position_dodge(.8),fontface="plain")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=12))+
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
        legend.background = element_blank())+
  geom_vline(aes(xintercept=5.5),
             linetype="dashed",colour="black",size=1)
  #geom_vline(aes(xintercept=6.5),
   #          linetype="dashed",colour="black",size=1)
B

grid.arrange(A,B)

