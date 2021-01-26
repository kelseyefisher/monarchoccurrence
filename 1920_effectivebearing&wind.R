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
summary<- merge(monarchs, summary, by="monarchrunid")

library(dplyr)
library(circular)
library(ggplot2)
library(grid)
library(lubridate)
library(emmeans)
library(lattice)
library(Rmisc)
library(gridExtra)

#################################################################################################

#orientation by release site

orient<-summary[!is.na(summary$euclidianbearing),]

winddirection <- data.frame("winddirection" = c("N","NNE", "NE", "ENE", "E", "SEE", "SE", "SSE", "S", "SSW", "SW", "SWW", "W", "NWW", "NW", "NNW"), "windbearing" = c(0, 22.5, 45, 67.5, 90, 112.5, 135, 157.5, 180, 202.5, 225, 247.5, 270, 292.5, 315, 337.5))
orient$windbearing <- winddirection$windbearing[match(orient$winddirection, winddirection$winddirection)]

orient$windcor <- (orient$euclidianbearing - orient$windbearing)
orient$adjeucbear <- ifelse(orient$windcor <0, orient$windcor +360, 
                          ifelse(orient$windcor >360, orient$windcor -360, orient$windcor)) 

library(circular)
orient$euclidianbearingdeg<-circular(orient$euclidianbearing, units="degrees", template="geographics")
orient$adjeucbeardeg<-circular(orient$adjeucbear, units="degrees", template="geographics")
write.csv(orient, "1920_orient.csv")

i<-subset(orient, releasesiteid == "1")
ii<-subset(orient, releasesiteid == "2")
iii<-subset(orient, releasesiteid == "3")
iv<-subset(orient, releasesiteid == "4")
v<-subset(orient, releasesiteid == "5")
vi<-subset(orient, releasesiteid == "3")
vii<-subset(orient, releasesiteid == "7")
viii<-subset(orient, releasesiteid == "8")
ix<-subset(orient, releasesiteid == "9")
x<-subset(orient, releasesiteid == "10")

#are they directional?
plot.circular(i$euclidianbearingdeg, stack=TRUE, col="darkslategrey")
rayleigh.test(i$euclidianbearingdeg)
plot.circular(ii$euclidianbearingdeg, stack=TRUE, col="darkslategrey")
rayleigh.test(ii$euclidianbearingdeg)
plot.circular(iii$euclidianbearingdeg, stack=TRUE, col="darkslategrey")
rayleigh.test(iii$euclidianbearingdeg)
plot.circular(v$euclidianbearingdeg, stack=TRUE, col="darkslategrey")
rayleigh.test(v$euclidianbearingdeg)
plot.circular(vi$euclidianbearingdeg, stack=TRUE, col="darkslategrey")
rayleigh.test(vi$euclidianbearingdeg)
plot.circular(vii$euclidianbearingdeg, stack=TRUE, col="darkslategrey")
rayleigh.test(vii$euclidianbearingdeg)
plot.circular(viii$euclidianbearingdeg, stack=TRUE, col="darkslategrey")
rayleigh.test(viii$euclidianbearingdeg)
plot.circular(ix$euclidianbearingdeg, stack=TRUE, col="darkslategrey")
rayleigh.test(ix$euclidianbearingdeg)
#only one that is significant
plot.circular(x$euclidianbearingdeg, stack=TRUE, col="darkslategrey")
cirmean<-mean(x$euclidianbearingdeg)
cirmean
arrows.circular(cirmean, col="darkslategrey", lwd=2)
rayleigh.test(x$euclidianbearingdeg)

#are they directional relative to the wind?
plot.circular(i$adjeucbeardeg, stack=TRUE, col="darkslategrey")
rayleigh.test(i$adjeucbeardeg)
plot.circular(ii$adjeucbeardeg, stack=TRUE, col="darkslategrey")
rayleigh.test(ii$adjeucbeardeg)
plot.circular(iii$adjeucbeardeg, stack=TRUE, col="darkslategrey")
rayleigh.test(iii$adjeucbeardeg)
plot.circular(v$adjeucbeardeg, stack=TRUE, col="darkslategrey")
rayleigh.test(v$adjeucbeardeg)
plot.circular(vi$adjeucbeardeg, stack=TRUE, col="darkslategrey")
rayleigh.test(vi$adjeucbeardeg)
plot.circular(vii$adjeucbeardeg, stack=TRUE, col="darkslategrey")
rayleigh.test(vii$adjeucbeardeg)
plot.circular(viii$adjeucbeardeg, stack=TRUE, col="darkslategrey")
rayleigh.test(viii$adjeucbeardeg)
plot.circular(ix$adjeucbeardeg, stack=TRUE, col="darkslategrey")
rayleigh.test(ix$adjeucbeardeg)
plot.circular(x$adjeucbeardeg, stack=TRUE, col="darkslategrey")
rayleigh.test(x$adjeucbeardeg)


i$adjwind<-c(180)
i$adjwind<-circular(i$adjwind, units="degrees", template="geographics")
data <- list(
  a = circular(i$adjeucbeardeg, units="degrees", template="geographics"),
  awind = circular(i$adjwind, units="degrees", template="geographics")
)
watson.williams.test(data)

plot.circular(i$adjeucbeardeg, stack=TRUE, col="darkslategrey")
cirmean<-circular(1.332466,units="degrees", template="geographics")
arrows.circular(cirmean, col="darkslategrey", lwd=2)
wind<- circular(180, units="degrees", template="geographics")
arrows.circular(wind, col="black", lwd=3, lty="dotted")



x$adjwind<-c(180)
x$adjwind<-circular(x$adjwind, units="degrees", template="geographics")
data <- list(
  a = circular(x$adjeucbeardeg, units="degrees", template="geographics"),
  awind = circular(x$adjwind, units="degrees", template="geographics")
)
watson.williams.test(data)

plot.circular(x$adjeucbeardeg, stack=TRUE, col="darkslategrey")
cirmean<-mean(x$adjeucbeardeg)
arrows.circular(cirmean, col="darkslategrey", lwd=2)
wind<- circular(180, units="degrees", template="geographics")
arrows.circular(wind, col="black", lwd=3, lty="dotted")




