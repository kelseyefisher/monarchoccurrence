#home laptop
#setwd("C:/Users/kelse/Box Sync/Publications/19&20 Habitat Utilization/Data Analysis")
#work
setwd("C:/Users/kefisher/Box/Publications/19&20 Habitat Utilization/Data Analysis")


ctmm19<-read.csv("19_itworked_forctmm.csv", header=TRUE)
ctmm20<-read.csv("20_itworked_forctmm.csv", header=TRUE)
release<-read.csv("1920_release.csv", header=TRUE)

ctmm<-rbind(ctmm19, ctmm20)
ctmm<-merge(ctmm, release, by="monarchrunid")

ctmm$individual.local.identifier<-ctmm$ctmmlabel

head(ctmm)
dates <- strptime(as.character(ctmm$date), format="%Y-%m-%d", tz="GMT")
#extract GMT times
times <- as.character(ctmm$time)
#create date-times and cast as POSIXct
datetimes <- as.POSIXct(paste(dates, times), tz="GMT")
#add appropriate datetimes into Monarch file
ctmm$timestamp <- datetimes
head(ctmm)

library(ctmm)
library(raster)

##turn into a telemetry object
monarchs<-as.telemetry(ctmm)



#####################################################
######FOLLOWING IS WHAT I WANT TO RUN AS A LOOP######
#####################################################



#fit a variogram from each monarch (individual.local.identifier = ac)
vg<- variogram(monarchs$`ac`)
#guess the best model fit for each variogram
GUESS<- ctmm.guess(monarchs$`ac`,
                    interactive = FALSE)
#include the error estimates in the calculation
GUESS$error<-TRUE
#Select best model and store the fit model for each individual
FIT<-ctmm.select(monarchs$`ac`,GUESS)
#run occurrence model for each individual based on best selected model
OCCU_ac<-occurrence(monarchs$`ac`,FIT)
plot(OCCU_ac)

OCCU.ac<-raster(OCCU_ac,level.UD=0.95,level=0.95,DF="PMF")
OCCU.OCCU.ac2<-projectRaster(OCCU.ac, crs=CRS('+proj=utm +zone=15'))
plot(OCCU.ac2)
writeRaster(OCCU.ac2, 'ac_occurrence.tif', options=c('TFW=YES'))









###next run of the loop would look like this (next individual.local.identifier = ae)...
vg_ae <- variogram(ctmm$`ae`)
GUESS_ae <- ctmm.guess(ctmm$`ae`,
                       interactive = FALSE)
GUESS_ae$error<-TRUE
FIT_ae<-ctmm.select(ctmm$`ae`,GUESS_ae)
OCCU_ae<-occurrence(ctmm$`ae`,FIT_ae)