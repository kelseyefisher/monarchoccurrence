#home laptop
setwd("C:/Users/Owner/Box Sync/ISU Research/2020/Summer Telemetry Analyses/CTMM 19")
#work desktop
setwd("C:/Users/kefisher/Box/ISU Research/2020/Summer Telemetry Analyses/CTMM 20")


locations<- read.csv("20_locationsonly.csv", header=TRUE)
names(locations)

####Converting UTM to Lat/Long
library(rgdal)
names(locations)
locations.sp<-locations
coordinates(locations.sp) <- c('locatione', 'locationn')
proj4string(locations.sp) <- CRS('+proj=utm +zone=15')
locations.latlong<-spTransform(locations.sp, CRS('+proj=longlat +datum=NAD83'))
coordinates(locations.latlong) 
locations<-as.data.frame(locations.latlong)
head(locations)
##rename location column names
colnames(locations)[colnames(locations)=="locatione"] <- "location.long"
colnames(locations)[colnames(locations)=="locationn"] <- "location.lat"
head(locations)
write.csv(locations, "20_locationsonly_latlong.csv")


monarchs<- read.csv("20_locationsonly.csv", header=TRUE)
names(monarchs)



##Fix date and time to create timestamp
#get dates in the right format
dates <- strptime(as.character(monarchs$date), format="%m/%d/%Y", tz="GMT")
#extract GMT times
times1 <- as.character(monarchs$startgmt)
times2 <- as.character(monarchs$endgmt)
#create date-times and cast as POSIXct
datetimes1 <- as.POSIXct(paste(dates, times1), tz="GMT")
datetimes2 <- as.POSIXct(paste(dates, times2), tz="GMT")
#add appropriate datetimes into Monarch file
monarchs$timestampstart <- datetimes1
monarchs$timestampend <- datetimes2
head(monarchs)


# Construct a data.frame with every second
d_full <- data.frame(date_time = seq(min(monarchs$timestampstart),
                                     max(monarchs$timestampend),
                                     by = "1 sec"))

names(monarchs)

#dummy variables for calculation
d_full$monarchrunid <- NA
d_full$ctmmlabel <- NA
d_full$locatione <- NA
d_full$locationn <- NA
d_full$location.long <- NA
d_full$location.lat <- NA



##Filter##
for (i in 1:nrow(d_full)){
  for (j in 1:nrow(monarchs)){
    if (d_full$date_time[i] >= monarchs$timestampstart[j] & d_full$date_time[i] <= monarchs$timestampend[j]){
      d_full$monarchrunid[i] <- monarchs$monarchrunid[j]
      d_full$ctmmlabel[i] <- monarchs$ctmmlabel[j]
      d_full$locatione[i] <- monarchs$locatione[j]
      d_full$locationn[i] <- monarchs$locationn[j]
      d_full$location.long[i] <- monarchs$location.long[j]
      d_full$location.lat[i] <- monarchs$location.lat[j]
    }
  }
  print(i)
}

##delete rows that didn't match
d_full <- d_full[-which(is.na(d_full$monarchrunid)),]


#####save output as a csv
test<-d_full
test$date_time<-as.character(test$date_time)
library(tidyr)
test2<-separate(data = test, col = date_time, into = c("date", "time"), sep = " ")
write.csv(test2, "itworked.csv")
