########################
## EDAV FINAL PROJECT ##
########################
rm(list = ls())
cat("\014") 
clf
dev.off()

setwd("/Users/xavierign/Google Drive/EDAV_final_project/")

require(lubridate)
library(ggmap)
library(ggplot2)

#read the data. since the data is huge big so that just read in 400000 rows including the data in 2015
request311 <- read.csv('311_Service_Requests_from_2010_to_Present.csv', nrows = 3857351)

#read the police department
poli <- read.csv('nypd-nyfd-stations.csv', nrows = 295)

#Convert the date in our date into standard Date format in R
request311$Created.Date <- mdy_hms(request311$Created.Date)

request311$Closed.Date <- mdy_hms(request311$Closed.Date)

#convert to hours
dif <-  as.numeric(request311$Closed.Date - request311$Created.Date)/60/60

#remove the NA is Closed in lat and lon
row.to.delete <- is.na(request311$Closed.Date)|is.na(request311$Latitude)|is.na(request311$Longitude)

Created.date <- request311$Created.Date[!row.to.delete ]
Lat <- request311$Latitude[!row.to.delete ]
Lon <- request311$Longitude[!row.to.delete ]
Closed.Date <- request311$Closed.Date[!row.to.delete ] # this is the last
dif <- dif [!row.to.delete ] 
agency <- request311$Agency[!row.to.delete]

map.data <- data.frame(dif,Lat,Lon,agency)

#assign negatives to zero
dif[dif<0] <- 0
dif[dif>720] <- 720

#HPD       NYPD        DOT        DEP       DSNY        DOB        DPR      DOHMH        DOF 
#1239562     636536     630411     349969     296454     178004     147732     111758      82218 

## NYC map
myLocation <- 'New York City'
maptype = 'terrain'
myMap <- get_map(location=myLocation,
                 source="google", maptype=maptype, crop=FALSE,
                 zoom=12)

#NYPD
dif.nypd <- dif
dif.nypd[dif.nypd>7]<-7

map.data <- data.frame(dif,dif.nypd,Lat,Lon,agency)

ggmap(myMap, extent = "device")+
  geom_point(aes(x = Lon, y = Lat, col=dif.nypd),
             size=0.01, data = map.data[map.data$agency=='NYPD',], 
             alpha = 0.5) +
  scale_colour_gradient2( low = "green", mid = "green", high = "red",
                         midpoint = 0, space = "rgb", na.value = "grey50", guide = "colourbar")+
  
  geom_point(aes(x=Longitude, y=Latitude), size=0.5, col='black',data= poli) +
  
  ggtitle('Resolution times -NYPD') 

