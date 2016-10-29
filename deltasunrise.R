library(maptools)

#load sunrise/set times for portage la prairie from NRC canada website
#http://www.nrc-cnrc.gc.ca/eng/services/sunrise/advanced.html
delta_sunriseset<-read.csv("Portage la Prairie sunriseset.csv")

#tz = Canada/Central = daylight savings time
#
delta_sunriseset$DATETIME<-as.POSIXct(paste("2015",delta_sunriseset$Date),format="%Y %b %e",tz="Canada/Central" )

longlat<-matrix(c(-98.2047,50.1986),nrow=1)

delta_sunriseset$SUNRISE.REAL<-sunriset(crds=longlat, dateTime=delta_sunriseset$DATETIME,direction="sunrise",POSIXct.out=TRUE)$time

delta_sunriseset$SUNSET.REAL<-sunriset(crds=longlat, dateTime=delta_sunriseset$DATETIME,direction="sunset",POSIXct.out=TRUE)$time


delta_rise<-as.POSIXlt(delta_sunriseset$SUNRISE.REAL)


#rand.start function takes a time, interval and random interval and returns a random start time
#time = start time, must be POSIXct
#round.min = the minutes to round up to (i.e. 08:32 rounds to 08:45, 08:30 rounds to 08:30, 8:44 rounds to 08:45)
#rand.int = random interval to start sampling from
rand.start<-function(time=time,round.min=15,rand.int=4){
  if(!all(is.na(as.Date(as.character(time),format="%Y-%m-%d %H:%M:%S")))){
    rand.int=rand.int-1
    time<-round(as.POSIXlt(time),units="mins")
    start.time<-as.POSIXlt(ceiling(as.numeric(time)/(round.min*60))*(round.min*60),origin=(as.POSIXlt('1970-01-01')),tz="Canada/Central")
    time<-as.POSIXlt(start.time+(sample((0:rand.int),size=length(start.time),replace=TRUE)*(15*60)),origin=(as.POSIXlt('1970-01-01')),tz="Canada/Central")
  }else{
    warning("time must be POSIXct")
  }
  return(time)
}


delta_sunriseset$SUNRISE.START<-rand.start(time=delta_rise,round.min=15,rand.int=4)

start<-delta_sunriseset$SUNRISE.START
end<-as.POSIXlt(delta_sunriseset$SUNSET.REAL)
new.end$hour=end$hour
new.end$hour<-ifelse(new.end>end,new.end$hour-1,new.end$hour-0)

delta_sunriseset$SUNSET.END<-new.end


delta<-delta_sunriseset[,c("Date","SUNRISE.REAL","SUNSET.REAL","SUNRISE.START","SUNSET.END")]

   
write.csv(delta,file="Delta Random Start & End Times.csv")



ephemeris <- function(lat, lon, date, span=1, tz="UTC") {
  
  # convert to the format we need
  lon.lat <- matrix(c(lon, lat), nrow=1)
  
  # make our sequence - using noon gets us around daylight saving time issues
  day <- as.POSIXct(date, tz=tz)
  sequence <- seq(from=day, length.out=span , by="days")
  
  # get our data
  sunrise <- sunriset(lon.lat, sequence, direction="sunrise", POSIXct.out=TRUE)
  sunset <- sunriset(lon.lat, sequence, direction="sunset", POSIXct.out=TRUE)
  solar_noon <- solarnoon(lon.lat, sequence, POSIXct.out=TRUE)
  
  # build a data frame from the vectors
  data.frame(date=as.Date(sunrise$time),
             sunrise=as.numeric(format(sunrise$time, "%H%M")),
             solarnoon=as.numeric(format(solar_noon$time, "%H%M")),
             sunset=as.numeric(format(sunset$time, "%H%M")),
             day_length=as.numeric(sunset$time-sunrise$time))
  
}

ephemeris(longlat[,2],longlat[,1],"2014-05-28",37,tz="Canada/Central")


