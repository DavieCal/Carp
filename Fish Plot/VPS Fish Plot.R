#################
#PLOT VPS DATA
#By: David Callaghan
#Last Edit: 2016-02-17
################
#librarys
################
source('VPS Error Functions.R')#load_coords function I created

library(PBSmapping)#required for load_coords
#if not installed use following line without the #
#install.packages("PBSmapping")

library(sp)#spatial polygons library
library(maptools)# map tools like loading shapefiles
#if not installed use following line without the #
#install.packages(c("sp","maptools))

################
#Load Data
################

#Load VPS dataset csv
all_data<-read.csv("VPS.csv")
#your data will something like this \/
#all_data<-read.csv("ALL-CALC-POSITIONS.csv")

#add UTM coordinates and POSIX class time 
#Note: VPS data is in GMT so just change the output_timezone the one for your data
#...CST is tricky...
data<-load_coords(data=all_data, output_timezone="MST")
#Note: make sure the the 2nd last line of the r output has the correct UTM zone in this case its zone 11

#Load shapefile
#load shore polygon and projection
shore<-readShapeSpatial(file.choose())
#add utm projection
proj4string(shore)<-CRS("+proj=utm +zone=11 +ellps=WGS84")

################
#Plot Data
################

##1 simple
#plot lake
plot(shore)
points(UTM.Y~UTM.X,data=data)


#lines

plot(shore)
lines(UTM.Y~UTM.X,data=data)


#alpha

plot(shore)
lines(UTM.Y~UTM.X,col=rgb(0,0,0,0.1),data=data)
points(UTM.Y~UTM.X,col=rgb(0,0,0,0.1),pch=19,data=data)

#movie

windows()	
#on mac quartz()

for(i in 1:dim(data)[1]){
  plot(shore)
  lines(UTM.Y~UTM.X,col=rgb(0,0,0,0.5),data=data[1:i,])
  points(UTM.Y~UTM.X,col=rgb(0,0,0,.2),pch=19,data=data[1:i,])
  points(UTM.Y~UTM.X,col=rgb(1,0,0,1),data=data[i,])
  Sys.sleep(0.5)
}
