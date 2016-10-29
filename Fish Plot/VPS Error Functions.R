#################
#LOAD NEW DATA
#By: David Callaghan
#Last Edit: 2015-10-15
################
#Loads VPS data and assigns datetime as POSIXct object and converts lat long to UTM
#projections
#
#
#data is the VPS data object
#timezone is the timezone of your study site for data analysis, default is GMT
#
load_coords<-function(data=data, zone=NULL, date = TRUE, timezone="GMT", input_timezone=timezone, output_timezone=timezone){
#required package to run function
require(PBSmapping)

if (date==TRUE){
  #Create POSIXct object and convert to appropriate timezone
  
  data$DATETIME<-as.POSIXct(data$DATETIME, tz=input_timezone)
  data$DATETIME<-as.POSIXct(as.POSIXlt(data$DATETIME, tz=output_timezone))
}

#add UTM####

if (is.null(zone)){
  # Convert it to UTM coordinates (in units of meters)
  utm<-data.frame(X = data$LON, Y = data$LAT)
  attr(utm, "projection") <- "LL"
  utm <- convUL(utm,km=F)
} else {
  # Convert it to UTM coordinates (in units of meters)
  utm<-data.frame(X = data$LON, Y = data$LAT)
  attr(utm, "projection") <- "LL"
  attr(utm, "zone") <- zone
  utm <- convUL(utm,km=F)
}


#Combine data and UTM positions
data<-cbind(utm,data)
#create column names for utm x and y coordinates
coln<-colnames(data)
coln[1]<-"UTM.X"
coln[2]<-"UTM.Y"
colnames(data)<-coln

return(data)
}


