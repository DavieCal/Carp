

Delta <- read.table(file = "DeltaNoZero.txt", header = TRUE)
str(Delta)
head(Delta)

Delta$DATETIME<-as.POSIXct(paste(Delta$Date,Delta$Time),format="%d/%m/%Y %H:%M:%S")
Delta$Time<-as.POSIXct
  
acf(Delta$LFCCR)
pacf(Delta$LFCCR)
ar1<-bshift(Delta$LFCCR>0)

plot(Delta$LFCCR)
plot(ar3)
plot(table(Delta$LFCCR))
plot(LFCCR~DATETIME,data=Delta, type="b",pch=19)

zim(Delta$LFCCR ~ ar3 + Delta$Discharge_cms + Pelicans| Water_Clarity + Percent_Obstructed, data=Delta)
zim(Delta$LFCCR ~ ar1 + Delta$Discharge_cms| Delta$Water_Clarity, dist = "zinb")


n<-dim(Delta)[1]
sub_Delta<-Delta[seq(sample((1:8),1),n,by=8),]
plot(table(sub_Delta$LFCCR))
plot(LFCCR~DATETIME,data=sub_Delta, type="b",pch=19)

acf(sub_Delta$LFCCR)
pacf(sub_Delta$LFCCR)  
  
#regression models for count data:
#hurdle (two-part) and zero infalted models (mixed)
#Zuileis et al 2008, Regression models in count data in R, 
#Journal of Statistical Software 27, 8.
library(pscl)

f1<-formula(LFCCR~)