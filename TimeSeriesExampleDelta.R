# First attemp at working with time series data


setwd("~/2015 Summer Sampling/Zuur")
library(zoo)

#reads in data table
Delta <- read.table(file = "DeltaNoZero.txt", header = TRUE)

#headers and first 6 rows of data
head(Delta)
#SortIt Order       Date Image_Name SiteNo SideNo LFC LFCC LFCCR DF Pelicans Grebes Cormorant Percent_Obstructed
#1   1200  1822 28/05/2014   IMG_1245      1      1   0    0     0  0        0      0         0                  0
#2   1201  1823 28/05/2014   IMG_1246      1      1   0    0     0  0        0      0         0                  0
#3   1202  1824 28/05/2014   IMG_1247      1      1   0    0     0  0        0      1         0                  1
#4   1203  1825 28/05/2014   IMG_1248      1      1   0    0     0  0        0      0         0                  0
#5   1204  1826 28/05/2014   IMG_1249      1      1   0    0     0  0        0      0         0                  0
#6   1205  1827 28/05/2014   IMG_1250      1      1   0    0     0  0        0      0         0                  0
#Exposure Image_Clarity Water_Clarity Month Day Year     Time Time_as_Num Discharge_cms Velocity_m_s Argo_Depth_m
#1        2             1             3    28   5 2014 10:00:00     0.41667         3.475        0.105        2.933
#2        2             1             3    28   5 2014 10:15:00     0.42708         4.610        0.079        2.932
#3        2             1             3    28   5 2014 10:30:00     0.43750         6.814        0.076        2.936
#4        2             1             3    28   5 2014 10:45:00     0.44792         7.547        0.076        2.943
#5        2             1             3    28   5 2014 11:00:00     0.45833         7.860        0.082        2.939
#6        2             1             3    28   5 2014 11:15:00     0.46875         8.655        0.103        2.948
#Logger_Depth_m Logger_Temp_C DucksUnlimited_Temp
#1          1.065        14.515                15.5
#2          1.064        14.579                15.6
#3          1.064        14.654                15.7
#4          1.063        14.695                15.7
#5          1.063        14.710                15.7
#6          1.064        14.741                15.7

str(Delta)
'data.frame':  2420 obs. of  28 variables:
$ SortIt             : int  1 2 3 4 5 6 7 8 9 10 ...
$ Order              : int  1822 1823 1824 1825 1826 1827 1828 1829 1830 1831 ...
$ Date               : Factor w/ 37 levels "01/06/2014","01/07/2014",..: 31 31 31 31 31 31 31 31 31 31 ...
$ Image_Name         : Factor w/ 2420 levels "IMG_0002","IMG_0003",..: 693 694 695 696 697 698 699 700 701 702 ...
$ SiteNo             : int  1 1 1 1 1 1 1 1 1 1 ...
$ SideNo             : int  1 1 1 1 1 1 1 1 1 1 ...
$ LFC                : int  0 0 0 0 0 0 0 0 3 2 ...
$ LFCC               : int  0 0 0 0 0 0 0 0 3 2 ...
$ LFCCR              : int  0 0 0 0 0 0 0 0 3 2 ...
$ DF                 : int  0 0 0 0 0 0 0 0 0 0 ...
$ Pelicans           : int  0 0 0 0 0 0 0 0 0 0 ...
$ Grebes             : int  0 0 1 0 0 0 0 0 0 0 ...
$ Cormorant          : int  0 0 0 0 0 0 0 0 0 0 ...
$ Percent_Obstructed : int  0 0 1 0 0 0 0 0 0 0 ...
$ Exposure           : int  2 2 2 2 2 2 2 2 2 2 ...
$ Image_Clarity      : int  1 1 1 1 1 1 1 1 1 1 ...
$ Water_Clarity      : int  3 3 3 3 3 3 3 3 3 3 ...
$ Month              : int  28 28 28 28 28 28 28 28 28 28 ...
$ Day                : int  5 5 5 5 5 5 5 5 5 5 ...
$ Year               : int  2014 2014 2014 2014 2014 2014 2014 2014 2014 2014 ...
$ Time               : Factor w/ 70 levels "10:00:00","10:15:00",..: 1 2 3 4 5 6 7 8 9 10 ...
$ Time_as_Num        : num  0.417 0.427 0.438 0.448 0.458 ...
$ Discharge_cms      : num  3.48 4.61 6.81 7.55 7.86 ...
$ Velocity_m_s       : num  0.105 0.079 0.076 0.076 0.082 0.103 0.143 0.157 0.163 0.178 ...
$ Argo_Depth_m       : num  2.93 2.93 2.94 2.94 2.94 ...
$ Logger_Depth_m     : num  1.06 1.06 1.06 1.06 1.06 ...
$ Logger_Temp_C      : num  14.5 14.6 14.7 14.7 14.7 ...
$ DucksUnlimited_Temp: num  15.5 15.6 15.7 15.7 15.7 15.7 15.7 15.8 15.8 15.8 ...


objects()
#[1] "Delta"

#par is used to set graphical parameters
#c (1,2) says that it will have two panels in the plot
#mar is setting your margins
par(mfrow= c (1,2), mar = c(5,4,2,1))

m + geom_histogram(colour = "darkgreen", fill = "white", binwidth = 0.5)
qplot(LFC, data=Delta, geom="histogram") 


#histogram
hist(Delta$LFC)

#ggplot is a nicer plotting library
library(ggplot2)
#####################################################################################
#used to do scatterplots in ggplot2
ggplot(Delta, aes(x=Discharge_cms, y = LFCCR)) +geom_point(size = 1.5)

#size the points
ggplot(Delta, aes(x=Discharge_cms, y = LFCCR)) +geom_point(size = 1.5)

#shape the points, add labels and size
ggplot(Delta, aes(x=Discharge_cms, y = LFCCR)) +geom_point(shape = 2, size = 3)+
  xlab("Discharge cms")+ylab("Fish Count") +theme (axis.title.x=element_text(size=20))+
  theme (axis.title.y=element_text(size=20))+  geom_vline(xintercept=0, size=1)

#shape and size the points
ggplot(Delta, aes(x=Discharge_cms, y = LFCCR)) +geom_point(shape = 21,size = 2)

#plot two variables with different colour based on a factor
ggplot(Delta, aes(x = LFCCR, y=Discharge_cms, colour = Your factor)) +geom_point()

#plot two variables with different shape based on a factor
ggplot(Delta, aes(x = LFCCR, y=Discharge_cms, shape = Your factor)) +geom_point()
#####################################################################################
#line plots in ggplot2
ggplot(Delta, aes(x =SortIt , y=LFCCR)) +geom_line()+
  xlab("Order of Data")+ylab("Fish Count") +theme (axis.title.x=element_text(size=20))+
  theme (axis.title.y=element_text(size=20))

ggplot(Delta, aes(x =SortIt , y=Discharge_cms)) +geom_line()+
  xlab("Order of Data")+ylab("Discharge cms") +theme (axis.title.x=element_text(size=20))+
  theme (axis.title.y=element_text(size=20))

#point plot in ggplot2
ggplot(Delta, aes(x =Order , y=LFCCR)) +geom_point()

##########################################################################################
#Area plot will likely not work, need to use the code below as the colour will overflow the figure

Delta$Flow_Direction[Delta$Discharge_cms >= 0] <- "From the Lake"
Delta$Flow_Direction[Delta$Discharge_cms < 0] <- "From the Marsh"

ggplot(Delta, aes(x =SortIt , y=Discharge_cms))  +   geom_area(aes(fill=Flow_Direction))+
  geom_line()+ xlab("Order of Data")+ylab("Discharge cms") +theme (axis.title.x=element_text(size=20))+
  theme (axis.title.y=element_text(size=20))+  geom_hline(yintercept=0)

str(Delta)

# Interpolating the data will correct for area plot problems related to the area flowing into the wrong area
#geom_hline for horizontal lines
interp <- approx(Delta$SortIt, Delta$Discharge_cms, n =200000)
NewDelta <- data.frame(SortIt =interp$x, Discharge_cms=interp$y)
NewDelta$Flow_Direction[NewDelta$Discharge_cms >= 0] <- "From the Lake"
NewDelta$Flow_Direction[NewDelta$Discharge_cms < 0] <- "From the Marsh"

ggplot(NewDelta, aes(x =SortIt , y=Discharge_cms))  +   geom_area(aes(fill=Flow_Direction))+
  geom_line()+ xlab("Order of Data")+ylab("Discharge cms") +theme (axis.title.x=element_text(size=20))+
  theme (axis.title.y=element_text(size=20))+  geom_hline(yintercept=0)

########################################################################################
# Frequency plot in ggplot2 when you have a continous variable
qplot(Delta$LFCCR)

# Frequency plot in ggplot2 when you have a factor
qplot(factor(Delta$Time_as_Num))

#######################################################################################
# Histogram in ggplot2 a larger binwidth makes reduces the number of bins
ggplot(Delta, aes(x =LFCCR )) +geom_histogram(binwidth=1, fill="White", colour="Black")

ggplot(Delta, aes(x =Discharge_cms )) +geom_histogram(binwidth=1, fill="White", colour="Black")+
  xlab("Discharge cms")+ylab("Frequency") +theme (axis.title.x=element_text(size=20))+
  theme (axis.title.y=element_text(size=20))

ggplot(Delta, aes(x =LFCCR )) +geom_histogram(binwidth=1, fill="White", colour="Black")+
  xlab("Fish Count")+ylab("Frequency") +theme (axis.title.x=element_text(size=20))+
  theme (axis.title.y=element_text(size=20))

########################################################################################
#boxplot with one data vector plotted
boxplot(Delta$LFCCR,  ylab = "Fish Count")

#boxplot with three data vectors plotted
boxplot(Delta$LFC, Delta$LFCC, Delta$LFCCR)

##########################################################################################
MyNames <- c("Fish Count",  "Pelicans Count","Water Clarity",
             "Discharge cms", "Water Temp")


#simple Matrix Scatterplot
pairs(Delta[,c(9, 11, 17,  22, 23)])


# Run the three funtions below and then this to plot histo, cor and lm
pairs(Delta[,c(9, 11, 15, 17, 23, 28)], pch=".",
      upper.panel = panel.cor,
      diag.panel = panel.hist,
      lower.panel = panel.lm)


#calculates correlation in a matrix
panel.cor <- function(x, y, digits=2, prefix="", cex.cor,...){
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, use ="complete.obs"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.5/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex)
}

#creates histogram in  a matrix
panel.hist <- function(x, ...){
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="white", ...)
}

  
#calcualtes a lm in a matrix
panel.lm <- function (x, y, col=par("col"), bg=NA,pch=par("pch"),
                      cex=1, col.smooth = "black", ...){
  points(x, y, pch= pch, col=col, bg=bg, cex=cex)
  abline(stats::lm(y~x),col=col.smooth, ...)
}

##################################################################################################
#GAM Generalized additive model
library (mgcv)
x= Delta$Discharge_cms
y= Delta$LFCCR

plot(x, y,  ylab="Fish Count", xlab= "Discharge cms", pch=2,col = "brown", bg="orange")
model <-gam(y~s(x))
#xv <- seq (0.5, 1.3, 0.01)
#adjust the seq to the range of data
xv <- seq (-32.575, 45.921)
yv <- predict (model, list (x=xv))
lines (xv, yv)
summary (model)

##########################################################################################################
# autocorrelation and partial autocorrelation
Delta <- read.table(file = "DeltaNoZero.txt", header = TRUE)

plot(Delta$LFCCR)
library(ggplot2)
ggplot(Delta, aes(x =SortIt , y=LFCCR)) +geom_line()
ggplot(Delta, aes(x =SortIt , y=LFCCR)) +geom_point()

#plots lags in the data to look at autocorrelation
par(mfrow=c(2,2))
sapply(1:4, function(x) plot (Delta$LFCCR[-c(2420:(2420-x+1))], Delta$LFCCR[-c(1:x)]))

sapply(5:8, function(x) plot (Delta$LFCCR[-c(2420:(2420-x+1))], Delta$LFCCR[-c(1:x)]))

sapply(9:12, function(x) plot (Delta$LFCCR[-c(2420:(2420-x+1))], Delta$LFCCR[-c(1:x)]))

sapply(13:16, function(x) plot (Delta$LFCCR[-c(2420:(2420-x+1))], Delta$LFCCR[-c(1:x)]))


#plots autocorrelation function (acf) #main is a figure title
par(mfrow=c(1,1))
acf(Delta$LFCCR, main = "", col="red", lag = 200)

#plots partial autocorrelation
acf(Delta$LFCCR, type="p", main = "", col="red", lag = 20)

###############################################################################
#Plot moving averages, adjust ma to the value you want, this one is 3
x= Delta$SortIt
y= Delta$LFCCR
ma3 <- function (x) {
  y <- numeric(length (x) -2)
for (i in 2:(length(x)-1)){
    y[i] <- (x[i-1]+x[i]+x[i+1])/3
}
y}


tm<-ma3 (Delta$LFCCR)
par(mfrow=c(1,1))
plot(Delta$LFCCR)
lines (tm[2:2418], col ="blue")
################################################################################################
#Cross Correlation

ccf(Delta$LFCCR, Delta$Discharge_cms, col="red", lag = 100)

ccf(Delta$Discharge_cms, Delta$LFCCR,  col="red", lag = 200)

ccf(Delta$LFCCR, Delta$LFCCR,  col="red", lag = 10)
