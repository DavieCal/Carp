library(ZIM)
library(pscl)
#library(splines)
data(syph)
count<-syph$a33

ar1<-bshift(count)
trend<-1:length(count)/1000


zim(count ~ ar1 + trend | ar1 + trend)

summary(zeroinfl(count ~ ar1 + trend | trend))
acf(zim(count ~ ar1 | ar1 + trend,dist = "zinb")$residuals)


acf(zim(count ~ ar1+ ar2 | trend,dist = "zinb")$residuals)
acf(zim(count ~ ar1+ ar2 + ar3 | ar1+ ar2 + ar3 + trend ,dist = "zinb")$residuals)
acf(zim(count ~ ar2 + ar3| trend,dist = "zinb")$residuals)
acf(zim(count ~ ar3 | trend,dist = "zinb")$residuals)
acf(zim(count ~ ar2| trend,dist = "zinb")$residuals)
acf(zim(count ~ ar1 + ar3 | trend,dist = "zinb")$residuals)
acf(zim(count ~ ar1+ ar2 + ar3 + trend | trend,dist = "zinb")$residuals)

zim(count ~ ar2 + trend | trend)$aic
zim(count ~ ar3 + trend | trend)$aic

zim(count ~ ar1 + trend | trend, dist = "zinb")

x <- arima.sim(model = list(ar = 0.8, sd = 0.5), n = 120)
ar1<-bshift(x,12)
plot(count,type="l")
plot(ar1,type="l")
pacf(count)
x<-as.ts(Delta$LFCCR)
ar1<-arima(x)


plot(count[2:length(count)]~count[1:(length(count)-1)])

plot(count[3:length(count)]~count[1:(length(count)-2)])

plot(count[4:length(count)]~count[1:(length(count)-3)])
plot(count[5:length(count)]~count[1:(length(count)-4)])
pacf(count)
