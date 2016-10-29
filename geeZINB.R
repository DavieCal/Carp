######################################################################################
##  Authors: Maiying Kong, Sheng Xu, Steven Levy, and Somnath Datta                                                      ####
######################################################################################
#install.packages("pscl")
library(pscl)
#install.packages("MASS")
library(MASS)
#install.packages("gee")
library(gee)
#install.packages("Matrix")
library(Matrix)
#install.packages("geepack")
library(geepack)

####################################################
##      GEE ZINB for a single estimation                                         ##
####################################################

GEE.ZINB.Single.Iter <- function(X, Y, Z, beta0, gamma0, tao0=tao0, alpha10=0.5, alpha20=0.5){
  IDS <- unique(X$SID)
  X1<-X[, c(-1)]; X1<-as.matrix(cbind(rep(1,length(X[,1])), X1))
  Z1<-Z[, c(-1)]; Z1<-as.matrix(cbind(rep(1,length(Z[,1])), Z1))
  n.X <- length((X1[1,]))
  n.Z <- length((Z1[1,]))        
  DX<-X1; DZ<-Z1         
  Y1 <- as.vector(Y[, c(-1)])
  lamda1 <- exp(X1%*%beta0)
  eta1 <- exp(Z1%*%gamma0)
  p1<-eta1/(1+eta1)
  ############################
  ##  Update Uij                              ##
  #############################
  temp<-(1-p1)*(1+tao0*lamda1)^(-(1/tao0))
  ub<-ifelse(Y1==0,1,0)/(1+temp/p1)
  ##################################
  ## Update gamma and beta
  #####################################
  DevBeta <- as.vector(lamda1)
  DevGamma <- as.vector(p1*(1-p1))
  DX<-diag(DevBeta)%*%X1
  DZ<-diag(DevGamma)%*%Z1
  V.gamma<-p1*(1-p1)
  V.beta<-lamda1*(1+tao0*lamda1)
  
  dvd.gamma<- matrix(rep(0,n.Z*n.Z), nrow=n.Z, ncol=n.Z)
  dvd.beta<- matrix(rep(0,n.X*n.X), nrow=n.X, ncol=n.X)
  dvy.gamma<- matrix(rep(0,n.Z), nrow=n.Z, ncol=1)
  dvy.beta<- matrix(rep(0,n.X), nrow=n.X, ncol=1)
  
  for(i in IDS)
  {  index<-X$SID==i
  NumTeethi <-sum(index)
  Yi <- as.matrix(subset(Y,SID == i, c(-1)))
  Ri.gamma <- matrix(rep(alpha10, NumTeethi^2), nrow=NumTeethi)
  diag(Ri.gamma)<-1
  Vi.gamma <- diag(V.gamma[index])^(1/2) %*% Ri.gamma %*% diag(V.gamma[index])^(1/2)
  dvd.gamma<-dvd.gamma+t(DZ[index,])%*% ginv( Vi.gamma) %*%DZ[index,]
  dvy.gamma<- dvy.gamma+t(DZ[index,])%*% ginv( Vi.gamma) %*%(ub[index,]-p1[index])
  
  Ri.beta <- matrix(rep(alpha20, NumTeethi^2), nrow=NumTeethi)
  diag(Ri.beta)<-1
  Vi.beta<- diag(V.beta[index])^(1/2) %*% Ri.beta %*% diag(V.beta[index])^(1/2)
  dvd.beta<-dvd.beta+t(DX[index,])%*% ginv( Vi.beta) %*%diag(1-ub[index])%*%DX[index,]
  dvy.beta<- dvy.beta+t(DX[index,])%*% ginv( Vi.beta) %*%((1-ub[index])*(Y1[index]-lamda1[index]))
  }   
  gamma1 <- gamma0 + solve(dvd.gamma) %*% dvy.gamma
  beta1<- beta0 + solve(dvd.beta) %*% dvy.beta
  ##############################
  ##     Renew  tao                              ##
  ##############################
  lamda.new <- exp(X1%*%beta1)
  eta.new <- exp(Z1%*%gamma1)
  p.new<-eta.new/(1+eta.new)
  num.tao<-sum(lamda.new^2*(1-ub)^2*((Y1-lamda.new)^2-lamda.new))
  den.tao<-sum((1-ub)^2*lamda.new^4)
  tao.new<-max(0.1,num.tao/den.tao)
  #############################
  ##       Renew alpha                        ##
  ##############################
  u.alpha1<-(ub-p.new)/sqrt(p.new*(1-p.new))
  u.alpha2<-(1-ub)*(Y1-lamda.new)/sqrt(lamda.new*(1+tao.new*lamda.new))
  num1 <- num2<-0
  den1 <- den2<-0
  n1.total<-n1.star<-n2.total<-n2.star<-0
  for(i in IDS)
  {  index<-X$SID==i
  temp.u1<-u.alpha1[index]
  temp.u2<-u.alpha2[index]
  ub.i<-1-ub[index]
  NumTeethi <-sum(index)
  a1.num<-sum(temp.u1%*%t(temp.u1))-sum(temp.u1^2)
  a1.den<-sum(temp.u1^2)
  num1 <- num1+a1.num
  den1 <- den1+a1.den
  n1.star<-n1.star + NumTeethi*(NumTeethi-1)
  n1.total<-n1.total+NumTeethi
  a2.num<-sum(temp.u2%*%t(temp.u2))-sum(temp.u2^2)
  a2.den<-sum(temp.u2^2)
  num2 <- num2+a2.num
  den2 <- den2+a2.den
  n2.star<-n2.star + sum(ub.i%*%t(ub.i))-sum(ub.i^2)
  n2.total<-n2.total+sum(ub.i^2)
  }   
  
  alpha1<-(num1/n1.star)/(den1/n1.total)
  alpha2<-(num2/n2.star)/(den2/n2.total)
  iteration.list <- list(beta=beta1, gamma=gamma1, tao=tao.new, alpha1=alpha1, alpha2=alpha2)
  return(iteration.list)
}


################################################################
###  Estimating the GEE.ZINB untill convergence             ####
################################################################
GEE.ZINB.solve<- function(X, Y, Z, beta0, gamma0, tao0=tao0, alpha10=0.5, alpha20=0.5){
  Results.1<-GEE.ZINB.Single.Iter(X=X, Y=Y, Z=Z, beta0, gamma0, tao0, alpha10, alpha20)
  beta1=Results.1$beta; gamma1=Results.1$gamma; tao1=Results.1$tao
  alpha1=Results.1$alpha1; alpha2=Results.1$alpha2
  iter<-1; tao0<-tao1+0.05; alpha10<-alpha1+0.05; alpha20<-alpha2+0.05
  while ((max(abs(beta1-beta0), abs(gamma1-gamma0))>0.00001 | max(abs(tao0-tao1),abs(alpha1-alpha1),abs(alpha20-alpha2))>0.0005) & iter<50){beta0<-beta1; gamma0<-gamma1; tao0<-tao1 
  alpha10<-alpha1; alpha20<-alpha2
result<-GEE.ZINB.Single.Iter(X=X, Y=Y, Z=Z, beta0, gamma0, tao0,alpha10, alpha20)
beta1=result$beta; gamma1=result$gamma; tao1=result$tao
alpha1=result$alpha1; alpha2=result$alpha2
iter<-iter+1
}
if(!(iter<50)) print("Number of interations is larger than 50")
if(!(tao1>0.05)) print("Small Tao")
if(!(iter<50 & tao1>0.05)) return(list(Converge="Error"))
Result.Summary=list(Converge="YES",Beta=beta1, Gamma=gamma1,
                    tao=tao1, alpha1=alpha1, alpha2=alpha2)
return(Result.Summary)
}


###############################################################################
###  Estimate the variance: without considering var(u),where u is latent variable                        ##
###  Estimate the variance: considering var(u), where u is latent variables                                     ##
###############################################################################

GEE.ZINB.var<- function(X, Y, Z, beta0, gamma0, tao0=tao0, alpha10=0.5, alpha20=0.5){
  Results.1<-GEE.ZINB.Single.Iter(X=X, Y=Y, Z=Z, beta0, gamma0,tao0, alpha10, alpha20)
beta1=Results.1$beta
gamma1=Results.1$gamma
tao1=Results.1$tao
alpha1=Results.1$alpha1
alpha2=Results.1$alpha2
iter<-1; tao0<-tao1+0.05
alpha10<-alpha1+0.05
alpha20<-alpha2+0.05
while ((max(abs(beta1-beta0), abs(gamma1-gamma0))>0.00001 | max(abs(tao0-tao1),abs(alpha10-alpha1),abs(alpha20-alpha2))>0.0005) & iter<50)
{beta0<-beta1
gamma0<-gamma1
tao0<-tao1
alpha10<-alpha1
alpha20<-alpha2
result<-GEE.ZINB.Single.Iter(X=X, Y=Y, Z=Z, beta0, gamma0, tao0,alpha10, alpha20)
beta1=result$beta; gamma1=result$gamma; tao1=result$tao
alpha1=result$alpha1; alpha2=result$alpha2
iter<-iter+1
}
if(!(iter<50)) print("Number of interations is larger than 50")
if(!(tao1>0.05)) print("Small Tao")
if(!(iter<50 & tao1>0.05)) return(list(Converge="Error"))

#################################
####  Generate Sandwich Variance  ####
##################################
IDS <- unique(X$SID)
X1<-X[, c(-1)]
X1<-as.matrix(cbind(rep(1,length(X1[,1])), X1))
Z1<-Z[, c(-1)]
Z1<-as.matrix(cbind(rep(1,length(Z1[,1])), Z1))
n.X <- length((X1[1,]))
n.Z <- length((Z1[1,]))        
DX<-X1; DZ<-Z1         
Y1 <- as.vector(Y[, c(-1)])
lamda1 <- exp(X1%*%beta1)
eta1 <- exp(Z1%*%gamma1)
p1<-eta1/(1+eta1)
###########################
##    Update Uij                          ##
###########################
temp<-(1+tao1*lamda1)^(-(1/tao1))
ub<-ifelse(Y1==0,1,0)/(1+(1-p1)*temp/p1)
###############################
##   Update gamma and beta           ##
###############################
DevBeta <- as.vector(lamda1)
DevGamma <- as.vector(p1*(1-p1))
DX<-diag(DevBeta)%*%X1
DZ<-diag(DevGamma)%*%Z1
V.gamma<-p1*(1-p1)
V.beta<-lamda1*(1+tao1*lamda1)

B.gamma.gamma<-B.gamma.gamma.old<-M.gamma.gamma<- matrix(rep(0,n.Z*n.Z), nrow=n.Z, ncol=n.Z)
B.gamma.beta<-B.gamma.beta.old<-M.gamma.beta<- matrix(rep(0,n.Z*n.X), nrow=n.Z, ncol=n.X)
B.beta.gamma<-B.beta.gamma.old<-M.beta.gamma<- matrix(rep(0,n.X*n.Z), nrow=n.X, ncol=n.Z)
B.beta.beta<-B.beta.beta.old<-M.beta.beta<- matrix(rep(0,n.X*n.X), nrow=n.X, ncol=n.X)

B1<-B.old<-M<-matrix(rep(0,(n.X+n.Z)*(n.X+n.Z)), nrow=n.X+n.Z, ncol=n.X+n.Z)
dvy.gamma<- matrix(rep(0,n.Z), nrow=n.Z, ncol=1)
dvy.beta<- matrix(rep(0,n.X), nrow=n.X, ncol=1)

for(i in IDS)
{  index<-X$SID==i
NumTeethi <-sum(index)
Yi <- as.matrix(subset(Y,SID == i, c(-1)))
lamdai<- lamda1[index]
Epsi<-diag(as.vector((Yi-lamdai)^2-lamdai*(1+tao1*lamdai))); Gammai<-as.vector(lamdai^2) 
Ri.gamma <- matrix(rep(alpha1, NumTeethi^2), nrow=NumTeethi)
diag(Ri.gamma)<-1
Vi.gamma <- diag(V.gamma[index])^(1/2) %*% Ri.gamma %*% diag(V.gamma[index])^(1/2)
Ri.beta <- matrix(rep(alpha2, NumTeethi^2), nrow=NumTeethi)
diag(Ri.beta)<-1
Vi.beta<- diag(V.beta[index])^(1/2) %*% Ri.beta %*% diag(V.beta[index])^(1/2)  
##  Assume the same correlation matrix as marginal correlation of ui  ##
var.ui<-(diag(ub[index]*(1-ub[index])))^0.5%*%Ri.gamma%*%(diag(ub[index]*(1-ub[index])))^0.5

B.gamma.gamma.old<-B.gamma.gamma.old-t(DZ[index,])%*% ginv(Vi.gamma) %*%DZ[index,]
B.gamma.gamma<-B.gamma.gamma-t(DZ[index,])%*% ginv(Vi.gamma) %*%DZ[index,]
B.gamma.gamma<-B.gamma.gamma+t(DZ[index,])%*%
  ginv(Vi.gamma)%*%var.ui%*%ginv(Vi.gamma)%*%DZ[index,]

B.gamma.beta<-B.gamma.beta-t(DZ[index,])%*% ginv(Vi.gamma)%*%var.ui%*%
  diag(Y1[index]-lamda1[index])%*%ginv(Vi.beta)%*%DX[index,]

B.beta.gamma<-B.beta.gamma-t(DX[index,])%*% ginv(Vi.beta)%*%
  diag(Y1[index]-lamda1[index])%*%var.ui%*%ginv(Vi.gamma)%*%DZ[index,]

B.beta.beta.old<-B.beta.beta.old-t(DX[index,])%*% ginv(Vi.beta)%*%diag(1-ub[index])%*%DX[index,] 
B.beta.beta<-B.beta.beta-t(DX[index,])%*%ginv(Vi.beta)%*%diag(1-ub[index])%*%DX[index,] 
B.beta.beta<-B.beta.beta+t(DX[index,])%*%ginv(Vi.beta)%*%diag(Y1[index]-lamda1[index])%*%var.ui%*%diag(Y1[index]-lamda1[index])%*%ginv(Vi.beta)%*%DX[index,]

dvy.gamma<-t(DZ[index,])%*% ginv(Vi.gamma) %*%(ub[index,]-p1[index])
dvy.beta<- t(DX[index,])%*%ginv(Vi.beta) %*%diag(1-ub[index])%*%(Y1[index]-lamda1[index])  

M[1:n.Z, 1:n.Z]<-M[1:n.Z, 1:n.Z]+dvy.gamma%*%t(dvy.gamma)
M[1:n.Z, (n.Z+1):(n.X+n.Z)]<-M[1:n.Z, (n.Z+1):(n.X+n.Z)]+dvy.gamma%*%t(dvy.beta)

M[(1+n.Z):(n.X+n.Z), 1:n.Z]<-M[(1+n.Z):(n.X+n.Z), 1:n.Z]+dvy.beta%*%t(dvy.gamma)
M[(1+n.Z):(n.X+n.Z), (n.Z+1):(n.X+n.Z)]<-M[(1+n.Z):(n.X+n.Z),(n.Z+1):(n.X+n.Z)]+dvy.beta%*%t(dvy.beta)
} 

B1[1:n.Z, 1:n.Z]<-B.gamma.gamma 
B1[1:n.Z, (n.Z+1):(n.X+n.Z)]<-B.gamma.beta
B1[(1+n.Z):(n.X+n.Z), 1:n.Z]<-B.beta.gamma
B1[(1+n.Z):(n.X+n.Z), (n.Z+1):(n.X+n.Z)]<-B.beta.beta
B.old[1:n.Z, 1:n.Z]<-B.gamma.gamma.old 
B.old[(1+n.Z):(n.X+n.Z), (n.Z+1):(n.X+n.Z)]<-B.beta.beta.old

Var.model<-solve(-B1); Var.model.old<-solve(-B.old)
Var.Sand<-Var.model%*%M%*%Var.model
Var.Sand.old<-Var.model.old%*%M%*%Var.model.old
##################################################################################
## Var.Sand.old: Variance estimate without considering the latent variables     ##
## Var.Sand:  Variance estimate with considering the latent variables           ##
##################################################################################
se.gamma<-sqrt(diag(Var.Sand)[1:n.Z])
se.beta<-sqrt(diag(Var.Sand)[(n.Z+1):(n.X+n.Z)])
se.gamma.old<-sqrt(diag(Var.Sand.old)[1:n.Z])
se.beta.old<-sqrt(diag(Var.Sand.old)[(n.Z+1):(n.X+n.Z)])
beta.fit<-cbind(beta=beta1, se=se.beta, p.val=2*pnorm(-abs(beta1/se.beta)), se.old=se.beta.old, p.val=2*pnorm(-abs(beta1/se.beta.old))) 
gamma.fit<-cbind(gamma=gamma1, se=se.gamma, p.val=2*pnorm(-abs(gamma1/se.gamma)), se.old=se.gamma.old, p.val=2*pnorm(-abs(gamma1/se.gamma.old))) 
colnames(beta.fit)<-colnames(gamma.fit)<-c("Estimate", "S.E.", "p-val", "S.E.old", "p-val.old")
Result.Summary=list(Converge="YES",Beta=round(beta.fit,3), Gamma=round(gamma.fit,3),
                    tao=tao1, alpha1=alpha1, alpha2=alpha2)
return(Result.Summary)
}


#################################################################################
###  Estimate the variance using the bootstrap variance                        ##
#################################################################################

GEE.ZINB.bootstrap<- function(X, Y, Z, beta0, gamma0, tao0=tao0,alpha10=0.5, alpha20=0.5)
{   Results.1<-GEE.ZINB.Single.Iter(X=X, Y=Y, Z=Z, beta0, gamma0, tao0, alpha10, alpha20)
beta1=Results.1$beta; gamma1=Results.1$gamma; tao1=Results.1$tao
alpha1=Results.1$alpha1; alpha2=Results.1$alpha2
iter<-1; tao0<-tao1+0.05; alpha10<-alpha1+0.05; alpha20<-alpha2+0.05
while ((max(abs(beta1-beta0), abs(gamma1-gamma0))>0.00001 | max(abs(tao0-tao1),abs(alpha10-alpha1),abs(alpha20-alpha2))>0.0005) & iter<50)
{   beta0<-beta1; gamma0<-gamma1; tao0<-tao1
alpha10<-alpha1; alpha20<-alpha2
result<-GEE.ZINB.Single.Iter(X=X, Y=Y, Z=Z, beta0, gamma0, tao0,alpha10, alpha20)
beta1=result$beta; gamma1=result$gamma; tao1=result$tao
alpha1=result$alpha1; alpha2=result$alpha2
iter<-iter+1
}
if(!(iter<50)) print("Number of interations is larger than 50")
if(!(tao1>0.05)) print("Small Tao")
if(!(iter<50 & tao1>0.05)) return(list(Converge="Error"))

#######################################################################
##             Bootstrap estimate of the variance                    ##
#######################################################################
Beta.GEE.ZINB<-Gamma.GEE.ZINB<-c()
for (iter.bstr in (1:200))
{  print(iter.bstr)
  IDS <- unique(X$SID)
  new.IDS<-sample(IDS, size=length(IDS), replace = TRUE)
  XS<-X[X$SID==new.IDS[1],]
  ZS<-Z[Z$SID==new.IDS[1],]
  YS<-Y[Y$SID==new.IDS[1],]
  XS$SID<-ZS$SID<-YS$SID<-1
  for (my.id in 2:length(new.IDS))
  { index<-X$SID==new.IDS[my.id]
  temp1.X<-X[index,]; temp1.Z<-Z[index,]; temp1.Y<-Y[index,]
  temp1.X$SID<-temp1.Y$SID<-temp1.Z$SID<-my.id
  XS<-rbind(XS, temp1.X)
  ZS<-rbind(ZS, temp1.Z)
  YS<-rbind(YS, temp1.Y)
  }
  ResultS<-  GEE.ZINB.solve(X=XS, Y=YS, Z=ZS, beta1, gamma1, tao1, alpha10=alpha1, alpha20=alpha2)
  if(ResultS$Converge=="YES")
  { Beta.GEE.ZINB<-rbind(Beta.GEE.ZINB, t(ResultS$Beta))
  Gamma.GEE.ZINB<-rbind(Gamma.GEE.ZINB,t(ResultS$Gamma))
  }
}
gamma.sd.BS<-apply(Gamma.GEE.ZINB, 2, sd) 
beta.sd.BS<-apply(Beta.GEE.ZINB, 2, sd)
Beta.fit<-cbind(beta=beta1, se.BS=beta.sd.BS, p.val.BS=2*pnorm(-abs(beta1/beta.sd.BS)))
Gamma.fit<-cbind(gamma=gamma1, se.BS=gamma.sd.BS, p.val.BS=2*pnorm(-abs(gamma1/gamma.sd.BS)))
colnames(Beta.fit)<-colnames(Gamma.fit)<-c("Estimate", "S.E.", "p-val")
Result.Summary=list(Beta.fit=round(Beta.fit,3),Gamma.fit=round(Gamma.fit,3))
return(Result.Summary)
}

##########################################################################
##                        END of Bootstrap Methods                                           ##
##########################################################################
Dental <- read.csv("E:\\Sheng's Thesis 2013\\dental\\IowaF.csv", header=T)
head(Dental)
##     Delete the Missing Value   
NoMissing.Dental <- Dental[complete.cases(Dental), ]
NoMissing.Dental$GenderM<-ifelse(NoMissing.Dental$Gender=="M", 1,0)


X <- subset(NoMissing.Dental, select=c(SID, GenderM, DentalExamAge, 
                                       AUCmgF0_5yrs, AUCSodaOz0_5yrs, 
                                       ToothBrushingFreqPerDayAvg, DentalVisitPast6moAvg, 
                                       FluorideTreatment6moAvg, HomeFluorideppmAvg))
Z <- subset(NoMissing.Dental, select=c(SID, GenderM, DentalExamAge, 
                                       AUCmgF0_5yrs, AUCSodaOz0_5yrs, 
                                       ToothBrushingFreqPerDayAvg, DentalVisitPast6moAvg, 
                                       FluorideTreatment6moAvg, HomeFluorideppmAvg))

Y <- subset(NoMissing.Dental, select=c(SID, CariesCount)) 



############################################################################
##  Run ZINB Model
###########################################################################
mZINB <- zeroinfl(formula=CariesCount~ Gender + DentalExamAge + AUCmgF0_5yrs + AUCSodaOz0_5yrs + ToothBrushingFreqPerDayAvg
                  + DentalVisitPast6moAvg + FluorideTreatment6moAvg + HomeFluorideppmAvg, 
                  dist = "negbin", data = NoMissing.Dental)
summary(mZINB)$coeff

########################################################################
###   Run GEE.ZINB Model with three different variance estimates                                    ##
########################################################################
##   Obtain Initial Values for the parameters in GEE.ZINB
#######################################################################

beta00<-as.vector(summary(mZINB)$coefficients$count[,1])
beta0<-beta00[c(-length(beta00))]
gamma0<-as.vector(summary(mZINB)$coefficients$zero[,1])
tao0<-exp(beta00[c(length(beta00))])
alpha10<-alpha20<-0.2

Result1<-GEE.ZINB.var(X, Y, Z, beta0, gamma0, tao0, alpha10=0.5, alpha20=0.5)
Result1
my.table<-rbind(Result1$Beta,Result1$Gamma)
set.seed(999)
Result2<-GEE.ZINB.bootstrap(X, Y, Z, beta0, gamma0, tao0, alpha10=0.5, alpha20=0.5)
Result2
write.csv(Result2, "E:\\Sheng's Thesis 2013\\BStrip.csv")

#########################################################################
####               END                                                                                                                             ##
#########################################################################


################
###Carp COUNT###
################
CountZ<-Z2
Z2=Z
Z2$SID=Z2$Obs

X <-subset(Z2, select=c(SID, Discharge, Discharge2, Temp, DOY))
Z <-subset(Z2, select=c(SID, DOY, Pelicans, fWaterClarity))
Y <-subset(Z2, select=c(SID, Count))


mZINB <- zeroinfl(formula=Count~ Discharge +Discharge2+ Temp + DOY|DOY+Pelicans+fWaterClarity ,dist = "negbin", data = CountZ)
summary(mZINB)$coeff

########################################################################
###   Run GEE.ZINB Model with three different variance estimates                                    ##
########################################################################
##   Obtain Initial Values for the parameters in GEE.ZINB
#######################################################################

beta00<-as.vector(summary(mZINB)$coefficients$count[,1])
beta0<-beta00[c(-length(beta00))]
gamma0<-as.vector(summary(mZINB)$coefficients$zero[,1])
tao0<-exp(beta00[c(length(beta00))])
alpha10<-alpha20<-0.2

Result1<-GEE.ZINB.var(X, Y, Z, beta0, gamma0, tao0, alpha10=0.5, alpha20=0.5)
Result1
my.table<-rbind(Result1$Beta,Result1$Gamma)
set.seed(999)
Result2<-GEE.ZINB.bootstrap(X=X1, Y=Y1, Z=Z1, beta0, gamma0, tao0, alpha10=0.5, alpha20=0.5)
Result2

