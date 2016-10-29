


library(MCMCglmm)
library(nlme)
#determine phi
m<-gls(Count~Discharge+Time,correlation=corAR1(form=~1|DOY),data=Z)
phi=0.7434478
#create correlation matrix
cAR1 <- corAR1(phi, form = ~ 1 | DOY)
cAR1 <- Initialize(cAR1, Z)
mat <- as(corMatrix(cAR1)[[1]], "sparseMatrix")

# generate AR1 covariance matrix

# IMPORTANT: the inverse needs to be passed, rather than the actual  
#covariance matrix. This is preferable because the inverse is often  
#sparser, and MCMCglmm may not "know" this from the matrix and a brute  
#force solve. Your example is a good one, because the inverse of an AR1  
#covaraince matrix  (when time points are ordered) only has non-zero  
#elements along the diagonal and sub-diagonals.  However, inverting mat  
#gives a dense matrix because of numerical inaccuracies:

length(solve(mat)@x)

# gives 2206 non-zero elements, but should really be :

dim(mat)[1]+2*(dim(mat)[1]-1)

# 139 non-zero elements. So:

smat<-solve(mat)
smat<-band(smat,c(-1,1), c(1,-1))

# also, it makes more sense for this to define the autocovariance  
#structure within each Rat (rather than across Rats) as in m3.  Since  
#the deisgn is balanced this is easy to form:

invC<-kronecker(diag(nlevels(Z$DOY)), smat)
invC<-as(invC,  "CsparseMatrix")

# create a new variable with a unique name for each datum (and have  
#the rownames in invC correspond)

rownames(invC) <- 1:dim(Z)[1]
Z$fObs<-as.factor(1:dim(Z)[1])

# Then fit

prior=list(R=list(V=1, nu=0.02), 
           G=list(G1=list(V=diag(2), nu=2, alpha.mu=c(0,0),alpha.V=diag(2)*1000), 
          G2=list(V=1,nu=1, alpha.mu=0,alpha.V=1000)))

m3.mcmc <- MCMCglmm(Count ~ Discharge + Temp, random=~us(1+Time):Rat+timeRat,  
                    ginverse=list(timeRat=invC), data=BodyWeight, verbose=F, nitt=53000,  
                    pr=T, prior=prior)

# This is almost identical to m3 except the AR1 process is for the  
random effects rather than the residuals. In m3 the covariance  
structure within each Rat is equal to vr*mat where vr is the residual  
variance. In  MCMCglmm  it is  equal to   va*mat +vb*I  where va is  
the variance associated with the timeRat term and vb is the variance  
associated with the units term (residual variance).

# Nevertheless, the lme point estimates correspond very closely to the  
posterior modes for all parameters:
  
  f1<-summary(m3)$coef$fixed[1]
f2<-summary(m3)$coef$fixed[2]
v1<-as.numeric(VarCorr(summary(m3))[1])
v2<-as.numeric(VarCorr(summary(m3))[2])
c12<-as.numeric(VarCorr(summary(m3))[,3][2])*sqrt(v1*v2)
vr<-as.numeric(VarCorr(summary(m3))[3])


par(mfrow=c(1,2))
hist(m3.mcmc$Sol[,1], breaks=30)
abline(v=f1, col="red")
hist(m3.mcmc$Sol[,2], breaks=30)
abline(v=f2, col="red")

par(mfrow=c(2,2))
hist(m3.mcmc$VCV[,1], breaks=30)
abline(v=v1, col="red")
hist(m3.mcmc$VCV[,4], breaks=30)
abline(v=v2, col="red")
hist(m3.mcmc$VCV[,2], breaks=30)
abline(v=c12, col="red")
hist(rowSums(m3.mcmc$VCV[,5:6]), breaks=30)
abline(v=vr, col="red")



>
