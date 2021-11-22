#
rm(list=ls())
library(tidyverse)
library(rjags)
library(smcure)
data(bmt)
summary(bmt)

# here Status=1 is if not censored
survival::survreg(Surv(Time,Status)~TRT, data=bmt,dist="exp")
 
#two segments Y=1 and Y=0
# eta=prob(Y=1) and logit(eta)=B_{0,c}+B{1,c}*treatment
#y=1, it is not possible to have events, when Y=0, it follows an exp distribution
# so when Status=1 (not censored)--> prob(Y=0)*pdf
# when Status=0 -->prob (y=0)*survival + (prob(y=1))


# instead of using distribution, write up the likelihood
# and use zero trick directly in jags
# we can use this approach to deal with any situation 
# where we have customized likelihood. 

X.model <-"
model{
for(i in 1:n)
{
  logit(eta[i])<-inprod(betaC[],XC[i,])
  base[i]<-lambda
  elinpred[i]<-exp(inprod(betaU[],XU[i,]))
  logHaz[i]=log(base[i]*elinpred[i])
  logSurv[i]=(-lambda*t[i])*elinpred[i]
  logLike[i]<-delta[i]*(log(1-eta[i])+logHaz[i]+logSurv[i])+
  (1-delta[i])*log(eta[i]+(1-eta[i])*exp(logSurv[i]))
  phi[i]<-(-logLike[i])+1000000
  zeros[i]~dpois(phi[i])
}
for (l in 1:NbetasC){
betaC[l]~dnorm(0,0.001)
}
for (l in 1:NbetasU){
betaU[l]~dnorm(0,0.001)
}
lambda~dgamma(0.01,0.01)
}
"

XC<-model.matrix(~TRT, data=bmt)
XU<-model.matrix(~TRT,data=bmt)
XU<-matrix(XU[,-1],ncol=1)

exp.spec<-textConnection(X.model)

# data for jags
d.jags<-list(n=nrow(bmt), t=bmt$Time,XC=XC,XU=XU,delta=bmt$Status,
             zeros=rep(0,nrow(bmt)),NbetasC=ncol(XC),NbetasU=ncol(XU))
#initial solution
i.jags<-function() list(betaC=rnorm(ncol(XC)),betaU=rnorm(ncol(XU)),
                        lambda=runif(1))
p.jags<-c("betaC","betaU","lambda")

# step one model set up
exp.jags <- jags.model(exp.spec,data=d.jags, inits=i.jags,n.chains=3)

# step two, update 
update(exp.jags,1000)

# run and thin for MCMC inference
exp.res<-coda.samples(exp.jags,variable.names=p.jags,n.iter=50000,thin=10)

summary(exp.res)


#betaC[1] -1.039429 0.3569470 2.914e-03      2.985e-03
#betaC[2] -0.403312 0.5267589 4.301e-03      4.341e-03
#betaU     0.660672 0.2657663 2.170e-03      2.302e-03
#lambda    0.003879 0.0007651 6.247e-06      6.551e-06


