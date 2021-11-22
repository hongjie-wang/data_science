#
rm(list=ls())
library(tidyverse)
library(rjags)
library(smcure)

set.seed(123)
x <-rnorm(600)   
rates1 <- exp(0.5*x+1) # the risk exp(beta*x), parameters for exp r.v.
rates2 <-exp(-0.5*x-0.1)
y1 <- rexp(600, rate = rates1) # generates the r.v.
y2<-rexp(600,rate=rates2)

survreg(Surv(y1,rep(1,600))~x,dist="exp")

p<-exp(0.1+0.3*x)/(1+exp(0.1+0.3*x))

seg<-rbinom(600,1,p)

view<-data.frame(x=x,y1=y1,y2=y2,seg=seg)

view<-view%>%
  mutate(y=ifelse(seg==0,y1,y2))

head(view,10)
  

XS<-model.matrix(~x, data=view)
time<-view$y
w<-ifelse(view$y>0.5,1,0)

X.model <-"
model{
for(i in 1:n)
{
  logit(eta[i])<-inprod(betaS[],XS[i,])
  w[i]~dbern(eta[i])
  lambda1[i]<-exp(inprod(betaU1[],XS[i,]))
  lambda2[i]<-exp(inprod(betaU2[],XS[i,]))
  time[i]~dexp((1-w[i])*lambda1[i]+w[i]*lambda2[i])
}
for (l in 1:NbetasU){
betaS[l]~dnorm(0,0.001)
betaU1[l]~dnorm(0,0.001)
betaU2[l]~dnorm(0,0.001)
}
}
"

exp.spec<-textConnection(X.model)

# data for jags
d.jags<-list(n=nrow(view),time=time,XS=XS,
             NbetasU=ncol(XS))
#initial solution
i.jags<-function() list(betaU1=rnorm(ncol(XS)),w=w,
                        betaU2=rnorm(ncol(XS)),
                        betaS=rnorm(ncol(XS)))
p.jags<-c("betaU1","betaU2","betaS")

# step one model set up
exp.jags <- jags.model(exp.spec,data=d.jags, inits=i.jags,n.chains=3)

# step two, update 
update(exp.jags,1000)

# run and thin for MCMC inference
exp.res<-coda.samples(exp.jags,variable.names=p.jags,n.iter=50000,thin=10)

summary(exp.res)

#Mean     SD Naive SE Time-series SE
#betaC[1] -1.0427 0.3596 0.002936       0.002943
#betaC[2] -0.3954 0.5240 0.004278       0.004113
#betaU[1] -5.5737 0.2018 0.001647       0.001723
#betaU[2]  0.6616 0.2657 0.002170       0.002270



