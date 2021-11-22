

#weibull
#hazard: alpha*lambda*x^{alpha-1}
#survival: exp(-lambda * x^{alpha})
#density hazard*survival
# lambda is the scale parameter, alpha is shape
# in proportional, we have h(x|Z)=h0(x)*exp(beta Z)
# s(x|Z)=s0(x)^exp(beta Z), note the diff with hazard

rm(list=ls())

library(tidyverse)
library(rjags)
library(KMsurv)


data(larynx)
summary(larynx)


view<-larynx%>%
  mutate(age=as.numeric(scale(age)),
         diagyr=as.numeric((scale(diagyr))),
         stage=as.factor(stage))

survival::survreg(formula = Surv(time, delta)~factor(stage)+age+diagyr, 
                  data = view, dist = "weibull")


  
X<-model.matrix(~stage+age+diagyr,data=view)

un<-view$delta==1

time_un<-view$time[un]
cen_un<-view$time[un]
is.censored_un<-rep(0,length(cen_un))
X_un<-X[un,]
cen_cen<-view$time[-un]
time_cen<-rep(NA,length(cen_cen))
is.censored_cen<-rep(1,length(cen_cen))
X_cen<-X[!un,]
cen_cen<-view$time[!un]

time<-c(time_un,time_cen)
cen<-c(cen_un, cen_cen)
is.censored<-c(is.censored_un,is.censored_cen)
Xnew<-rbind(X_un,X_cen)



X.model <-"
model{
for (i in 1:n){
is.censored[i]~dinterval(time[i],cen[i])
time[i]~dweib(alpha, lambda[i])
lambda[i]<-exp(-mu[i]*alpha)
mu[i]<-inprod(beta[],X[i,])
}
for (l in 1:Nbetas){
beta[l]~dnorm(0,0.001)
}
alpha~dunif(0,10)
}
"

aft.spec<-textConnection(X.model)

d.jags<-list(n=nrow(X), time=time,cen=cen,X=Xnew,is.censored=is.censored,Nbetas=ncol(X))
i.jags<-function() list(beta=rnorm(ncol(X)), alpha=runif(1))
p.jags<-c("beta","alpha")


aft.jags <- jags.model(aft.spec,data=d.jags, inits=i.jags,n.chains=3)
update(aft.jags,1000)

aft.res<-coda.samples(aft.jags,variable.names=p.jags,n.iter=50000,thin=10)

summary(aft.res)


# see if we can comparalbe results using proporitonal hazard with weibull base hazard


X.model2 <-"
model{
for (i in 1:n){
base[i]<-lambda*alpha*pow(time[i],alpha-1)
elinpred[i]<-exp(inprod(beta[],X[i,]))
logHaz[i]<-log(base[i]*elinpred[i])
logSurv[i]<-(-lambda)*pow(time[i],alpha)*elinpred[i]
logpdf[i]<-logHaz[i]+logSurv[i]
logLike[i]<-(1-is.censored[i])*logpdf[i]+is.censored[i]*logSurv[i]
phi[i]<-100000-logLike[i]
zeros[i]~dpois(phi[i])
}
for (l in 1:Nbetas){
beta[l]~dnorm(0,0.001)
}
alpha~dunif(0,10)
lambda~dgamma(0.01,0.01)
}
"

cox.spec<-textConnection(X.model2)
#remove intercept for cox since the baseline contains it

Xnew<-matrix(Xnew[,-1],ncol=5)
d.jags<-list(n=nrow(X), time=cen,X=Xnew,is.censored=is.censored,
             zeros=rep(0,nrow(view)),Nbetas=ncol(Xnew))
i.jags<-function() list(beta=rnorm(ncol(Xnew)), alpha=runif(1), lambda=runif(1))
p.jags<-c("beta","alpha","lambda")


cox.jags <- jags.model(cox.spec,data=d.jags, inits=i.jags,n.chains=3)
update(aft.jags,1000)

aft.res<-coda.samples(aft.jags,variable.names=p.jags,n.iter=50000,thin=10)

summary(aft.res)