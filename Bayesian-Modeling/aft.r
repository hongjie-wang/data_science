
library(tidyverse)
library(rjags)
library(survival)

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


time_un<-larynx$time[un]
cen_un<-larynx$time[un]
is.censored_un<-rep(0,length(cen_un))
X_un<-X[un,]
time_cen<-rep(NA,length(cen_cen))
is.censored_cen<-rep(1,length(cen_cen))
X_cen<-X[!un,]
cen_cen<-larynx$time[!un]

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
