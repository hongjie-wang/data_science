# HW 10-27-2021
# various ways to fit exp distribution with censored records

library(tidyverse)
library(rjags)
# simulate exp distribution with rate =0.2 (lambda)
# that corresponds to mean =5
sim<-5000
duration<-rexp(sim,rate=0.2)
plot(density(duration))
x<-pmin(duration,6)

censored<-ifelse(duration<=6,0,1)

view<-data.frame(time=x,censored=censored)

# fitdistcens needs two cols left and right
# for right censored records, left is time;
# right will put NA for the censored records;

view<-view%>%
  mutate(
         left=time,
         right=ifelse(censored==1,NA,time)
  )

# we see that it recovers the parameter 

fitdistrplus::fitdistcens(view, "exp")

# we can also fit an intercept only regression 
# rate=exp(-1.595438)
# for this procedure, it takes an event indicator not censored indicator

survival::survreg(Surv(time,1-censored)~1, data=view,dist="exp")
 
#fit exp model by ML directly

expll <- function(parm) {
  n=nrow(view)
  temp=rep(0,n)
  lambda=exp(parm)
  for (i in 1:n){
    if (view$censored[i]!=1){
      # f(t)=lambda exp(-lambda *t)
      temp[i]=log(lambda)+(-lambda*x[i])
    }
    else {
      # S(t)=exp(-lambda *t)
      temp[i]=-lambda*x[i]
    }
  }
  ll=sum(temp)
  return(-ll)
}
initial=1.2
bfgs_optim = optim(initial,expll,method="BFGS",hessian = TRUE)
print(exp(bfgs_optim$par))


# bayesian using jags
# id time censored 
# 1  40    1
# 2  35    0
# we add two cols
# new.time t.cen
# NA       40
# 35       35 
# to be used in function dinterval
# is.censored[i]~dinterval(new.time[i],t.cen)
# when censored=0, new.time=t.cen=observed time
# when censored=1 it is meant to say the unobserved time is more than t.cen

X.model <-"
model{
  for(i in 1:n)
  {
    is.censored[i]~dinterval(time[i],t.cen[i])
    time[i]~dexp(lambda[i])
    lambda[i]<-exp(beta0)
  }
  beta0~dnorm(0.0,0.001)
}
"

# define data
time<-view$time
is.na(time)<-view$censored==1 # redefine time to make NA for censored records
is.censored<-view$censored
t.cen<-view$time

exp.spec<-textConnection(X.model)

# data for jags
d.jags<-list(n=nrow(view), time=time,t.cen=t.cen,is.censored=is.censored)
#initial solution
i.jags<-function() list(beta0=rnorm(1))
p.jags<-c("beta0")

# step one model set up
exp.jags <- jags.model(exp.spec,data=d.jags, inits=i.jags,n.chains=3)

# step two, update 
update(exp.jags,1000)

# run and thin for MCMC inference
exp.res<-coda.samples(exp.jags,variable.names=p.jags,n.iter=50000,thin=10)

summary(exp.res)
# exp(-1.6153)=0.2


# instead of using distribution, write up the likelihood
# and use zero trick directly in jags
# we can use this approach to deal with any situation 
# where we have customized likelihood. 
X.model2 <-"
model{
for(i in 1:n)
{
  lambda[i]<-exp(beta0)
  logpdf[i]<-log(lambda[i])-lambda[i]*time[i]
  logsurv[i]<-(-lambda[i])*time[i]
  logll[i]<-(1-censored[i])*logpdf[i]+censored[i]*logsurv[i]
  phi[i]<-(-logll[i])+1000000
  zeros[i]~dpois(phi[i])

}
beta0~dnorm(0.0,0.001)
}
"

# define data
time<-view$time
censored<-view$censored

exp.spec<-textConnection(X.model2)

# data for jags
d.jags<-list(n=nrow(view), time=time,censored=censored, zeros=rep(0,nrow(view)))
#initial solution
i.jags<-function() list(beta0=rnorm(1))
p.jags<-c("beta0")

# step one model set up
exp.jags <- jags.model(exp.spec,data=d.jags, inits=i.jags,n.chains=3)

# step two, update 
update(exp.jags,1000)

# run and thin for MCMC inference
exp.res<-coda.samples(exp.jags,variable.names=p.jags,n.iter=50000,thin=10)

summary(exp.res)
# exp(-1.6153)=0.2

