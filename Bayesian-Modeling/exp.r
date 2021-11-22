#https://sourceforge.net/p/mcmc-jags/discussion/610037/thread/50365933/

#https://stats.stackexchange.com/questions/133347/ml-estimate-of-exponential-distribution-with-censored-data


library(tidyverse)
library(rjags)
library(survival)

customers<-c(8,14,16,32,40,47,50,52,57,60,65,67,68,72,75,81,90,94,96,96,96,97,97,101)
customers_daily<-c(8,diff(customers))
part1<-data.frame(time=seq(1:24),customers=customers_daily, purchased=rep(1,24))
part2<-data.frame(time=24,customers=1499-101,purchased=0)
view<-rbind(part1,part2)
view<-view%>%
  filter(customers>0)%>%
  mutate(censored=1-purchased,
         left=time,
         right=ifelse(purchased!=1,NA,time)
         )

print (view)


fitdistrplus::fitdistcens(view, "exp",weight=view$customers)




survreg(Surv(time,censored)~1, data=view, weights = customers,dist="exp")
 
#fit exp model

expll <- function(parm) {
  n=length(view)
  temp=rep(0,n)
  lambda=exp(parm)
  for (i in 1:n){
    if (view$purchased[i]==1){
      temp[i]=log(lambda)+(-lambda*view$time[i])
    }
    else {
      temp[i]=-lambda*view$time[i]
    }
  }
  ll=sum(temp)
  return(-ll)
}
initial=1.2
bfgs_optim = optim(initial,expll,method="BFGS",hessian = TRUE)
# estimated lambda=0.184
print(exp(bfgs_optim$par))


survreg(formula=Surv(time,purchased)~1, data=view, dist="exponential")

km <- survfit(Surv(time, purchased)~1, data=view)
km

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
events<-view$purchased
time<-view$time
is.na(t)<-events==0
is.censored<-1-view$purchased
t.cen<-view$time+view$purchase

exp.spec<-textConnection(X.model)

d.jags<-list(n=nrow(view), time=time,t.cen=t.cen,is.censored=is.censored)
i.jags<-function() list(beta0=rnorm(1))
p.jags<-c("beta0")


exp.jags <- jags.model(exp.spec,data=d.jags, inits=i.jags,n.chains=3)
update(exp.jags,1000)

exp.res<-coda.samples(exp.jags,variable.names=p.jags,n.iter=50000,thin=10)

summary(exp.res)
