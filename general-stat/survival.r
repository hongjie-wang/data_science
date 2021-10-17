set.seed(2345)
# simulate data from exp distribution
# rate=lambda=0.2, mean=1/lambda=5
# censored at time=6
sim<-5000
duration<-rexp(sim,rate=0.2)
plot(density(duration))
x<-pmin(duration,6)

censored<-ifelse(duration<=6,0,1)

# we further make the duration integer so that we can compare
# with discrete model better

x<-ceiling(x)


# define log ll function and get paramter for parametric model

expll <- function(parm) {
  n=length(x)
  temp=rep(0,n)
  lambda=exp(parm)
  for (i in 1:n){
    if (censored[i]!=1){
      temp[i]=log(lambda)+(-lambda*x[i])
    }
    else {
      temp[i]=-lambda*x[i]
    }
  }
  ll=sum(temp)
  return(-ll)
}

initial=0
bfgs_optim = optim(initial,expll,method="BFGS",hessian = TRUE)
# estimated lambda=0.184
print(exp(bfgs_optim$par))


# we can approximate it using logisic model using person-period data
# in this case, we estimate hazard function
# for exp distribution, hazard=lambda constant
# notice in discrete model, hazard is conditional prob
# in general, it is not

nbs=length(x)
mydata=data.frame()
for (id in 1:nbs){
  for (t in 1:x[id]){
    if ((t==x[id]) & censored[id]==0) status=1
    else status=0
    row=list(id=id,t=t,event=status)
    mydata=rbind(mydata,row)
  }
}

logit_model<-glm(event~1,data=mydata,family = "binomial")

# we get 0.185 
exp(logit_model$coefficients[1])/(1+exp(logit_model$coefficients[1]))