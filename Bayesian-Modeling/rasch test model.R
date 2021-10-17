# HWang 1-25-2021
# Rasch two parameter model
# y(i,j) are results of tests (j=1,p) given to individuals (1:n) y= got right or not;
#logit(y(ij))=lambda(j)*(intelligence(i)-difficulty(j))
# If intelligence is high, person is more likely to get correct, regardless of subjects;
# the difficult level of item j is negatively impacting getting right for that subject
# lambda(j) is so called discriminant factor, it shows 
# sensitivity of a problem in telling people apart
# if a problem is very difficult, and no one gets it right, then, it is not discriminating
# The goal of such modeling is to uncover these latent factors
# if you are interested in the persons taking the test, you can also get the factor for the intelligence
# If you are interested in the test designs, you can use this see their relative difficulty 
# and their ability to discriminate
# you can introduce other covarites for persons taking the tests to explain intelligence;
# you can also estimate difficulty and discriminating parameters by gender or race to see how different
# the problems impact are for different groups of people.

rm(list = ls())
library(HydeNet)
mynet<-HydeNetwork(~int+test1|int
                   +test2|int+
                    test3|int+
                    test4|int)

mynet<-setNode(network=mynet,node=int,nodeType = "dnorm",mean=0,tau=1)
# here 0.97 is discriminating factor, -1.66 is the difficult level (low)
# problem 3 is hard, problem 4 is discriminating
equation1<-"0.97*(int-(-1.66))";
equation2<-"1.26*(int-(-0.54))";
equation3<-"1.23*(int-(0.9))";
equation4<-"1.47*(int-(-0.12))";


mynet<-setNode(network=mynet,node=test1,nodeType = "dbern",
               prob=paste("ilogit(",equation1,")"),validate=FALSE)

mynet<-setNode(network=mynet,node=test2,nodeType = "dbern",
               prob=paste("ilogit(",equation2,")"),validate=FALSE)


mynet<-setNode(network=mynet,node=test3,nodeType = "dbern",
               prob=paste("ilogit(",equation3,")"),validate=FALSE)

mynet<-setNode(network=mynet,node=test4,nodeType = "dbern",
               prob=paste("ilogit(",equation4,")"),validate=FALSE)


 


writeNetworkModel(mynet,pretty=TRUE)
plot(mynet)


evidence=NULL

trackedVars<-c("test1","test2","test3","test4")

compiledNet<-compileJagsModel(mynet,data=evidence,n.chain=3,n.adapt=5000)
post<-HydeSim(compiledNet,variable.names =trackedVars,n.iter=1000)
dplyr::sample_n(post,10)


# 

library(rjags)
set.seed=1234
# lambda is positive 
mydata<-list(p=4,n=3000,
             y=cbind(post$test1,post$test2,post$test3,post$test4))


model= "model {
for (i in 1:n) { 
                 theta[i] ~ dnorm(0,1); #intelligence 
                 for (j in 1:p){ y[i,j] ~ dbern(pi[i,j])
                    pi[i,j] <- ilogit(lambda[j]*(theta[i]-alpha[j]))
                 }
}

# priors
for (j in 1:p) {lambda[j] ~ dnorm(0,tau.lambda) T(0,)
                 alpha[j] ~ dnorm(0,tau.alpha) }
tau.alpha ~ dexp(1)
tau.lambda ~ dexp(1)
}
"

# Initial Values and Estimation

inits1 <- list(lambda=rep(0.7,4),alpha=rep(0,4),theta=rep(0,3000))
inits2 <- list(lambda=rep(0.8,4), alpha=rep(0,4),theta=rep(0.2,3000))
inits=list(inits1,inits2)


mod=jags.model(textConnection(model),data=mydata,n.chains=2,inits=inits) 
update(mod,5000) 
pars <- c("lambda","alpha")
mod_sim=coda.samples(model=mod,variable.names=pars,n.iter=5000,seed=8899) 
summary(mod_sim)
 
