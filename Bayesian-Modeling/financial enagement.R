# HWang 1-24-2021
# latent factor analytic model

# simulation of the data
# a)latent factor fin as a function (income, age): people's latent fin needs;
# b)fin.measure is fin +error- we do not observe this factor directly either
# c)transaction type j for customer i y(i,j):
# y(i,j)~alpha(j)+lambda(j)*fin.measure(i)+kappa(j)*personal(i)
# utility for type j is a function of fin latent factor and personal factor

# note: for banking application, this latent factor may be more interesting and useful
# than the coefficients and other parameters
rm(list = ls())
library(HydeNet)
mynet<-HydeNetwork(~income+age+
                     fin|income*age+
                     fin.measure|fin+
                     personal
                   +trans1|personal*fin.measure
                   +trans2|personal*fin.measure
                   +trans3|personal*fin.measure)

mynet<-setNode(network=mynet,node=income,nodeType = "dnorm",mean=40,tau=0.1)

mynet<-setNode(network=mynet,node=age,nodeType = "dunif",a=20,b=80)


mynet<-setNode(network=mynet,node=fin,nodeType ="determ",define=fromFormula(),
             nodeFormula = fin~0.4*income+0.2*age)

mynet<-setNode(network=mynet, node=fin.measure, nodeType = "dnorm", 
        mean="fin",tau=0.5)

mynet<-setNode(network=mynet, node=personal, nodeType = "dnorm", 
        mean=0,tau=1)


mynet<-setNode(network=mynet,node=trans1,nodeType ="dpois",
        lambda="exp(0.5+0.1*fin.measure+0.5*personal)")

mynet<-setNode(network=mynet,node=trans2,nodeType ="dpois",
        lambda="exp(2-0.05*fin.measure+0.3*personal)")


mynet<-setNode(network=mynet,node=trans3,nodeType ="dpois",
        lambda="exp(-0.5+0.15*fin.measure+0.4*personal)")


writeNetworkModel(mynet,pretty=TRUE)
plot(mynet)


evidence=NULL

trackedVars<-c("income","age","trans1","trans2","trans3")

compiledNet<-compileJagsModel(mynet,data=evidence,n.chain=3,n.adapt=5000)
post<-HydeSim(compiledNet,variable.names =trackedVars,n.iter=1000)
dplyr::sample_n(post,10)


# estimate the model 
# fix two parameters to help with identification
# with latent factor models, one needs to be careful with priors
# as the model is not identifiable by definition if restrictions are not imposed 


library(rjags)
set.seed=1234

mydata<-list(age=post$age,income=post$income,p=3,n=3000,
             y=cbind(post$trans1,post$trans2,post$trans3))


model= "model 
{ 
for (i in 1:n) {
# factor scores and unstructured residuals 
               F[i] ~ dnorm(mu.F[i],tau.F)
               mu.F[i] <- b[1]*income[i]+b[2]*age[i]
               u[i] ~ dnorm(0,tau.u) 
               for (j in 1:3) {y[i,j] ~ dpois(mu[i,j]);          
                               log(mu[i,j]) <- alpha[j]+lambda[j]*F[i]+kappa[j]*u[i]
               }
}

# Priors
tau.F ~ dgamma(1,0.01)
tau.u ~ dgamma(1,0.01)
for (k in 1:2) {b[k] ~ dnorm(0,0.001)}
for (k in 1:3)    {alpha[k] ~ dnorm(0,0.001)} 

lambda[1] <- 0.1; 
lambda[2]<-lam[1];
lambda[3]<-lam[2];
kappa[1]<-kap[1];
kappa[2]<-0.3
kappa[3]<-kap[2]
for (k in 1:2){lam[k]~dnorm(0,0.001)
               kap[k]~dnorm(0,0.001)}
}

"

# Initial Values and Estimation

inits1 <- list(b=rep(0,2),alpha=rep(0,3),tau.F=1,tau.u=1,lam=rep(0.01,2),
               kap=rep(0,2),F=rep(0,3000))

inits=list(inits1)
mod=jags.model(textConnection(model),data=mydata,n.chains=2,inits=inits1) 
update(mod,500) 
pars <- c("b","lambda","kappa","alpha")
mod_sim=coda.samples(model=mod,variable.names=pars,n.iter=50000,seed=8899) 
summary(mod_sim)
 