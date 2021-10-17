library(rjags)
setwd("C:/Users/hongjie/Desktop/study/BHMRA-2019-DATASETS")
attach("DS_9_3.Rdata")
set.seed=1234


model= "model 
{ for (i in 1:n) {
    # factor scores and unstructured residuals 
F[i] ~ dnorm(mu.F[i],tau.F)
mu.F[i] <- b[1]*unem[i]+b[2]*border[i]+b[3]*gdp[i] +b[4]*urb[i]
u[i] ~ dnorm(0,tau.u) 
for (j in 1:p) {y[i,j] ~ dpois(mu[i,j]);          
                mu[i,j] <- pop[i]*rho[i,j]; 
              log(rho[i,j]) <- alpha[j]+lambda[j]*F[i]+kappa[j]*u[i]
}
}
# Priors
tau.F ~ dgamma(1,0.01)
tau.u ~ dgamma(1,0.01)
for (k in 1:4) {b[k] ~ dnorm(0,0.001)}
kappa[1] <- kap[1]
kappa[2] <- 1; 
kappa[3] <- kap[2]
kappa[4] <- kap[3]
lambda[1] <- 1; 
for (k in 2:p) {lambda[k] <- lam[k-1] }
for (k in 1:pm) {lam[k]~ dnorm(0,1)
                 kap[k]~ dnorm(0,1) }
for (k in 1:p)    {alpha[k] ~ dnorm(0,0.001); 
tau[k] ~ dgamma(1,0.001)}  
} "

# Initial Values and Estimation

inits1 <- list(b=rep(0,4),alpha=rep(0,4),tau.F=1,tau.u=1,lam=rep(1,3),
               kap=rep(1,3),F=rep(0,49))
inits2 <- list(b=rep(0.25,4),alpha=rep(0.5,4),tau.F=10,tau.u=10,lam=rep(1,3),
               kap=rep(1,3),F=rep(0.1,49))
inits=list(inits1,inits2)
mod=jags.model(textConnection(model),data=DS_9_3,n.chains=2,inits=inits) 
update(mod,500) 
pars <- c("b","lambda","kappa","F")
mod_sim=coda.samples(model=mod,variable.names=pars,n.iter=10000,seed=8899) 
summary(mod_sim)