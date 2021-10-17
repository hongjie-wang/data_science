# perform bayesian conformatory factor analysis
# HW 1-15-2021

rm(list = ls())

library(MASS)
#step one, simulate data 
# two factors (correlated) and 5 indicators
# y1,y2 loaded on f1 and y3,y4 loaded on f2, y5 irrelevant 

Sigma <- matrix(c(1,0.5,0.5,1),2,2)
f<-mvrnorm(n=200, rep(0, 2), Sigma)

y1=1.5+1.2*f[,1]+0*f[,2]+rnorm(200,0,1.5)
y2=2.1+0.87*f[,1]+0*f[,2]+rnorm(200,0,1)
y3=0.5+0*f[,1]+0.65*f[,2]+rnorm(200,0,2.1)
y4=0.7+0*f[,1]+0.78*f[,2]+rnorm(200,0,1.7)
y5=1.2+0.0001*f[,1]+0*f[,2]+rnorm(200,0,1.3)

mydata<-list(y=cbind(y1,y2,y3,y4,y5),p=5,q=2,n=200)

#step two bayesian modeling
# suppose we have some prior knowledge such that
# y1,y2 are loaded on f1, y3,y4,y5 are on f2
# we fit the following model y_i=a_i + gamma_i1*lambda_i1*f1+gamma_i2*lambda_i2*f2+error_i
# here (gamma_i1,gamma_i2)~bernoulli (pi_i)
# pi_i is a latent prob if indicator i is relevant or not.
# from that, we draw two indepenent binary variables and then combine (product)
# to get realized loading on factors

require(rjags)
require(loo)


model= "model {for (j in 1:p) {s[j]~dunif(0,100)
a[j] ~ dnorm(0,0.001)}
# priors for loadings on factor 1
# priors on main loadings
for (j in 1:2){lam1[j] ~ dnorm(0,1) T(0,)}
# informative priors for cross loadings concentrated at zero
for (j in 3:p){lam1[j] ~ dnorm(0,100)}
# priors for loadings on factor 2
# informative priors for cross loadings concentrated at zero
for (j in 1:2){lam2[j] ~ dnorm(0,100)}
# priors on main loadings
for (j in 3:p) {lam2[j] ~ dnorm(0,1) T(0,)}
# realised loadings
for (j in 1:p){lam1.r[j] <- lam1[j]*gamma[j,1] 
lam2.r[j] <- lam2[j]*gamma[j,2] }
# selection indicators
for (j in 1:p) {pi.gamma[j] ~ dbeta(1,1)}
for (k in 1:q) {for (j in 1:p) { gamma[j,k] ~ dbern(pi.gamma[j])}}
# prior for correlated factors 
for (j in 1:q) { B[j] <- 0
for (k in 1:q) {Q[j,k] <- equals(j,k)}}
inv.Phi[1:2,1:2] ~ dwish(Q[,],2)
Phi[1:2,1:2] <- inverse(inv.Phi[,])
cor.F <- Phi[1,2]/sqrt(Phi[1,1]*Phi[2,2])
for (i in 1:n) { F[i,1:2] ~ dmnorm(B[],inv.Phi[,])
# Likelihood
for (j in 1:p){ y[i,j] ~ dnorm(mu[i,j], 1/(s[j]*s[j]));
mu[i,j] <- a[j]+gamma[j,1]*lam1[j]*F[i,1] + gamma[j,2]*lam2[j]*F[i,2]}}} "

# Initial Values and Estimation
F0 <- matrix(0,200,2)
inits1 <- list(s=rep(1,5),lam1=rep(1,5), lam2=rep(1,5),a=rep(5,0),F=F0)
inits2 <- list(s=rep(2,5),lam1=rep(0.8,5), lam2=rep(0.8,5),a=rep(5,0),F=F0)
inits=list(inits1,inits2)
mod=jags.model(textConnection(model),data=mydata,n.chains=2,inits=inits) 
update(mod,500) 
pars=c("a","lam1.r","lam2.r","gamma","pi.gamma","cor.F") 
mod_sim=coda.samples(model=mod,variable.names=pars,n.iter=100000,seed=8899) 
summary(mod_sim)