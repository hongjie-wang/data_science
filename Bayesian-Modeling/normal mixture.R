# an example of a simple mixture 
# we practically fixed the gaussian and focus on the proportion
library(R2jags)
set.seed(123)
N <- 100
group <- rbinom(N,1,.3)
y <- rnorm(N,mean=2+group,sd=.3)

plot(density(y))

mix.model <-"model {
  for (i in 1:length(y)) {
    y[i] ~ dnorm(mu[group[i]+1],tau)
    group[i] ~ dbern(p)
  }
  
  mu0[1]~dnorm(2,100)
  mu0[2]~dnorm(3,100)
  p ~ dbeta(1,1)
  mu[1:2] <- sort(mu0)
  sd~dnorm(0.3,2000)
  tau<-1/(sd*sd)
}"

inits<-function(){
  list(mu0=rnorm(2,0,100))
}

model.data<-list(y=y)

params<-c("p","sd","mu")

mix.jags<-jags(model=textConnection(mix.model),
              inits=inits,
              data=model.data,
              parameters=params)
print(mix.jags)
              
