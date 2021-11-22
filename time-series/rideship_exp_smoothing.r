
library(tidyverse)
library(Ecdat)
data(Crime)

y<-subset(Crime,year==81)[["prbarr"]]

library(runjags)

X.model <-'model {
  for (i in 1:length(y)) {
    y[i] ~ dnorm(mu,tau)
  }
  mu ~ dnorm (0,.0001)
  tau ~ dgamma(.01,.01)
  sd <- sqrt(1/tau)
}' 
X.jags<-run.jags(model=X.model,data=list(y=y),monitor=c("mu","sd"))


summary(X.jags)




xls_example <- readxl_example("c:/users/hongjie/documents/data_science/time-series/data/amtrk.xls")

Amtrak.data <- read.csv("Amtrak data.csv")
ridership.ts <- ts(Amtrak.data$Ridership, start = c(1991,1), end = c(2004, 3), freq = 12)
plot(ridership.ts, xlab = "Time", ylab = "Ridership", ylim = c(1300, 2300), bty = "l")