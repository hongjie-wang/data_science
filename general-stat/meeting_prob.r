
# A and B will arrive at starbucks between 7:00 and 8:00. Each will wait 10 mins
# what is the prob they will meet?
# we solve by simulation
library(tidyverse)
library(ggplot2)
library(rsample)

n <- 100000
set.seed(1234)


sim_data<-data.frame(A=runif(n,0,60),
           B=runif(n,0,60))%>%
        mutate(result = as.integer(ifelse(abs(A-B)<=10,
                       1, 0)))
summary(sim_data$result)

# it is 30.67%, what is the confidence interval

# approach one bayesian 
# assume f(p)~uniform=beta(1,1), what is f(p|n,y)
# in general prior beta(a,b), then f(p|n,y)~beta(a+y,b+n-y)

y<-sum(sim_data$result)

print(c(qbeta(p = 0.05, shape1 = 1+y, shape2 = 1+n-y),
qbeta(p = 0.95, shape1 = 1+y, shape2 = 1+n-y)))


# approach two exact binomial test
# idea: what is the smallest p such that the observed rate is not likely?
# what is the biggest p such that... this forms the confidence interval
binom.test(y, n)


# other approachs 1) use CLT normal approximiation
# boostrapping 