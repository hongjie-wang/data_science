rm(list = ls())


# one side noncompliace makes always takers a complier
set.seed(12345)

library(tidyverse)
library(GJRM)
nbr<-400000
status<-c("compliers","never takers","always takers","defiers")
mydata<-data.frame(status=sample(status,nbr,replace=TRUE,prob=c(0.4,0.2,0.4,0)))

mydata<-mydata%>%
  mutate(status=as.factor(status),
         xbeta.base=-0.5+0.4*(status=="compliers")+0.2*(status=="never takers")
         +0.1*(status=="always takers"),
         prob.base=exp(xbeta.base)/(1+exp(xbeta.base)),
         potential.y0=rbinom(nbr,1,prob.base),
         prob=prob.base+0.05*(status=="compliers")+
                       0.15*(status=="never takers")+
                       0.25*(status=="always takers"),
         potential.y1=rbinom(nbr,1,prob))%>%
  select(-xbeta.base)

mydata%>%group_by(status)%>%
  summarize(meanp0=mean(prob.base),
            meanp1=mean(prob),
            diffp=meanp1-meanp0,
            meany0=mean(potential.y0),
            meany1=mean(potential.y1),
            diffy=meany1-meany0)



mydata<-mydata%>%
  mutate(intent=rbinom(nbr,1,0.5),
         treat=ifelse(status=="always takers",1,
                      ifelse(status=="never takers",0,intent)))
mydata<-mydata%>%
     mutate(treat=pmin(intent,treat),
         y.obs=ifelse(treat==1,potential.y1,potential.y0))

 
mydata%>%group_by(status,intent,treat,
                  potential.y0,potential.y1,y.obs)%>%
  summarize(counts=n())


mydata%>%group_by(status,intent)%>%
  summarize(treat_rate=mean(treat), y.obs=mean(y.obs))%>%
  pivot_wider(id_cols = status, 
              names_from = intent, 
              values_from = c("treat_rate", "y.obs"))


mydata%>%group_by(intent)%>%summarize(treat_rate=mean(treat), y.obs=mean(y.obs))

mydata<-
mydata%>%
  mutate(y.obs=as.factor(y.obs),
         treat=as.factor(treat),
         intent=as.factor(intent))

out <- gjrm(list(treat ~ intent,
                 y.obs ~ treat ),
            data = mydata,
            margins = c("probit", "probit"),
            Model = "B")
conv.check(out)
summary(out)

#COPULA:   Gaussian
#MARGIN 1: Bernoulli
#MARGIN 2: Bernoulli

#EQUATION 1
#Link function for mu.1: probit 
#Formula: treat ~ intent

#Parametric coefficients:
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept)   -8.211   5792.619  -0.001    0.999
#intent1        9.053   5792.619   0.002    0.999


#EQUATION 2
#Link function for mu.2: probit 
#Formula: y.obs ~ treat

#Parametric coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -0.162898   0.002816  -57.85   <2e-16 ***
#  treat1       0.381063   0.004989   76.38   <2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


#n = 400000  theta = 0.0178(0.0105,0.0271)  tau = 0.0113(0.0067,0.0173)
#total edf = 5

                                      