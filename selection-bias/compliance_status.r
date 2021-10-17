
library(tidyverse)
nbr<-100000
status<-c("compliers","never takers","always takers","defiers")
mydata<-data.frame(status=sample(status,nbr,replace=TRUE,prob=c(0.2,0.3,0.5,0)))
mydata<-mydata%>%
  mutate(status=as.factor(status),
         xbeta.base=-0.5+0.4*(status=="compliers")+0.2*(status=="never takers")
         +0.1*(status=="always takers"),
         prob.base=exp(xbeta.base)/(1+exp(xbeta.base)),
         potential.y0=rbinom(nbr,1,prob.base),
         prob=prob.base+0.05*(status=="compliers")+
                       0.15*(status=="never takers")+
                       0*(status=="always takers"),
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
                      ifelse(status=="never takers",0,intent)),
         y.obs=ifelse(treat==1,potential.y1,potential.y0))

mydata%>%group_by(status,intent,treat)%>%summarize(counts=n())


mydata%>%group_by(intent)%>%summarize(treat_rate=mean(treat), y.obs=mean(y.obs))

                                      