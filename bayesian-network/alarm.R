#Example statement: You have a new burglar alarm installed. It reliably detects
#burglary, but also responds to minor earthquakes. Two neighbors, John and Mary,
#promise to call the police when they hear the alarm. John always calls when he
#hears the alarm, but sometimes confuses the alarm with the phone ringing and calls
#then also. On the other hand, Mary likes loud music and sometimes doesn???t hear the
#alarm. Given evidence about who has and hasn???t called, you???d like to estimate the
#probability of a burglary (from Pearl (1988)).

rm(list = ls())
library(HydeNet)

net<-HydeNetwork(~burglary+
                   earthquake+
                   alarm|burglary*earthquake
                   +alarm2|alarm+john|alarm2+mary|alarm2)

# 1% chance of burglary (that is output 1)
net<-setNode(net,burglary,nodeType="dbern",p=0.01)

# 2% chance of earthquake
net<-setNode(net,earthquake,nodeType="dbern",p=0.02)


# use conditional table to populate alarm node
# this is a useful approach 
# draw back is everything has to be factor
cp1=data.frame(burglary=as.factor(c(1,1,1,1,0,0,0,0)),
               earthquake=as.factor(c(1,1,0,0,1,1,0,0)),
               alarm=as.factor(c("T","F","T","F","T","F","T","F")),
               prob=c(0.95,0.05,0.94,0.06,0.29,0.71,0.001,0.999))

alarm_h=cpt(alarm~burglary+earthquake,data=cp1,wt=cp1$prob)

net<-setNodeModels(net,alarm_h)

# we create a node to get rid of the factor problem
# notice even if we use 0/1 in alarm, it is still a factor
# and create problems later when I use it as inputs for another node

net<-setNode(net,alarm2,"determ",define=fromFormula(),
             nodeFormula = alarm2~ifelse(alarm=="T",1,0))

# another very useful approach
# convert the binary probs into logit 
# john will call 90% if alarm and 5% call witout alarm by mistake

net<-setNode(net,john,nodeType="dbern",prob="ilogit(-2.944439+5.116*alarm2)")


#mary will call only 70% if alarm, but makes only 1% mistake for false alarm call

net<-setNode(net,mary,nodeType="dbern",prob="ilogit(-4.59512+5.442418*alarm2)")

evidence=NULL

trackedVars<-c("alarm","earthquake","burglary","alarm2","john","mary")
compiledNet<-compileJagsModel(net,data=evidence,n.chain=3,n.adapt=5000)
post<-HydeSim(compiledNet,variable.names =trackedVars,n.iter=10000)
dplyr::sample_n(post,10)

evidence=list(john=1,mary=0)

trackedVars<-c("alarm","earthquake","burglary","alarm2","john","mary")
compiledNet<-compileJagsModel(net,data=evidence,n.chain=3,n.adapt=5000)
post<-HydeSim(compiledNet,variable.names =trackedVars,n.iter=10000)
summary(post$earthquake)
summary(post$burglary)

evidence=list(john=0,mary=1)

trackedVars<-c("alarm","earthquake","burglary","alarm2","john","mary")
compiledNet<-compileJagsModel(net,data=evidence,n.chain=3,n.adapt=5000)
post<-HydeSim(compiledNet,variable.names =trackedVars,n.iter=10000)
summary(post$earthquake)
summary(post$burglary)

evidence=list(john=1,mary=1)

trackedVars<-c("alarm","earthquake","burglary","alarm2","john","mary")
compiledNet<-compileJagsModel(net,data=evidence,n.chain=3,n.adapt=5000)
post<-HydeSim(compiledNet,variable.names =trackedVars,n.iter=10000)
summary(post$earthquake)
summary(post$burglary)









