rm(list = ls())
library(HydeNet)
net<-HydeNetwork(~+condition+inspect+report|condition*inspect+U1|inspect+buy|report+U2|condition*buy)

net<-setNode(net,node=condition,nodeType ="dbern",p=0.7)

net<-setNode(net,node=inspect,nodeType ="dbern",p=0.5)

# use this approch to enter conditional prob direclty

cp.report=data.frame(inspect=as.factor(c(1,1,1,1,1,1,0,0,0,0,0,0)),
               condition=as.factor(c(1,1,1,0,0,0,1,1,1,0,0,0)),
               report=as.factor(c("g","b","u","g","b","u",
                                  "g","b","u",
                                  "g","b","u")),
               
               prob=c(0.95,0.05,0,0.1,0.9,0,0,0,1,0,0,1))

report.h=cpt(report~inspect+condition,data=cp.report,wt=cp.report$prob)

net<-setNodeModels(net,report.h)

net<-setNode(net,U1, nodeType = "determ",define=fromFormula(),
             nodeFormula = U1~ifelse(inspect==1,-600,0))

#net<-setNode(net,node=buy,nodeType ="dbern",p=0.5)

net<-setNode(net,buy,nodeType="determ",define=fromFormula(),
             nodeFormula = buy ~ifelse(report=="b",0,1))

net<-setNode(net,U2, nodeType = "determ",define=fromFormula(),
             nodeFormula = U2~ifelse(buy==1 && condition==1,5000,
                                     ifelse(buy==1 && condition==0,-3000,0)))




net<-setDecisionNodes(net,inspect)
net<-setUtilityNodes(net,U1)
net<-setUtilityNodes(net,U2)

plot(net)


writeNetworkModel(net,pretty=TRUE)

evidence=NULL

trackedVars<-c("inspect","condition","report","U1","buy","U2")


compiledNet<-compileJagsModel(net,data=evidence,n.chain=3,n.adapt=5000)
post<-HydeSim(compiledNet,variable.names =trackedVars,n.iter=10000)
dplyr::sample_n(post,10)

