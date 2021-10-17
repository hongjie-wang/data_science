
rm(list = ls())
library(HydeNet)

net<-HydeNetwork(~prob.defect+test+defect.found|prob.defect*test)

net<-setNode(net, prob.defect, nodeType="dbeta",a=2,b=98)

net<-setNode(net,test,"determ",define=fromFormula(),
             nodeFormula = test~1000)

plot(net)
net<-setNode(net,defect.found,nodeType ="dbin",prob="prob.defect",size="test")

evidence=NULL

trackedVars<-c("defect.found","prob.defect","test")
compiledNet<-compileJagsModel(net,data=evidence,n.chain=3,n.adapt=5000)
post<-HydeSim(compiledNet,variable.names =trackedVars,n.iter=10000)
dplyr::sample_n(post,10)

summary(post$prob.defect)
plot(density(post$prob.defect))

# suppose we have some data we tested 1000 and find 100 defects 

evidence=list(defect.found=100)

trackedVars<-c("defect.found","prob.defect","test")
compiledNet<-compileJagsModel(net,data=evidence,n.chain=3,n.adapt=5000)
post<-HydeSim(compiledNet,variable.names =trackedVars,n.iter=10000)
dplyr::sample_n(post,10)

summary(post$prob.defect)
plot(density(post$prob.defect))
