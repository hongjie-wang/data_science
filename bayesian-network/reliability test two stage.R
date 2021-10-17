# model simple reliability
#example 14.3 (page 465) from Risk Assessment and Decision Analysis with Bayesian Networks
# given what we have seen, what is the prob system will not fail at all in the next n samples

library(HydeNet)
net<-HydeNetwork(~prob.failure+test.cases1+defects1|prob.failure*test.cases1+
                   test.cases2+defects2|test.cases2*prob.failure 
                 +reliable2|defects2)
plot(net)

net<-setNode(net, prob.failure, nodeType="dbeta",a=1,b=1)

net<-setNode(net,test.cases1,nodeType ="dpois",lambda=5000000)

net<-setNode(net,defects1,nodeType ="dbin",prob="prob.failure",size="test.cases1")

net<-setNode(net,test.cases2,nodeType ="dpois",lambda=5000000)

net<-setNode(net,defects2,nodeType ="dbin",prob="prob.failure",size="test.cases2")


net<-setNode(net,reliable2,nodeType="determ",define=fromFormula(),
             nodeFormula=reliable2~ifelse((defects2<=0),1,0))
 
 writeNetworkModel(net,pretty=TRUE)
 
 # first,classic problem by Litttlewood and Strigini
 # suppose we have no defect in first 10E6 cases
 # what is the prob of no defect in the next 10E6 cases?
 # the answer is surpringly 0.5
 # this is due to the fact we want to see no defect (airline)
 
 trackedVars<-c("prob.failure","reliable2")
 
evidence=list(test.cases1=1000000,defects1=0,test.cases2=1000000)
 compiledNet<-compileJagsModel(net,data=evidence,n.chain=3,n.adapt=5000)
 post<-HydeSim(compiledNet,variable.names =trackedVars,n.iter=10000)
 dplyr::sample_n(post,10)
 
plot(density(post$prob.failure))
mean(post$reliable2)

#another very interesting results due to Littlewood and Strigini
#in order for us to hve 90% confidence that we will have no defect in the next n
# we would need to see no defect in 10*n previously (in stage 1)

trackedVars<-c("prob.failure","reliable2")

evidence=list(test.cases1=1000,defects1=0,test.cases2=100)
compiledNet<-compileJagsModel(net,data=evidence,n.chain=3,n.adapt=5000)
post<-HydeSim(compiledNet,variable.names =trackedVars,n.iter=10000)
dplyr::sample_n(post,10)

plot(density(post$prob.failure))
mean(post$reliable2)

