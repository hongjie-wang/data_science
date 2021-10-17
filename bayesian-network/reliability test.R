# model simple reliability
#example 14.1 and 14.2 (page 464) from Risk Assessment and Decision Analysis with Bayesian Networks

library(HydeNet)
net<-HydeNetwork(~prob.failure+test.cases+defects|prob.failure*test.cases+reliable|prob.failure)
plot(net)

net<-setNode(net, prob.failure, nodeType="dbeta",a=1,b=1)

net<-setNode(net,test.cases,nodeType ="dpois",lambda=100)
net<-setNode(net,defects,nodeType ="dbin",prob="prob.failure",size="test.cases")
net<-setNode(net,reliable,nodeType="determ",define=fromFormula(),
             nodeFormula=reliable~ifelse((prob.failure<0.01),1,0))
 
 writeNetworkModel(net,pretty=TRUE)
 
 
 trackedVars<-c("test.cases","prob.failure","defects","reliable")
# if we observe 2 defects in 100 sample, the prob that 
# prob failure is less than 0.01 is 8%
 
evidence=list(test.cases=100,defects=2)
 compiledNet<-compileJagsModel(net,data=evidence,n.chain=3,n.adapt=5000)
 post<-HydeSim(compiledNet,variable.names =trackedVars,n.iter=10000)
 dplyr::sample_n(post,10)
 
plot(density(post$prob.failure))
mean(post$reliable)

# we address another very common problem
# suppose we sample and do not see any defect, how many samples we should do
# to make sure prob failure is less than 0.01 with 90% confidence. 

sample.size=numeric()
confidence=numeric()
j=0
for (i in seq(100,300,10)){
  evidence=list(test.cases=i,defects=0)
  j=j+1
  sample.size[j]=i
  compiledNet<-compileJagsModel(net,data=evidence,n.chain=3,n.adapt=5000)
  post<-HydeSim(compiledNet,variable.names =trackedVars,n.iter=10000)
  confidence[j]<-mean(post$reliable)
}


# according to this analysis, it takes roughly 230 cases
plot(sample.size,confidence)