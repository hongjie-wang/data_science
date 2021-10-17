# Hongjie Wang 6-09-2020
#implement the approach in paper "the economic selection of sample sizes for list testing"
# by Phillip Pfeifer
#situation: list of 50K names, mailing cost $1 and value per responder $50
# the break-even rate 0.02%. We desire to test the list
# what is the optimal number of names to test?

 
library(HydeNet)
net<-HydeNetwork(~prior.resprate+test.size+test.resp|prior.resprate*test.size
                 +rollout.rate|test.size*test.resp+
                 rollout|test.resp*test.size
                 +rollout.resp|test.size*rollout.rate
                 +profit|rollout*test.size*test.resp*rollout.resp)

# prior from beta(r*n,(1-r)*n) where r is the response rate, n is the number of records
# this paramiterization is very useful as we can use historical campaign to set up the prior

net<-setNode(net, prior.resprate, nodeType="dbeta",a=0.02*200,b=(1-0.02)*200)

# we set test size a random poission, but it is the decision variale
net<-setNode(net,test.size,nodeType ="dpois",lambda=2000)

# # of responders from test follows binomial

net<-setNode(net,test.resp,nodeType ="dbin",prob="prior.resprate",size="test.size")

# we get the posterior beta update based on test outcome
# notic, we can use use the support feature in this case

net<-setNode(net,rollout.rate, nodeType="dbeta",a="4+test.resp",
                                                b="196+test.size-test.resp")

# we assume rational person risk neural, in other words
# we will roll out if the expected rate from the updated beta is above cutoff 2%
# notice, we did not use rollout.rate>0.02, since this would be just a random draw
# not the mean of the updated beta.
net<-setNode(net,rollout,nodeType = "determ",define=fromFormula(),
            nodeFormula = 
              rollout~ifelse((4+test.resp)/(200+test.size)>=0.02,1,0))

# we use the updated beta to generate probability for binomial
net<-setNode(net,rollout.resp, nodeType ="dbin",prob="rollout.rate",size="50000-test.size") 

# if we roll out, we pay both test and roll out, as well as the units from both of them
# otherwise, just from test campaign

net<-setNode(net,profit,nodeType="determ",define=fromFormula(),
             nodeFormula = profit~ifelse((rollout==1),(test.resp+rollout.resp)*50-50000*1,
                                         test.resp*50-test.size*1))

net<-setDecisionNodes(net,test.size)
net<-setUtilityNodes(net,profit)
plot(net)
 
writeNetworkModel(net,pretty=TRUE)
 
trackedVars<-c("prior.resprate","test.size","test.resp","rollout.rate","rollout","profit","rollout.resp")
 
evidence=NULL
 compiledNet<-compileJagsModel(net,data=evidence,n.chain=3,n.adapt=50000)
 post<-HydeSim(compiledNet,variable.names =trackedVars,n.iter=100000)
 dplyr::sample_n(post,10)
 
plot(density(post$profit))
mean(post$profit)

history=data.frame()

for (i in seq(200,10000,100)){
  evidence=list(test.size=i)
  compiledNet<-compileJagsModel(net,data=evidence,n.chain=3,n.adapt=5000)
  post<-HydeSim(compiledNet,variable.names =trackedVars,n.iter=10000)
  results=cbind(test.size=i,expect.profit=mean(post$profit))
  history=rbind(history,results)
}

plot(history[,1],history[,2])

 