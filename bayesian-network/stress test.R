# model stress test
#example 13.6 (page 455) from Risk Assessment and Decision Analysis with Bayesian Networks
library(HydeNet)
 net<-HydeNetwork(~market+trade+market.trade
                  +n1.market|market+n2.market|market
                  +n1.trade|trade+n2.trade|trade
                  +n1.market.trade|market.trade+n2.market.trade|market.trade
                  +market.loss|n1.market*market*n2.market
                  +joint.loss|n1.market.trade*market.trade*n2.market.trade
                  +trade.loss|n1.trade*trade*n2.trade
                  +total.loss|trade.loss*joint.loss*market.loss)
 plot(net)

 net<-setNode(net,market,nodeType="dbern",p=0.1)
 net<-setNode(net,trade,nodeType="dbern",p=0.03)
 net<-setNode(net,market.trade,nodeType="dbern",p=0.003)
 
 net<-setNode(net,n1.market,nodeType="dnorm",mu=10,tau=1/100)
 net<-setNode(net,n2.market,nodeType="dnorm",mu=500,tau=1/10000)
 
 net<-setNode(net,n1.trade,nodeType="dnorm",mu=0,tau=100000)
 net<-setNode(net,n2.trade,nodeType="dnorm",mu=250,tau=1/10000)
 
 net<-setNode(net,n1.market.trade,nodeType="dnorm",mu=0,tau=100000)
 net<-setNode(net,n2.market.trade,nodeType="dnorm",mu=750,tau=1/10000)
 
 net<-setNode(net,market.loss,"determ",define=fromFormula(),
              nodeFormula = market.loss~ifelse((market==0),n1.market,n2.market))
 net<-setNode(net,trade.loss,"determ",define=fromFormula(),
              nodeFormula = trade.loss~ifelse((trade==0),n1.trade,n2.trade))
 net<-setNode(net,joint.loss,"determ",define=fromFormula(),
              nodeFormula = joint.loss~ifelse((market.trade==0),n1.market.trade,n2.market.trade))
 
 net<-setNode(net,total.loss,"determ",define=fromFormula(),
              nodeFormula = total.loss~market.loss+joint.loss+trade.loss)
 
 writeNetworkModel(net,pretty=TRUE)
 
 
 trackedVars<-c("market.loss","trade.loss","joint.loss","total.loss")
 evidence=NULL
 compiledNet<-compileJagsModel(net,data=evidence,n.chain=3,n.adapt=5000)
 post<-HydeSim(compiledNet,variable.names =trackedVars,n.iter=10000)
 dplyr::sample_n(post,10)
 
plot(density(post$total.loss))

quantile(post$market.loss,0.995)
quantile(post$trade.loss,0.995)
quantile(post$total.loss,0.995)