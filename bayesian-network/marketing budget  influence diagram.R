 library(HydeNet)
 net<-HydeNetwork(~budget+price|budget+units|budget*price+U1|price*units
              +cost|units+U2|cost+U3|U1*U2)

 net<-setNode(net,budget,nodeType = "dnorm",mu=100,tau=1/400)
 
 net<-setNode(net,units,nodeType = "dnorm",mu="20+0.2*budget-0.1*price",tau=1/25)
 
 net<-setNode(net, cost,nodeType="dnorm",mu="400+10*units",tau=1/2500)
 
 net<-setNode(net, U2,nodeType ="determ",define=fromFormula(),
              nodeFormula = U2~-cost)
 
 net<-setNode(net, U1,nodeType ="determ",define=fromFormula(),
              nodeFormula = U1~price*units)
 
 net<-setNode(net, U3,nodeType ="determ",define=fromFormula(),
              nodeFormula = U3~U1+U2)
 
 net<-setNode(net,price,nodeType = "dunif",a=50,b=100)
 
 net<-setDecisionNodes(net,price)
 net<-setUtilityNodes(net,U2)
 net<-setUtilityNodes(net,U1)
 net<-setUtilityNodes(net,U3)
 
 
 plot(net)
 
 trackedVars<-c("budget","price","units","cost","U1","U2","U3")
 compiledNet<-compileJagsModel(net,data=evidence,n.chain=3,n.adapt=5000)
 post<-HydeSim(compiledNet,variable.names =trackedVars,n.iter=10000)
 dplyr::sample_n(post,10)
 
 history=data.frame()
 
 for (budget.amt in seq(50,200,50)){
   evidence=list(budget=budget.amt)
   
   policy<-data.frame(price=seq(50,500,50))
   
   compiledNet<-compileDecisionModel(net,policyMatrix = policy, data=evidence)
   
   samples<-lapply(compiledNet,HydeSim,variable.names=trackedVars,n.iter=10000)
   
   budget<-lapply(samples,function(l) mean(l$budget))
   
   price<-lapply(samples,function(l) mean(l$price))
   
   profit<-lapply(samples,function(l) mean(l$U3))
   
   results=data.frame(cbind(budget,price,profit))
   
   history=rbind(history,results)
 }
 
 
 
 