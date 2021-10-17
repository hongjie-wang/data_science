#airline set up how many seats they will allow to book
# they have an estimate of prob of showing
# customers only pay when they show up.
# ticket $1, if someone bumped, ticket free
# what is the optimal nbr of booking allowed 
 
library(HydeNet)
net<-HydeNetwork(~nbr.seats+prob.show+nbr.book+
                   nbr.showed|prob.show*nbr.book+
                   profit|nbr.seats*nbr.showed)

net<-setNode(net,nbr.seats,nodeType = "determ",define=fromFormula(),
             nodeFormula =nbr.seats~30)

net<-setNode(net,prob.show,nodeType = "determ",define=fromFormula(),
             nodeFormula =prob.show~0.6)

net<-setNode(net,nbr.book,nodeType ="dpois",lambda=40)

net<-setNode(net,nbr.showed,nodeType ="dbin",prob="prob.show",size="nbr.book")

net<-setNode(net,profit,nodeType = "determ",define=fromFormula(),
             nodeFormula = profit~ifelse(nbr.seats>=nbr.showed,nbr.showed,nbr.seats-(nbr.showed-nbr.seats)))


net<-setDecisionNodes(net,nbr.book)

net<-setUtilityNodes(net,profit)

plot(net)

writeNetworkModel(net,pretty=TRUE)
 
trackedVars<-c("prob.show","nbr.book","nbr.seats","nbr.showed","profit")
 
evidence=NULL
 compiledNet<-compileJagsModel(net,data=evidence,n.chain=3,n.adapt=50000)
 post<-HydeSim(compiledNet,variable.names =trackedVars,n.iter=100000)
 dplyr::sample_n(post,20)
 
plot(density(post$profit))
mean(post$profit)

history=data.frame()

for (i in seq(30,100,1)){
  evidence=list(nbr.book=i)
  compiledNet<-compileJagsModel(net,data=evidence,n.chain=3,n.adapt=5000)
  post<-HydeSim(compiledNet,variable.names =trackedVars,n.iter=10000)
  results=cbind(nbr.book=i,expect.profit=mean(post$profit))
  history=rbind(history,results)
}

plot(history$nbr.book,history$expect.profit)

 