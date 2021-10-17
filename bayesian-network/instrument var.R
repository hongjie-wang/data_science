
rm(list = ls())

library(HydeNet)
net<-HydeNetwork(~Z+latent+T|Z*latent+y|T*latent)

net<-setNode(net,Z,nodeType ="dbern",prob=0.5)

net<-setNode(net,latent,nodeType ="dnorm",mu=10,tau=10)

net<-setNode(net,T,nodeType = "dnorm",mean=fromFormula(),tau=1,
             nodeFormula = T~10+2.5*Z-6*latent)

net<-setNode(net,y,nodeType = "dnorm",mean=fromFormula(),tau=1,
             nodeFormula = y~3+10*T+30*latent)

plot(net)
writeNetworkModel(net,pretty=TRUE)
 
 
trackedVars<-c("latent","Z","T","latent","y")
 
evidence=NULL
compiledNet<-compileJagsModel(net,data=evidence,n.chain=3,n.adapt=5000)
post<-HydeSim(compiledNet,variable.names =trackedVars,n.iter=10000)
dplyr::sample_n(post,10)
 
lm(y~T+latent,data=post)

lm(y~T,data=post)



 