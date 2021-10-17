# simulating of instument variable 
# T is the treatment var (binary, imagine product adoption)
# Z is instrument var (imagine we ran a randomization campaign to promote this product)
# others is another instrument that we do NOT know and did not include

rm(list = ls())

library(HydeNet)
net<-HydeNetwork(~Z+others+latent+T|Z*latent*others+y|T*latent)

net<-setNode(net,Z,nodeType ="dbern",prob=0.5)

net<-setNode(net,latent,nodeType ="dnorm",mu=0,tau=1)

net<-setNode(net,others,nodeType ="dnorm",mu=0,tau=1)

net<-setNode(net,T,nodeType = "dbern",prob = "ilogit(0+2*Z-5*latent+1*others)")

net<-setNode(net,y,nodeType = "dnorm",mean=fromFormula(),tau=1,
             nodeFormula = y~3+10*T+30*latent)

plot(net)
writeNetworkModel(net,pretty=TRUE)

 
trackedVars<-c("latent","Z","T","latent","y","others")
 
evidence=NULL
compiledNet<-compileJagsModel(net,data=evidence,n.chain=3,n.adapt=5000)
post<-HydeSim(compiledNet,variable.names =trackedVars,n.iter=10000)
dplyr::sample_n(post,10)

#verify
summary(glm(T~Z+others+latent,data=post))
 
# correct model 
lm(y~T+latent,data=post)

#inconsistent estimator
lm(y~T,data=post)

# in this case, we have a strong instrument, but not all are included
# notice we have some bias, but helped a lot. 
stage1<-glm(T~Z,data=post,family="binomial")
post$T.hat <- predict(stage1, newdata =post, type = "response")

lm(y~T.hat,data=post)





 