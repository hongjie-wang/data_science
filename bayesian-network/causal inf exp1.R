
rm(list=ls(all=TRUE))

library(HydeNet)

# simple serial network z->x->y
# we want to draw causal inference of x on y
net<-HydeNetwork(~z+x|z+y|x)

plot(net)


net<-setNode(net,z, nodeType="dnorm",mu=0,tau=1)

net<-setNode(net,x,nodeType = "dbern", prob=paste("ilogit(-1.5+2*z)"))

net<-setNode(net,y,nodeType = "dnorm",
             mean=fromFormula(),tau=0.5,
             nodeFormula = y~-2*x)

writeNetworkModel(net,pretty=TRUE)
 
 
trackedVars<-c("z","x","y")
evidence=NULL
compiledNet<-compileJagsModel(net,data=evidence,n.chain=3,n.adapt=5000)

post<-HydeSim(compiledNet,variable.names =trackedVars,n.iter=10000)
dplyr::sample_n(post,10)

summary(post[c("x","y","z")])

cor(post[c("x","y","z")])



# the do operation requires cutting off incoming edges to x and set x to values
# E(y| do x=0)=-0.002519
# E(y| do x=1)=-2.001

net1<-HydeNetwork(~z+x+y|x)

plot(net1)


net1<-setNode(net1,z, nodeType="dnorm",mu=0,tau=1)
net1<-setNode(net1,x,nodeType = "dbern", prob=0.5)

net1<-setNode(net1,y,nodeType = "dnorm",
              mean=fromFormula(),tau=0.5,
              nodeFormula = y~-2*x)
trackedVars<-c("z","x","y")
evidence=list(x=0)
compiledNet1a<-compileJagsModel(net1,data=evidence,n.chain=3,n.adapt=5000)
post1a<-HydeSim(compiledNet1a,variable.names =trackedVars,n.iter=10000)
summary(post1a$y)


evidence=list(x=1)
compiledNet1b<-compileJagsModel(net1,data=evidence,n.chain=3,n.adapt=5000)
post1b<-HydeSim(compiledNet1b,variable.names =trackedVars,n.iter=10000)
summary(post1b$y)


# notice in this case, E(y|do x) is the same as E(y|x)
# E(y|x=0)=-0.001892, E(y|x=1)=-2.006
summary(subset(post, x == 0)$y)
summary(subset(post, x == 1)$y)

# indeed, we can tell z is not a confounder in regression setting
summary(lm(y~x,data=post))
summary(lm(y~x+z,data=post))



