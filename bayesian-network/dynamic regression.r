rm(list = ls())
library(HydeNet)

net<-HydeNetwork(~channel+
                   apply|channel+
                   open|apply*channel+
                   fund|open*channel+
                   churn|open*fund*channel+
                   value.not.churn|channel+
                   value.churn+
                   value|value.not.churn*churn*value.churn*open)
net<-setNode(net,channel,nodeType ="dbern",prob=0.5)
equation.apply<-"-2+0.5*channel"

net<-setNode(net,apply,nodeType = "dbern",
             prob=paste("ilogit(",equation.apply,")"),
             validate=FALSE)

equation.open<-"-0.3-0.2*channel";

net<-setNode(net,open, nodeType = "dbern",
             prob=paste("(apply==1)*ilogit(",equation.open,")"),
             validate=FALSE)

equation.fund<-"0.5+0.3*channel"

net<-setNode(net,fund, nodeType = "dbern",
             prob=paste("(open==1)*ilogit(",equation.fund,")"),
             validate=FALSE)

equation.churn<-"-0.1+0.25*channel-0.4*fund"

net<-setNode(net,churn, nodeType = "dbern",
             prob=paste("(open==1)*ilogit(",equation.churn,")"),
             validate=FALSE)

net<-setNode(net,value.not.churn, nodeType = "dnorm",
             mu="250+50*channel",
             tau=5)

net<-setNode(net,value.churn, nodeType = "dnorm",
             mu=10,
             tau=1)

net<-setNode(net,value, nodeType = "determ",define=fromFormula(),
             nodeFormula = value~ifelse(open==0,0,ifelse(churn==1,value.churn,value.not.churn)))
                                  

plot(net)
writeNetworkModel(net,pretty=TRUE)

evidence=NULL



trackedVars<-c("channel","apply","open","fund","churn","value.not.churn","value.churn","value")
compiledNet<-compileJagsModel(net,data=evidence,n.chain=3,n.adapt=5000)
post<-HydeSim(compiledNet,variable.names =trackedVars,n.iter=100000)
dplyr::sample_n(post,10)
results <- dplyr::select(post, -obs_index, -chain_index,-value.churn,-value.not.churn)
colMeans(results)


# model two;

evidence=list(apply=1)

#evidence=list(brand.choice=0)

trackedVars<-c("channel","apply","open","fund","churn","value.not.churn","value.churn","value")
compiledNet<-compileJagsModel(net,data=evidence,n.chain=3,n.adapt=5000)
post<-HydeSim(compiledNet,variable.names =trackedVars,n.iter=100000)
dplyr::sample_n(post,10)
results <- dplyr::select(post, -obs_index, -chain_index,-value.churn,-value.not.churn)
colMeans(results)

evidence=list(apply=1,open=1)

#evidence=list(brand.choice=0)

trackedVars<-c("channel","apply","open","fund","churn","value.not.churn","value.churn","value")
compiledNet<-compileJagsModel(net,data=evidence,n.chain=3,n.adapt=5000)
post<-HydeSim(compiledNet,variable.names =trackedVars,n.iter=100000)
dplyr::sample_n(post,10)
results <- dplyr::select(post, -obs_index, -chain_index, -value.churn,-value.not.churn)
colMeans(results)


evidence=list(apply=1,open=1,channel=1)

#evidence=list(brand.choice=0)

trackedVars<-c("channel","apply","open","fund","churn","value.not.churn","value.churn","value")
compiledNet<-compileJagsModel(net,data=evidence,n.chain=3,n.adapt=5000)
post<-HydeSim(compiledNet,variable.names =trackedVars,n.iter=100000)
dplyr::sample_n(post,10)
results <- dplyr::select(post, -obs_index, -chain_index)
colMeans(results)


evidence=list(apply=1,open=1,fund=1)

#evidence=list(brand.choice=0)

trackedVars<-c("channel","apply","open","fund","churn","value.not.churn","value.churn","value")
compiledNet<-compileJagsModel(net,data=evidence,n.chain=3,n.adapt=5000)
post<-HydeSim(compiledNet,variable.names =trackedVars,n.iter=100000)
dplyr::sample_n(post,10)
results <- dplyr::select(post, -obs_index, -chain_index, -value.churn,-value.not.churn)
colMeans(results)
























