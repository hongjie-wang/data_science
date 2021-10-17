#bayesian artificial page 100
# betting on football
rm(list = ls())
library(HydeNet)
net<-HydeNetwork(~+weather+result|weather+bet+U|bet*result)

# use dcat for categorical var, include binary, its label seems to work
# dbern lable creates strange problems later on with cpt

net<-setNode(net,node=weather,nodeType = "dcat",pi=vectorProbs(p=c(0.7,0.3),weather),
             factorLevels = c("Dry","Wet"))

# use this approch to enter conditional prob direclty

cp1=data.frame(weather=as.factor(c("Wet","Wet","Dry","Dry")),
               result=as.factor(c("win","loss","win","loss")),
               prob=c(0.6,0.4,0.25,0.75))

result.h=cpt(result~weather,data=cp1,wt=cp1$prob)

net<-setNodeModels(net,result.h)

net<-setNode(net,node=bet,nodeType = "dcat",pi=vectorProbs(p=c(0.5,0.5),bet),
             factorLevels = c("Yes","No"))

net<-setNode(net,U, nodeType = "determ",define=fromFormula(),
             nodeFormula = U~ifelse(result=="win" && bet=="Yes",40,
                                    ifelse(result=="win"&& bet=="No",20,
                                           ifelse(result=="loss"&& bet=="Yes",-20,-5))))

net<-setDecisionNodes(net,bet)
net<-setUtilityNodes(net,U)

plot(net)


writeNetworkModel(net,pretty=TRUE)

evidence=NULL

trackedVars<-c("weather","result","bet","U")


compiledNet<-compileJagsModel(net,data=evidence,n.chain=3,n.adapt=5000)
post<-HydeSim(compiledNet,variable.names =trackedVars,n.iter=10000)
dplyr::sample_n(post,10)

# should we bet? 
policies<-data.frame(bet=c("Yes","No"))
compliedNets<-compileDecisionModel(net,policyMatrix = policies)

samples<-lapply(compliedNets,HydeSim,variable.names=trackedVars,n.iter=10000)

lapply(samples,head)
lapply(samples,function(l) mean(l$U))
