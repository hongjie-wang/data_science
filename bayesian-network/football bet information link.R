#bayesian artificial page 103
# betting on football
# in this case, notice the bet is determined by forecast
# suppose want to know what is the optimal strategy based on the information
# we create a new node forecast and a dotted line (informatino link) to bet


rm(list = ls())
library(HydeNet)

# notice, in our model, bet does not depend on forecast
# this is different from executing an optimal decision (see football bet optimal decsion)

net<-HydeNetwork(~+weather+result|weather+forecast|weather+bet+U|bet*result)


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

cp2=data.frame(weather=as.factor(c("Wet","Wet","Wet","Dry","Dry","Dry")),
               forecast=as.factor(c("rainy","cloudy","sunny","rainy","cloudy","sunny")),
               prob=c(0.6,0.25,0.15,0.1,0.4,0.5))

forecast.h=cpt(forecast~weather,data=cp2,wt=cp2$prob)

net<-setNodeModels(net,forecast.h)


net<-setNode(net,bet,nodeType = "dbern",p=0.5)
  

net<-setNode(net,U, nodeType = "determ",define=fromFormula(),
             nodeFormula = U~ifelse(result=="win" && bet==1,40,
                                    ifelse(result=="win"&& bet==0,20,
                                           ifelse(result=="loss"&& bet==1,-20,-5))))

net<-setDecisionNodes(net,bet)
net<-setUtilityNodes(net,U)

plot(net)


writeNetworkModel(net,pretty=TRUE)

trackedVars<-c("weather","result","bet","U","forecast")

# if the forecast is cloud, we should not bet

evidence=list(forecast="cloudy",bet=1)
compiledNet<-compileJagsModel(net,data=evidence,n.chain=3,n.adapt=5000)
post<-HydeSim(compiledNet,variable.names =trackedVars,n.iter=10000)
mean(post$U)

evidence=list(forecast="cloudy",bet=0)
compiledNet<-compileJagsModel(net,data=evidence,n.chain=3,n.adapt=5000)
post<-HydeSim(compiledNet,variable.names =trackedVars,n.iter=10000)
mean(post$U)

