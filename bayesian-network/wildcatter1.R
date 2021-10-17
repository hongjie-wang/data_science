library(HydeNet)
# simplied version of oil wildcatte problem page (p421 Bayesian network and decsion graphs Jensen)
library(HydeNet)
net<-HydeNetwork(~hole+test+test.result|test+drill|test.result+gain|hole*drill+cost|test)

# hole has a prior 20% chance of having oil
net<-setNode(net,hole,nodeType="dbern",p=0.2,factorLevels = c("good","bad"))

# we make random decision in terms of testing, but notice this is a decsion varibable
net<-setNode(net,test,nodeType="dbern",p=0.5,factorLevels = c("yes","no"))




net<-setNode(net,oil,nodeType="dcat",pi=vectorProbs(p=c(0.5,0.3,0.2),oil),
             factorLevels = c("dry","wet","soaking"))

net<-setNode(net,U1,nodeType = "determ",
             define=fromFormula(),
             nodeFormula = U1~ifelse(test=="yes",-10,0))

h<-inputCPT(seismic ~ test + oil,
            factorLevels <- list(seismic = c("diffuse","open","closed","noresults"),
                                 test     = c("yes","no"),
                                 oil  = c("dry","wet","soaking")),reduce = FALSE)

net<-setNodeModels(net,h)
 
 