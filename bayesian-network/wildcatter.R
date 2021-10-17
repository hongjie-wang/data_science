# based on data from Bayesian Networks and ID
#by Madsen on page 82

library(HydeNet)
 
 
net<-HydeNetwork(~test+oil+seismic|test*oil+U1|test+drill|test*seismic+U2|oil*drill)

# we take random action in testing to collect data, later we wil determine which is bes
net<-setNode(net,test,nodeType="dbern",p=0.5,factorLevels = c("yes","no"))

#prior of oil 

net<-setNode(net,oil,nodeType="dcat",pi=vectorProbs(p=c(0.5,0.3,0.2),oil),
                factorLevels = c("dry","wet","soaking"))

# if test, cost is 10

net<-setNode(net,U1,nodeType = "determ",
             define=fromFormula(),
             nodeFormula = U1~ifelse(test=="yes",-10,0))
#test result (seismic is a function of oil and taking test decision)
# we enter these values interactive, CPT is not easy to load manaully with this package
# need to figure out a better way
# define a model h for this node, then add to the network
h<-inputCPT(seismic ~ test + oil,
         factorLevels <- list(seismic = c("diffuse","open","closed","noresults"),
                              test     = c("yes","no"),
                              oil  = c("dry","wet","soaking")),reduce = FALSE)

net<-setNodeModels(net,h)


# we randomly decide to drill to collect data via simulation 
# so that we can figure out optimal 

net<-setNode(net,drill,nodeType = "dbern", p=0.5)

# results if we drill.             
net<-setNode(net,U2,nodeType = "determ",
             define=fromFormula(),
             nodeFormula = U2~ifelse(oil=="dry" && drill==1,-70,
                                     ifelse(oil=="wet" && drill==1,50,
                                            ifelse(oil=="soaking" && drill==1,200,0))))


net<-setDecisionNodes(net,test)
net<-setDecisionNodes(net,drill)
net<-setUtilityNodes(net,U1)
net<-setUtilityNodes(net,U2)

plot(net)


writeNetworkModel(net,pretty=TRUE)


evidence=NULL
 
 trackedVars<-c("test","oil","seismic","U1","drill","U2")
 compiledNet<-compileJagsModel(net,data=evidence,n.chain=3,n.adapt=5000)
 post<-HydeSim(compiledNet,variable.names =trackedVars,n.iter=10000)
 dplyr::sample_n(post,10)
 summary(post$U2+post$U1)

 aggregate(U1+U2~drill+test+seismic,data=post,mean)
 # based on the results, if we oberve test results, we should drill unless it is diffuse
 
 #drill test   seismic   U1 + U2
 #1     0  yes   diffuse -10.00000
 #2     1  yes   diffuse -40.99045
 #3     0  yes      open -10.00000
 #4     1  yes      open  25.06324
 #5     0  yes    closed -10.00000
 #6     1  yes    closed  76.87569
 #7     0   no noresults   0.00000
 #8     1   no noresults  18.95054 
 
 # we then implement this optimal rule for drill and see if we should test or not
 net<-setNode(net,drill,nodeType = "determ",
                           define=fromFormula(),
                           nodeFormula = drill~ifelse(((test=="yes")&& (seismic=="diffuse")),0,1))
 
 evidence=list(test="yes")
 
 trackedVars<-c("test","oil","seismic","U1","drill","U2")
 compiledNet<-compileJagsModel(net,data=evidence,n.chain=3,n.adapt=5000)
 post<-HydeSim(compiledNet,variable.names =trackedVars,n.iter=10000)
 summary(post$U2+post$U1)
 
 evidence=list(test="no")
 
 trackedVars<-c("test","oil","seismic","U1","drill","U2")
 compiledNet<-compileJagsModel(net,data=evidence,n.chain=3,n.adapt=5000)
 post<-HydeSim(compiledNet,variable.names =trackedVars,n.iter=10000)
 summary(post$U2+post$U1)
 
 # the value is 23 if test and 20 if no test, therefore, we should always test
 
 