#example 14.6 in book "Risk Assessent and Decision Analysis with Bayesian Network"
#P0 and V0 are initial location and speed
#Vi=V_(i-1), P_i=P_{i-1}+V_{i-1}
#O are the observations of location based on sensor
# the sensor could be faulty
#question- given the observed data (O1,O2,O3,O4), what is the prob 
# sensor is faulty
# I could not get the exact solution from the book it did not specify V0,P0.

library(HydeNet)

net<-HydeNetwork(~V0+P0
                 +sensor.faulty+sensor.accuracy|sensor.faulty
                   +V1|V0+V2|V1+V3|V2+V4|V3
                   +P1|P0*V0+P2|V1*P1+P3|V2*P2+P4|V3*P3
                   +O1|P1*sensor.accuracy+O2|P2*sensor.accuracy
                   +O3|P3*sensor.accuracy+O4|P4*sensor.accuracy)
plot(net)

net<-setNode(net, sensor.faulty, nodeType="dbern",p=0.5)

net<-setNode(net,sensor.accuracy,nodeType="determ",
              define=fromFormula(),
              nodeFormula = sensor.accuracy~ifelse(sensor.faulty==1,1000,10))
    

net<-setNode(net,V0,nodeType ="dnorm",mu=10,tau=1/1000 )
net<-setNode(net,P0,nodeType ="dnorm",mu=0, tau=1/1000)

net<-setNode(net,V1,nodeType = "determ",define=fromFormula(),
             nodeFormula = V1~V0)

net<-setNode(net,V2,nodeType = "determ",define=fromFormula(),
             nodeFormula = V2~V1)

net<-setNode(net,V3,nodeType = "determ",define=fromFormula(),
             nodeFormula = V3~V2)

net<-setNode(net,V4,nodeType = "determ",define=fromFormula(),
             nodeFormula = V4~V3)

net<-setNode(net,P1,nodeType = "determ",define=fromFormula(),
             nodeFormula = P1~V0+P0)

net<-setNode(net,P2,nodeType = "determ",define=fromFormula(),
             nodeFormula = P2~V1+P1)

net<-setNode(net,P3,nodeType = "determ",define=fromFormula(),
             nodeFormula = P3~V2+P2)

net<-setNode(net,P4,nodeType = "determ",define=fromFormula(),
             nodeFormula = P4~V3+P3)

net<-setNode(net,O1,nodeType ="dnorm",mu="P1", tau="1/sensor.accuracy")
net<-setNode(net,O2,nodeType ="dnorm",mu="P2", tau="1/sensor.accuracy")
net<-setNode(net,O3,nodeType ="dnorm",mu="P3", tau="1/sensor.accuracy")
net<-setNode(net,O4,nodeType ="dnorm",mu="P4", tau="1/sensor.accuracy")

writeNetworkModel(net,pretty=TRUE)
 
 
 trackedVars<-c("V0","V1","V2","V3","V4",
                "P0","P1","P2","P3","P4",
                "O1","O2","O3","O4","sensor.faulty","sensor.accuracy")
              
# if we observe 2 defects in 100 sample, the prob that 
# prob failure is less than 0.01 is 8%
 
evidence=list(O1=10,O2=20,O3=17,O4=45)
 compiledNet<-compileJagsModel(net,data=evidence,n.chain=3,n.adapt=5000)
 post<-HydeSim(compiledNet,variable.names =trackedVars,n.iter=10000)
 dplyr::sample_n(post,10)
 
plot(density(post$V0))
mean(post$sensor.faulty)