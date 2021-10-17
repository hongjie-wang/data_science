# illustrative example to show operational network 
# customer acquisition, retention and service access quality 
# we run acqusition marketing 
# new customers and existing customers compete for service resources
# capacity level (and cost) leads to quality which leads to retention

rm(list = ls())
library(HydeNet)

net<-HydeNetwork(~marketing+cost1|marketing
                 + modeling + cost2|modeling+  
                   resp.rate|modeling+responders|marketing*resp.rate
                 +underwriting + cost3|underwriting+app.rate|underwriting
                 +new.customers|responders*app.rate
                 +inquiry.rate+inquiry.req|inquiry.rate*marketing
                 +nc.serv.rate+nc.serv.req|new.customers*nc.serv.rate
                 +current.customers+cc.serv.rate+cc.serv.req|current.customers*cc.serv.rate
                 +reps+capacity|reps
                 +cost4|reps
                 +serv.quality|capacity*cc.serv.req*nc.serv.req*inquiry.req
                 +value.per.customer
                 +revenue|value.per.customer*new.customers*current.customers*serv.quality
                 +total.profit|revenue*cost1*cost2*cost3*cost4)

# we market to 5MM consumers for new opening 
net<-setNode(net,marketing,nodeType ="dpois",lambda=5000000)

# cost of marketing 
net<-setNode(net,cost1,nodeType = "dnorm",
             mean=fromFormula(),tau=100,
             nodeFormula = cost1~0.0006*marketing)


# consumers who are marketed will have questions at 1.5% on average
net<-setNode(net,inquiry.rate,nodeType = "dbeta",a=1.5,b=100)

# nbr of inquiries. 
net<-setNode(net,inquiry.req,nodeType ="dbin",prob="inquiry.rate",size="marketing")



# use a model or not. It will be a decision variable

net<-setNode(net,modeling,nodeType="dbern",p=0.5)

# cost of modeling 
net<-setNode(net,cost2,nodeType = "dnorm",
             mean=fromFormula(),tau=100,
             nodeFormula = cost2~100 * modeling)

# model improves the response by 0.015 on average 

net<-setNode(net,resp.rate,nodeType="dbeta",a="1000*(0.01+0.015*modeling)",
                                           b="1000*(1-(0.01+0.015*modeling))")

# get responders
net<-setNode(net,responders,nodeType ="dbin",prob="resp.rate",size="marketing")

# improve underwriting or not, later a decision variable 
net<-setNode(net,underwriting,nodeType="dbern",p=0.5)

# cost of underwriting modeling 
net<-setNode(net,cost3,nodeType = "dnorm",
             mean=fromFormula(),tau=100,
             nodeFormula = cost3~150*underwriting)

# underwriting efforts can improve the approval by 15%

net<-setNode(net,app.rate,nodeType="dbeta",a="1000*(0.35+0.15*underwriting)",
             b="1000*(1-(0.35+0.15*underwriting))")

# nbr of new customers 
net<-setNode(net,new.customers,nodeType ="dbin",prob="app.rate",size="responders")

# on average 50% will need service among new customers
net<-setNode(net,nc.serv.rate,nodeType = "dbeta",a=50,b=50)

# nbr of service requests
net<-setNode(net,nc.serv.req,nodeType ="dbin",prob="nc.serv.rate",size="new.customers")

# nbr of existing customers
net<-setNode(net,current.customers,nodeType ="determ",define=fromFormula(),
             nodeFormula = current.customers~14000000)

# 2% on average service rate
net<-setNode(net,cc.serv.rate,nodeType = "dbeta",a=20,b=80)

# nbr of service requests from existing customers
net<-setNode(net,cc.serv.req,nodeType ="dbin",prob="cc.serv.rate",size="current.customers")

# decision variable number of reps 
# we specified it as a RV so that we can set evidence to run scenario simulation
# you can also use policy if we use deterministic node
net<-setNode(net,reps,nodeType ="dnorm",mu=600,tau=3000)

# nbr of call capacity in 120 days 
net<-setNode(net,capacity,nodeType = "dnorm",
             mean=fromFormula(),tau=0.00001,
             nodeFormula = capacity~reps*40*120)

# cost of reps, 
net<-setNode(net,cost4,nodeType = "dnorm",
             mean=fromFormula(),tau=0.01,
             nodeFormula = cost4~6000+6*reps)

# service not filfilled due to capacity 
# if this number is negative, that means surplus of capability

net<-setNode(net,serv.quality,nodeType = "determ",
             mean=fromFormula(),
             nodeFormula = serv.quality~(cc.serv.req+nc.serv.req+inquiry.req-capacity)/(cc.serv.req+nc.serv.req+inquiry.req))

net<-setNode(net,value.per.customer,nodeType = "dnorm",mean=0.02,tau=200000)


net<-setNode(net,revenue,nodeType ="determ",define=fromFormula(),
             nodeFormula = revenue~exp(-max(0,serv.quality))*value.per.customer*(new.customers+current.customers))

net<-setNode(net,total.profit,nodeType ="determ",define=fromFormula(),
             nodeFormula = total.profit~revenue-cost1-cost2-cost3-cost4)

net<-setDecisionNodes(net,marketing)
net<-setDecisionNodes(net,modeling)
net<-setDecisionNodes(net,underwriting)
net<-setDecisionNodes(net,reps)

net<-setUtilityNodes(net,cost1)
net<-setUtilityNodes(net,cost2)
net<-setUtilityNodes(net,cost3)
net<-setUtilityNodes(net,cost4)
net<-setUtilityNodes(net,revenue)
net<-setUtilityNodes(net,total.profit)

plot(net)

writeNetworkModel(net,pretty=TRUE)

evidence=NULL

trackedVars<-c("marketing","modeling","resp.rate","cost1","responders",
               "underwriting","cost2","app.rate","cost3",
               "new.customers","nc.serv.rate","nc.serv.req",
               "current.customers","cc.serv.rate","cc.serv.req",
               "reps","capacity","cost4","serv.quality",
               "value.per.customer","revenue","inquiry.rate","inquiry.req","total.profit")


compiledNet<-compileJagsModel(net,data=evidence,n.chain=3,n.adapt=5000)
post<-HydeSim(compiledNet,variable.names =trackedVars,n.iter=10000)
dplyr::sample_n(post,10)

#plot(density(post$capacity))
#plot(density(post$total.profit))


# should we use models?
policies<-data.frame(modeling=c(0,1,0,1),
                     underwriting=(c(0,0,1,1)))

net2<-compileDecisionModel(net,policyMatrix =policies)

samples<-lapply(net2,HydeSim,variable.names=c("modeling","underwriting","total.profit"),n.iter=10000)
lapply(samples, head)
lapply(samples,function(l) summary(l$total.profit))

# what level of allocation or staffing is optimal

history=data.frame()

for (i in seq(200,1000,10)){
  evidence=list(reps=i)
  compiledNet<-compileJagsModel(net,data=evidence,n.chain=3,n.adapt=10000)
  post<-HydeSim(compiledNet,variable.names =c("serv.quality","total.profit"),n.iter=10000)
  results=cbind(reps=i,serv.quality=mean(post$serv.quality),mean.profit=mean(post$total.profit),
                p25.profit=quantile(post$total.profit,0.05),p95.profit=quantile(post$total.profit,0.95))
  history=rbind(history,results)
}
plot(history$reps,history$total.profit)





