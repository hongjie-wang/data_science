
rm(list=ls(all=TRUE))

library(HydeNet)

# simple serial network z->x->y
# we want to draw causal inference of z on y
net<-HydeNetwork(~z+x|z+y|x)

plot(net)


net<-setNode(net,z, nodeType="dbern",prob=0.5)

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



# the do operation requires cutting off incoming edges to z and set z to values
# in this case, there is nothing to change
# E(y| do x=0)=-0.3651
# E(y| do x=1)=-1.24417
# one unit of z leads to -0.879 delta y. 


evidence=list(z=0)
compiledNeta<-compileJagsModel(net,data=evidence,n.chain=3,n.adapt=5000)
posta<-HydeSim(compiledNeta,variable.names =trackedVars,n.iter=10000)
summary(posta$y)


evidence=list(z=1)
compiledNetb<-compileJagsModel(net,data=evidence,n.chain=3,n.adapt=5000)
postb<-HydeSim(compiledNetb,variable.names =trackedVars,n.iter=10000)
summary(postb$y)

##the same from looking at conditional mean directly (tiny diff)
post%>%
  dplyr::select(z,y)%>%
     dplyr::group_by(z)%>%
      dplyr::summarise(mean_y=mean(y))

 
# notice in this case, by introducing x, we would fail to detect the casual effect
# of z. 
summary(lm(y~z,data=post))
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -0.37692    0.01362  -27.67   <2e-16 ***
#  z           -0.84782    0.01920  -44.17   <2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

summary(lm(y~x+z,data=post))
#Coefficients:
#  Estimate Std. Error  t value Pr(>|t|)    
#(Intercept) -0.004901   0.012072   -0.406    0.685    
#x           -1.994833   0.018525 -107.681   <2e-16 ***
#  z            0.022962   0.018197    1.262    0.207    




