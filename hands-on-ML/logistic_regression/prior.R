# purpose is to show 
# response based sampling
# adjust for intercept in logitistic model
# equivalently use weights in estimation
# the same as using different cutoff in classification
# can we detect class proportion drift and estimate it? 
# 4/23/2021

rm(list=ls())

library(tidyverse)
library(ROCR)
library(cutpointr)
library(R2jags)

set.seed(1234)


n<-100000
x<-rnorm(n,0,1)
xbeta=-2+1.5*x
prob=exp(xbeta)/(1+exp(xbeta))
y=rbinom(n,1,prob)
population<-data.frame(x=x,y=as.factor(y),truep=prob)
summary(population)
table(population$y)%>%prop.table()

# population cutoff at 0.5 
pop_cut<-cutpointr(data=population,x=truep,class=y,method=maximize_metric,
                     metric=accuracy)
plot_metric(pop_cut)
summary(pop_cut)

# split into train and test
# make train more balanced 

population<-population%>%
  mutate(seltemp=runif(n,0,1))%>%
  mutate(group=ifelse(seltemp>=0.5,"test","train"))%>%
  select(-seltemp)


train<-population%>%
  filter(group=="train")

test<-population%>%
  filter(group=='test')


train<-train%>%
  mutate(seltemp=runif(dim(train)[1],0,1))%>%
  mutate(group=ifelse(y==1,"yes",ifelse(seltemp<=0.25,"yes","no")))%>%
    filter(group=="yes")%>%
    select(-group,-seltemp)

table(train$y)%>%prop.table()
table(test$y)%>%prop.table()


# build model based on train 

model<-glm(y~x,data=train,family="binomial")

# produce unbiased estimate for the response rate in training
# avg score =0.4856. 
# using around 0.5 cutoff, accuracy is 0.74, sensitivity 0.72, 
#  spe 0.76

train$score<-predict(model,train,type="response")

train_cut<-cutpointr(data=train,x=score,class=y,method=maximize_metric,
                     metric=accuracy)
summary(train_cut)



# notice it produces biased estimte for test sample 
test$score<-predict(model,test,type="response")
summary(test)
table(test$y)%>%prop.table()


# notice the p(x|y),p(score|y) are the same for both train and test
# that is to be expected due to response based sampling
# but because of this, the score does not work in test due to prior changes

train %>%
  group_by(y) %>%
  summarize(counts=n(),avgx=mean(x),sdx=sd(x),
             avgscore=mean(score), sdsore=sd(score))

test %>%
  group_by(y) %>%
  summarize(counts=n(),avgx=mean(x),sdx=sd(x),
            avgscore=mean(score), sdsore=sd(score))



# correcing using new priors
  

test<-test%>%
  mutate(newprior1=0.18875,
         oldprior1=0.4856165,
         ratio1=newprior1/oldprior1,
         newprior0=1-newprior1,
         oldprior0=1-oldprior1,
         ratio0=newprior0/oldprior0,
         proby_1=score,
         proby_0=1-score)%>%
  mutate(score2=ratio1*proby_1/(ratio1*proby_1+ratio0*proby_0))%>%
  select(-newprior1, -oldprior1,-ratio1,-newprior0,-oldprior0,-ratio0,-proby_1,
         -proby_0)                                        

summary(test$score2)

#  new score is should be like building a logisitc regression using
# old score as the only input with fixed coef

test%>%
  sample_n(1000)%>%
  ggplot()+geom_point(mapping=aes(x=score,y=score2,color=y))

# ROC are the same for both old and adjusted score
# For the same false positive, both have the same true positive
# but the same false positive would correspond to different cutoff

perf1<-prediction(test$score,test$y)%>%
  performance(measure="tpr",x="fpr")

perf2<-prediction(test$score2,test$y)%>%
  performance(measure="tpr",x="fpr")

plot(perf1,col="black",lty=2)
plot(perf2,add=TRUE, col="blue")
legend(0.8,0.2,legend=c("original","adjusted"),
       col=c("black","blue"))

# the adjustment can be equivalently achieved by adjusting cutoff 

test_cut<-cutpointr(data=test,x=score,class=y,method=maximize_boot_metric,
                     metric=accuracy)
summary(test_cut)

test_cut_adj<-cutpointr(data=test,x=score2,class=y,method=maximize_metric,
                     metric=accuracy)
summary(test_cut_adj)

# can we estiamte the new prior? 
# see D. Tasche we implement the AC methods

train$predict=as.factor(ifelse(train$score>=0.5,1,0))
test$predict=as.factor(ifelse(test$score>=0.5,1,0))

table(train$y,train$predict) 

table(test$y,test$predict) 

p_g0_y1<-2472/(2472+6899)
p_g0_y0<-30141/(30141+10291)
q_g0<-(30141+2472)/(30141+10291+2472+6899)

q0<-(q_g0-p_g0_y1)/(p_g0_y0-p_g0_y1)

print(q0)
summary(test$y)%>%prop.table()




# suppose we do not know the prior, but we assume p(x|y) remain constant, 
# we could estimate the prior- can we use ML to estimate? 



mix.model <-"model {
  for (i in 1:length(y)) {
y[i] ~ dnorm(mu[group[i]+1],tau)
group[i] ~ dbern(p)
}

mu0[1]~dnorm(0.335,5000)
mu0[2]~dnorm(0.645,5000)
p ~ dbeta(1,1)
mu[1:2] <- sort(mu0)
sd<-0.23
tau<-1/(sd*sd)
}"

inits<-function(){
  list(mu0=rnorm(2,0,100))
}

model.data<-list(y=test$score)

params<-c("p","sd","mu")

mix.jags<-jags(model=textConnection(mix.model),
               inits=inits,
               data=model.data,
               parameters=params)
print(mix.jags)


#Inference for Bugs model at "4", fit using jags,
#3 chains, each with 2000 iterations (first 1000 discarded)
#n.sims = 3000 iterations saved
#mu.vect sd.vect       2.5%        25%        50%        75%      97.5%  Rhat
#mu[1]         0.296   0.002      0.292      0.295      0.296      0.297      0.300 1.008
#mu[2]         0.654   0.004      0.647      0.652      0.654      0.657      0.662 1.002
#p             0.273   0.006      0.262      0.269      0.273      0.276      0.284 1.010
#sd            0.230   0.000      0.230      0.230      0.230      0.230      0.230 1.000
#deviance -13565.526 317.551 -14174.273 -13778.910 -13568.255 -13347.819 -12943.024 1.007
#n.eff
#mu[1]      340


                                                            
