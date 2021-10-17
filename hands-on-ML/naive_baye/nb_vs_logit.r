#program nb_vs_logit.r
#illustrate the relationship between Naive Baye and Logistic model
#verifying the proof in T Mitchell new book chapter on generative model


library(tidyverse)
library(ggplot2)
library(rsample)

set.seed(1234)

# Part One
#data simulation
#prob(z=0|x)=prob(y=1|x)=exp(0+1.5x)/(1+exp(0+1.5))
#prob(z=1|x)=prob(y=0|x)=exp(0-1.5x)/(1+exp(0-1.5x))

n<-20000
x<-rnorm(n,0,1)
xbeta=0+1.5*x
prob=exp(xbeta)/(1+exp(xbeta))
y=rbinom(n,1,prob)
mydata<-data.frame(x=x,y=y)%>%
  mutate(z=1-y,
         z=as.factor(z),
         y=as.factor(y))

# train and test

split_strat<-initial_split(mydata,prop=0.5,strata="y")

mydata_train<-training(split_strat)
mydata_test<-testing(split_strat)

# Part two, some EDA

# summary by rank
summary_by_rank<-mydata_train %>%
  mutate(xrank=as.integer(cut(x,breaks=20))) %>%
  group_by(xrank) %>%
  filter(n()>=2)%>%
  summarize(counts=n(),minx=min(x),max=max(x),avgx=mean(x),avgy=mean(y==1))
  
summary_by_rank %>%
  ggplot(aes(avgx,avgy))+geom_point()

# prob plot 
ggplot(mydata_train, aes(x=x)) + geom_density(aes(group=y))


# Part three logistic model
# as we can see, recover the true paramters nicely
# obtain prob(z=1|x)=prob(y=0|x) using logistic regression 

logit_model<-glm(z~x,data=mydata_train,family="binomial")
summary(logit_model)


# Part four, NB approach 
# we show the coefs can be recovered using NB approach
#equations 22 and 23 in T Mitchell's book chapter Machine Learning chapter 3
# geneative model and the connection between NB and logistic regression 

# NB assumes f(x|y) normal and we get sample based estimates of 
# mean and std of the normal distribution

nb_params<-mydata_train%>%
  group_by(y)%>%
  summarize(counts=n(),minx=min(x),maxx=max(x),meanx=mean(x),std=sd(x))

# we assume the same variance, as required by NB
# pi is prob(y=1) or the prior
mu0=nb_params$meanx[1]
mu1=nb_params$meanx[2]
theta=(nb_params$std[1]+nb_params$std[2])/2
pi=nb_params$counts[2]/(sum(nb_params$counts))

# implementing the equations 22 and 23 in the book
beta1=(mu0-mu1)/theta**2
beta0=log(pi/(1-pi))+(mu1**2-mu0**2)/(2*theta**2)

# notice the coefficients are very close to what we get from logistic model
print(c(pi,mu0,mu1,theta,beta0,beta1))



#Part five, in practice, we obvously do not recover coefs
# from NB, we apply NB directly, we will show that they generate
# almost idetnical prob as logistic regression when the assumptions are made
# if the assumptions are not true that is f(x|y) conditianlly IID gaussian
# then NB will be biased. NB is more parametric than logistic 
# logistic is trying to approximate prob(y|x) directly, in a sense
# like NN.From model standpoint it is less parametric, although
# the functional class it uses to approximate p(y|x) is parametric.
# NN is non parametric in modeling, but parametric completey in algorithmic


# scoring test using the logit model prob(y=0|x)

logit_pred<-predict(logit_model,newdata=mydata_test,type="response")


#nb model 
#p(y=0|x)=f(x|y=0)*p(y=0)/(f(x|y=0)*p(y=0)+f(x|y=1)*p(y=1))
# where f(x|y) is conditional density function which we assume to be normal

 
f0=dnorm(mydata_test$x,nb_params$meanx[1],nb_params$std[1])
f1=dnorm(mydata_test$x,nb_params$meanx[2],nb_params$std[2])
nb_pred<-f0*(1-pi)/(f0*(1-pi)+f1*pi)

sqrt(sum((logit_pred-nb_pred)**2))
print(c(mean(nb_pred),mean(logit_pred),cor(nb_pred,logit_pred)))
