
# various ways for regularized regressions using glmnet
# example from book hands-on ml with R
# Hongjie Wang 3-21-2021
rm(list=ls())

library(caret)
library(dplyr)
library(rsample)
library(ggplot2)
library(vip)
library(pdp)
library(glmnet)
library(modeldata)

data(attrition)
summary(attrition)

df<-attrition%>%mutate_if(is.ordered,factor,ordered=FALSE)

set.seed(123)
split<-initial_split(df,prop=0.7,strata="Attrition") 
attrition_train<-training(split)
attrition_test<-testing(split)
table(attrition_test$Attrition)%>%prop.table()
table(attrition_train$Attrition)%>%prop.table()

# logistic regression using caret with CV
# so that you can get CV feature of running many logit model
# should be preferred to one-logit model approach
glm_mod<-train(
  Attrition~.,
  data=attrition_train,
  method="glm",
  family="binomial",
  preProc=c("zv","center","scale"),
  trControl=trainControl(method="cv",number=10)
)

# using caret to tune
# two parameters alpha (the proportion between lasso and ridge)
# lambda (the penality for the coeffients)
# glmnet should prep-process, but maybe you have to if you invoke it
# from caret
penalized_mod<-train(
  Attrition~.,
  data=attrition_train,
  method="glmnet",
  family="binomial",
  preProc=c("zv","center","scale"),
  trControl=trainControl(method="cv",number=10),
  tuneLength=10
)

# alpha = 0.5 and lambda = 0.006741201.
#ggplot(penalized_mod)

# we can also use glmnet directly
# glmnet.cv searchs for lambda,not alpha, 
# it also requires matrix data separating y and X

X<-model.matrix(Attrition~.,attrition_train)[,-1]
y<-attrition_train$Attrition

# we use alpha=0.5 from prevous caret process

elastic_net_mod<-cv.glmnet(
  x=X, 
  y=y, 
  family = "binomial", 
  alpha=0.5,
  type.measure = "class")
 
plot(elastic_net_mod)

# we could not get the same lambda from this
# not sure why, but it is possible that 
# there is no unique solution to these tuning paramters 
min(elastic_net_mod$cvm)
elastic_net_mod$lambda[elastic_net_mod$cvm==min(elastic_net_mod$cvm)]








 



