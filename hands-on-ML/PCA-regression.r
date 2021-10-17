
# PCA regression
# example from book hands-on ml with R
rm(list=ls())

library(caret)
library(dplyr)
library(rsample)
library(ggplot2)
library(vip)

ames<- AmesHousing::make_ames()

set.seed(123)
split<-initial_split(ames,prop=0.7,strata="Sale_Price") 
ames_train<-training(split)
ames_test<-testing(split)
density(ames_train$Sale_Price) %>%plot
lines(density(ames_test$Sale_Price))

cv_model_pcr<-train(
  Sale_Price~.,
  data=ames_train,
  method="pcr",
  trControl=trainControl(method="cv",number=10),
  preProcess=c("zv","center","scale"),
  tuneLength=25
)
print(cv_model_pcr$bestTune)
ggplot(cv_model_pcr)


