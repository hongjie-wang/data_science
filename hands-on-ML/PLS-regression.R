
# Partial least square  regression
# example from book hands-on ml with R
rm(list=ls())

library(caret)
library(dplyr)
library(rsample)
library(ggplot2)
library(vip)
library(pdp)

ames<- AmesHousing::make_ames()

set.seed(123)
split<-initial_split(ames,prop=0.7,strata="Sale_Price") 
ames_train<-training(split)
ames_test<-testing(split)
density(ames_train$Sale_Price) %>%plot
lines(density(ames_test$Sale_Price))

cv_model_pls<-train(
  Sale_Price~.,
  data=ames_train,
  method="pls",
  trControl=trainControl(method="cv",number=10),
  preProcess=c("zv","center","scale"),
  tuneLength=20
)
print(cv_model_pls$bestTune)
ggplot(cv_model_pls)

vip(cv_model_pls,num_features = 20,method="model")

partial(cv_model_pls,"Gr_Liv_Area", grid.resolution = 20, plot= TRUE)

