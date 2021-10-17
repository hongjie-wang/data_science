# example of using pre-processing and control in fitting KNN
rm(list=ls())
library(dplyr)
library(rsample)
library(caret)
library(ggplot2)
library(recipes)

set.seed(123)
ames <- AmesHousing::make_ames()
# use rsample pacakge to stratify 

split<-initial_split(ames,prop=0.7,strata="Sale_Price")
ames_train<-training(split)
ames_test<-testing(split)
density(ames_train$Sale_Price) %>%plot
lines(density(ames_test$Sale_Price))

# use reciple pacakge to set up pre-processing blueprint
# specifiy the outcome var, nzv remove cat var with high concentration
blueprint<-recipe(Sale_Price~.,data=ames_train) %>%
  step_nzv(all_nominal()) %>%
  step_integer(matches("Qual|Cond|QC|Qu")) %>%
  step_center(all_numeric(),-all_outcomes())%>%
  step_scale(all_numeric(),-all_outcomes()) %>%
  step_dummy(all_nominal(),-all_outcomes(),one_hot=TRUE)

cv<-trainControl(
  method="repeatedcv",
  number=10,
  repeats=5
)

hyper_grid<-expand.grid(k=seq(2,25,by=1))

knn_fit<-train(
  blueprint,
  data=ames_train,
  method="knn",
  trControl = cv,
  tuneGrid = hyper_grid,
  metric="RMSE"
)

ggplot(knn_fit)