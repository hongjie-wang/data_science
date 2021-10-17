# illustrative example of logistic regression
# Hongjie Wang 3-28-2021
# avoid using automation to show basic steps to EJW

rm(list = ls())
setwd("c:/users/hongjie/Documents/data_science/hands-on-ML/logistic_regression")
library(tidyverse)
# col_types is useful in getting hint on var types
# but not practical if you have huge nbr of variables 
mydata<-read_csv("donors.csv", col_types = "nnffnnnnnnnnffffffffff")

# change the dep to 0/1 factor

mydata<-mydata %>%
  mutate(respondedMailing=as.factor(ifelse(respondedMailing==TRUE,1,0)))

# for illustration,we assume only subset of vars
mydata<-mydata%>%
  select(incomeRating,respondedMailing,urbanicity,gender,
         age,averageGiftAmount,monthsSinceLastDonation,
         numberGifts,totalGivingAmount)

library(rsample)
set.seed(1234)
split_stra<-initial_split(mydata,prop=0.7,strata = respondedMailing)
donor_train<-training(split_stra)
donor_test<-testing(split_stra)

# focus on training dataset
# check out the categorical variables

donor_train %>% select(where(is.factor)) %>%
  summary()

# change missing to UNK
donor_train<-donor_train%>% 
  mutate(urbanicity=as.character(urbanicity)) %>%
  mutate(urbanicity=ifelse(is.na(urbanicity),'UNK',urbanicity))%>%
  mutate(urbanicity=as.factor(urbanicity))

donor_train <- donor_train%>%
  mutate(incomeRating = as.character(incomeRating)) %>%
  mutate(incomeRating = as.factor(ifelse(is.na(incomeRating), 'UNK',
                                         incomeRating)))
# gender is a factor, need to convert to charater so that 
# we can add UNK, it will
# then convert back to factor
donor_train <- donor_train%>%
  mutate(gender=as.character(gender))%>%
  mutate(gender = as.factor(ifelse(is.na(gender), 'UNK',
                                         gender)))

#exam the numeric variables

donor_train%>%select(where(is.numeric))%>%summary()

# we will impute age with median of the same gender

donor_train<-donor_train%>%
  group_by(gender)%>%
  mutate(age=ifelse(is.na(age),median(age,na.rm=TRUE),age))%>%
  ungroup()

# donor_train%>%group_by(gender)%>%summarise(m=mean(age),n=sd(age))


# look at distribution of variables
donor_train %>%
  select (-respondedMailing) %>%
  keep (is.numeric) %>%
  gather () %>%
  ggplot () +
  geom_histogram(mapping = aes(x=value,fill=key), color = "black") +
  facet_wrap (~ key, scales = "free") +
  theme_minimal ()

# derive outlier cutoff for totalGivingAmount
# set it as max fo this variable
donor_train<-donor_train%>%
  mutate(temp1=quantile(totalGivingAmount, .75) 
         + (1.5 *IQR(totalGivingAmount)))%>%
  mutate(totalGivingAmount=ifelse(totalGivingAmount>=temp1,
                                  temp1,totalGivingAmount))%>%
  select(-temp1)
  

# make sure no excessive multicollinearity 

library(stats)
library(corrplot)
donor_train%>%
  keep(is.numeric) %>%
  cor() %>%
  corrplot()


# use smote to oversample 
library(performanceEstimation)

donor_train_adj <- smote(respondedMailing ~ ., 
                         donor_train, 
                         perc.over = 1,perc.under=2)


# model 
mymod <- glm(data=donor_train_adj, family=binomial,
                   formula=respondedMailing ~ .)


train_pred_adj <- predict(mymod, donor_train_adj, type = 'response')
summary(train_pred_adj)

train_pred <- predict(mymod, donor_train, type = 'response')
summary(train_pred)

# implementation
# technically, we should find these transformations strictly from training process
#  we do not do that for time constraints
# in automation like recipe or caret, such will be automated

donor_test<-donor_test%>% 
  mutate(urbanicity=as.character(urbanicity)) %>%
  mutate(urbanicity=ifelse(is.na(urbanicity),'UNK',urbanicity))%>%
  mutate(urbanicity=as.factor(urbanicity))%>%
  mutate(incomeRating = as.character(incomeRating)) %>%
  mutate(incomeRating = as.factor(ifelse(is.na(incomeRating), 'UNK',
                                         incomeRating)))%>%
  mutate(gender=as.character(gender))%>%
  mutate(gender = as.factor(ifelse(is.na(gender), 'UNK',
                                   gender)))


donor_test<-donor_test%>%
  group_by(gender)%>%
  mutate(age=ifelse(is.na(age),median(age,na.rm=TRUE),age))%>%
  ungroup()

donor_test<-donor_test%>%
  mutate(temp1=quantile(totalGivingAmount, .75) 
         + (1.5 *IQR(totalGivingAmount)))%>%
  mutate(totalGivingAmount=ifelse(totalGivingAmount>=temp1,
                                  temp1,totalGivingAmount))%>%
  select(-temp1)

# score on test data
test_pred <- predict(mymod, donor_test, type = 'response')
summary(test_pred)

# find optimal cutoff to classify 0 or 1
library(InformationValue)
ideal_cutoff <-optimalCutoff(
    actuals = donor_test$respondedMailing,
    predictedScores =test_pred,
    optimiseFor = "Both")

# misclassification table manually 
test_class<-ifelse(test_pred>=ideal_cutoff,1,0)

table(donor_test$respondedMailing,test_class)



# we can use the caret pacakge confusion matrix directly
test_class<-as.factor(test_class)
donor_matrix<-caret::confusionMatrix(test_class,
                                     donor_test$respondedMailing,positive="1")

# ROCR gives ROC plots and AUC

roc_pred <-
  ROCR::prediction(
    predictions = test_pred,
    labels = donor_test$respondedMailing
  )
roc_perf <- ROCR::performance(roc_pred, measure = "tpr", x.measure = "fpr")
plot(roc_perf, main = "ROC Curve", col = "green", lwd = 3)
abline(a = 0, b = 1, lwd = 3, lty = 2, col = 1)
mod_auc<-ROCR::performance(roc_pred, measure = "auc")
unlist(slot(mod_auc,"y.values"))