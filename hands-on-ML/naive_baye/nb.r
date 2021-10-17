# Hongjie Wang  
#example to illustrate Naive Baye
# p(y|x) ~ p(y) *p(x|y), naive in that p(x|y) is a product of p(x1|y)*p(x2|y)...
# that is, we assume indepenent - joint conditional becomes product


library(tidyverse)
library(rsample)
setwd("c:/users/hongjie/Documents/data_science/hands-on-ML/naive_baye")
email<-read_csv("email.csv")
str(email)
glimpse(email)

head(email,5)

email<-email%>% mutate(
  message_label=as.factor(message_label)
)
# see the top words for each class 
email%>%
  gather(-message_index,-message_label,key="word",value="count") %>%
  group_by(message_label,word) %>%
  summarize(occurrence=sum(count)) %>%
  arrange(message_label,desc(occurrence)) %>%
  slice(1:10)

set.seed(1234)
split_strat<-initial_split(email,prop=0.7,strata="message_label")

email_train<-training(split_strat)
email_test<-testing(split_strat)

table(email_train$message_label) %>%prop.table()
table(email_test$message_label) %>%prop.table()

library(e1071)
# laplace is used for smoothing, for some x, prob(x=a|y) could be zero
# this would make posterior prob zero. So, we smooth it by replacing 
# with a small number
email_mod <-naiveBayes(message_label ~ .-message_index, 
                       data = email_train, 
                       laplace = 1)

prediction<-predict(email_mod,newdata=email_test,type = "class")

table(email_test$message_label,prediction)