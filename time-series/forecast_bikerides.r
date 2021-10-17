# example from SHMUELI & LICHTENDAHL book on practical time series 

library(tidyverse)
library(lubridate)
library(forecast)
bike.df<-read.csv('C:/Users/hongjie/Documents/data_science/time-series/data/BikeSharingDaily.csv')

# pre-processing data, especially some date related variables

bike.df<-bike.df%>%
  mutate(Date=ymd(dteday),
         month=month(Date,label=TRUE),
         DOW=wday(Date,label=TRUE),
         WorkingDay=factor(workingday,levels=c(0,1),labels=c("not_working","working")),
         Weather=factor(weathersit,levels=c(1,2,3),labels=c("Clear","Mist","Rain_Snow")))%>%
  select(-dteday,-workingday,-mnth,-weathersit,-weekday)

# create indicators since the later process does not take factor 

Month.dummies<-model.matrix(~0+month,data=bike.df)
DOW.dummies<-model.matrix(~0+DOW,data=bike.df)
WorkingDay_Weather.dummies<-model.matrix(~0+WorkingDay:Weather,data=bike.df)
colnames(Month.dummies)<-gsub("month","",colnames(Month.dummies))
colnames(DOW.dummies)<-gsub("DOW","",colnames(DOW.dummies))
colnames(WorkingDay_Weather.dummies)<-gsub("WorkingDay","",colnames(WorkingDay_Weather.dummies))
colnames(WorkingDay_Weather.dummies)<-gsub("Weather","",colnames(WorkingDay_Weather.dummies))
colnames(WorkingDay_Weather.dummies)<-gsub(":","_",colnames(WorkingDay_Weather.dummies))

# for each sets ofindicators, remove one for reference category

y<-bike.df$cnt
X<-as.data.frame(cbind(Month.dummies[,-12],DOW.dummies[,-7],WorkingDay_Weather.dummies[,-6]))

nTotal<-length(y)

nValidation<-90
nTrain<-nTotal-nValidation
xTrain<-X[1:nTrain,]
yTrain<-y[1:nTrain]
xValidation<-X[(nTrain+1):nTotal,]
yValidation<-y[(nTrain+1):nTotal]

yTrain.ts<-ts(yTrain)

formula<-as.formula(paste("yTrain.ts", paste(c("trend", colnames(xTrain)),collapse = "+"),sep="~"))



bike.tslm<-tslm(formula, data=xTrain,lambda=1)

# forecast function will contain both the trained data and the out-of-time validation 

bike.tslm.pred<-forecast(bike.tslm,newdata=xValidation)

plot(bike.tslm.pred,ylim=c(0,9000), xlab="Days", ylab="Daily Bike Rentals")

options(scipen=999, digits=6)
summary(bike.tslm)
