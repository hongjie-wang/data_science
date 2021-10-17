# example from SHMUELI & LICHTENDAHL book on practical time series 

library(tidyverse)
library(lubridate)
library(forecast)
walmart.df<-read.csv('C:/Users/hongjie/Documents/data_science/time-series/data/Walmart_One_Pair.csv')

# create time series
nTrain<-143
yTrain.ts<-ts(walmart.df$Weekly_Sales[1:nTrain],freq=52,start=c(2011,5))
# periotic makes the seasonable pattern static over time
stl.run<-stl(yTrain.ts, s.window="periodic")
# notice the auto relationship patterns in residuals
plot(stl.run)

# create a dataframe of x variables in this case just holiday indicators
xTrain<-walmart.df%>%
  select(IsHoliday)%>%
  mutate(IsHoliday=as.numeric(IsHoliday))%>%
  slice(1:nTrain)


summary(xTrain)
# remove the seasonal
# we remove seasonal (or trends) before fitting arma

season.comp<-stl.run$time.series[,1]
deseasoned<-yTrain.ts-season.comp
arima.fit.deseason<-auto.arima(deseasoned,xreg=as.matrix(xTrain))

# this can be done directly using stlm 

nTest<-39
xTest<-data.frame(IsHoliday=as.numeric(walmart.df$IsHoliday[(nTrain+1):(nTrain+nTest)]))
# fit a model with season, xreg and rest with arima 
reg.fit<-stlm(yTrain.ts,s.window="periodic",xreg=as.matrix(xTrain),method="arima")
summary(reg.fit$model)

pred.ts<-forecast(reg.fit,xreg=as.matrix(xTest), h=nTest)
plot(pred.ts, xlab="year", ylab="weekly sales")
