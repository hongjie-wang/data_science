---
title: "housing price index eda for EJW"
author: "Hongjie Wang"
date: "April 24, 2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

We show an example of getting data from web, perform some exploratory data analysis. It is only used as a simple demonstration for EJW to show high level steps.  

In data analysis, one should also start with some questions or hypotheses that one hopes the data can provide some insights. Usually, more specific the questions are, the easier the task and more productive the process.

But sometimes, we are given the task to "find something interesting." Such tasks are actually very difficult to do. But they are good to practice some basic skills. Data science in my opinion is a bit like detective work. You want to bring vigorous logic, rich past experience, solid mathematical knowledge, versatile statistical techniques, a deep domain knowledge and finally your common sense to find useful patterns, make appropriate inferences and reach sensible conclusions and decisions. 

In this demo, we show the steps of the high level analysis steps, not our underlying thinking, nor do we focus on any specific questions or findings. 



First, we load some packages
```{r packges}
rm(list = ls())
library(rvest)
library(tidyverse)
library(ggplot2)
library(GGally)
```

We first obtain data from a table embedded in HTML page
We use the functions in rvest package for this step.

This is a housing price data. You can look at the website to get more information of the data. 

```{r scape}
data_url<-"https://wiki.socr.umich.edu/index.php/SOCR_Data_Dinov_091609_SnP_HomePriceIndex"
wiki_url<- read_html(data_url)

mydata<-wiki_url%>% 
  html_node("table")%>%
  html_table()

```

Some high level summary of the data to make sure all the types 
are correct. It is always a good idea to understand the definitions of data.But in that process, you will need to apply encapsulation. For example, you may want to know a particular field in the data is related to some medical risk factor. And you may want to know that the higher the worse the condition. But you may not need to get into the specific medical science part of it, at least not initially. 

```{r first}
str(mydata)
head(mydata,10)
tail(mydata,5)
summary(mydata)
```
We replace the year and month with a date field.

```{r date}
temp=seq(as.Date('1991-01-01'),as.Date('2009-06-01'),by='month')
mydata<-mydata%>%
  mutate(date_field=temp)%>%
  select(-Year,-Month)
head(mydata)
```

we change the data from wide format to long format so that
we can plot price curves by location 

```{r longplot}
mydata%>%
  select(-Index)%>%
  gather(-date_field,key="market",value="Price")%>%
  ggplot(aes(x=date_field, y=Price, color=market)) +
geom_line(size=1.5) + ggtitle("HomePriceIndex:1991-2009")

  
```

we change the data from wide format to long format so that
we can plot price curves by location 

```{r relations}
subset<-mydata[,10:15]
ggpairs(subset)

```


We can examine one particular market (Boston) more closely

```{r boston}
boston<-mydata$`MA-Boston`

summary(boston)

#standard deviation
sd(boston)

plot(density(boston))

```


Let's examine the relationship between San Francisco Los Angeles more closely.

One way to concisely capture relationship between two random variables is to look at the correlations. It is equivalent to find a simple linear function (or model) like $y_t=\alpha + \beta *x_t + \epsilon_t$ where $y_t$ is the price of SF market at time t, $x_t$ is the LA market. 

$\alpha$ is called intercept and $\beta$ is called slope. In particular, the slope tells us how much SF housing price moves with LA housing prices. 

```{r cfvsla}
CA<-mydata%>%
  select(contains("CA-"))

head(CA)

colnames(CA)<-c("LA","SD","SF")

mymodel<-lm(SF~LA,data=CA)
summary(mymodel)

CA$pred_sf = predict(mymodel,data=CA)

ggplot(data=CA, aes(x = LA)) +
geom_point(aes(y = SF)) +
geom_line(aes(y = pred_sf), color='Magenta', size=2) +
ggtitle("PredictHomeIndex SF - LA")

  
```

Final example, we want to see if the relationship between SF and LA changes over time. Although not applicable, but this is the same concept as in pair trade in stock. If you have two stocks A and B and you believe their price relationship in the long-term should be stable. If you then a significant deviation of one stock's price, you could buy or sell, in anticipation of the relationship going back to normal in the near future. 

```{r evolve}

mydata<-mydata%>%
  select(`CA-SanFrancisco`,`CA-LosAngeles`,date_field)%>%
  rename(SF=`CA-SanFrancisco`,LA=`CA-LosAngeles`)


model_intercepts<-numeric(11)
model_beta<-numeric(11)
for (i in 1:11){
  temp<-mydata[(i-1)*20+1:i*20,]
  mymodel<-lm(SF~LA,data=temp)
  model_intercepts[i]<-mymodel$coefficients[1]
  model_beta[i]<-mymodel$coefficients[2]
}


par(mfrow=c(2,2)) 
plot(model_intercepts)
plot(model_beta)
plot(model_intercepts,model_beta)



```