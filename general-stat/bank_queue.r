# simulating of bank queue with one teller
# the customers come in with Possion rate
# they have exp duration needs for service
# processed in order

library(tidyverse)
library(ggplot2)

set.seed(1234)

# function to form a dataframe of customers
# each min, poission nbr of customers come with income_rate
# customers will need service with exp(service_time)
# it returns a dataframe with id, arrival time, endtime
# endtime start as inf
customer_queue<-function(income_rate,time=60,service_time){
  #poisson process runs for time mins with rate per min
  customers_per_min<-rpois(time,lambda=income_rate)
  # define a vector whose length is total nbr of cust
  arrival<-numeric(sum(customers_per_min))
  position<-1
  for (i in 1:time){
    numcust<-customers_per_min[i]
    if(numcust!=0){
      arrival[position:(position+numcust-1)]<-rep(i,numcust)
      position<-position+numcust
    }
  }
  duration <- rexp(length(arrival), rate = 1/service_time) 
  queue_df <- data.frame(arrival, duration, custnum = 1:length(duration),
                   endtime = Inf, stringsAsFactors = FALSE)
  return (queue_df)
}

# function to see if any customers left not finished transaction yet
any_active<-function(df){
  return(max(df$endtime) == Inf) 
}

next_customer<-function(df) {
  return(df%>%
    filter(endtime==Inf)%>%
    arrange(arrival)%>%
    head(1))
}

update_customer<-function(df,id,end_time){
  return(mutate(df,endtime=ifelse(custnum==id,end_time,endtime)))
}


# main program

# form the queue first
customers<-customer_queue(0.5,time=100,1.2)

# start simulation
clock <- 0 # set up beginning of simulation

# keep running until no customers left

while (any_active(customers)){
  next_one <- next_customer(customers)
  now <- ifelse(next_one$arrival >= clock, next_one$arrival, clock)
  clock <- now + next_one$duration
  customers <- update_customer(customers, next_one$custnum, clock)
}

customers<-customers%>%
  mutate(total_time=endtime-arrival)



summary(customers)
