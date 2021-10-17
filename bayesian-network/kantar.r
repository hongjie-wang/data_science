library(bnlearn)
scenario<-empty.graph(nodes=c("outbreak","consumer","biz"))
scenario <- set.arc(scenario, from = "outbreak", to = "consumer")
scenario <- set.arc(scenario, from = "outbreak", to = "biz")
scenario <- set.arc(scenario, from = "consumer", to = "biz")
plot(scenario)
outbreak.lv<-c("one","multi")
consumer.lv<-c("control","panic")
biz.lv<-lv<-c("fragmented","comprehensive")
outbreak.prob<-array(c(0.8,0.2),dim=2,dimnames=list(outbreak=outbreak.lv))
consumer.prob<-array(c(0.75,0.25,0.3,0.7),dim=c(2,2),dimnames=list(consumer=consumer.lv,outbreak=outbreak.lv))
biz.prob<-array(c(0.7,0.3,0.2,0.8,0.9,0.1,0.1,0.9),dim=c(2,2,2),dimnames=list(biz=biz.lv,outbreak=outbreak.lv,consumer=consumer.lv))

cpt <- list(outbreak = outbreak.prob, consumer=consumer.prob, biz = biz.prob)
bn<- custom.fit(scenario, cpt)

path(bn,from="biz",to="consumer")

cpquery(bn,event=(biz=="comprehensive") & (consumer=="panic"),evidence=(outbreak=="one"))

prop.table(table(cpdist(bn,nodes=c("biz"),evidence=(outbreak=="one"))))

prop.table(table(cpdist(bn,nodes=c("biz"),evidence=(outbreak=="multi"))))