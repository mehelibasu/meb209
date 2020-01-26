getwd()
traffic = read.csv("trafficfile_foranalysis.csv")
read<-with(traffic,aggregate(cbind(traffic$Device) ~ traffic$Identifier,FUN=function(x){length(unique(x))}))

library(foreign)
write.foreign(read, "mydata.txt", "c:/mydata.sps",   package="SPSS")

library(xlsx)
write.xlsx(read, "D:/mydata.xlsx")

colnames(read, do.NULL = FALSE)
colnames(read) <- c("Identifier","Numofdev")
totaltraffic <- merge(traffic,read,by="Identifier")

install.packages("WriteXLS")
#library(WriteXLS)
#WriteXLS("totaltraffic",ExcelFileName="totaltraffic_foranalysis.xlsx",row.names=F,col.names=T)
#write.xlsx(totaltraffic, "D:/totaltraffic_foranalysis.xlsx")
write.csv(totaltraffic, file = "totaltraffic_foranalysis.csv", row.names = FALSE)
library(plyr)
read2<-count(totaltraffic, c("Identifier", "Device= Personal computer"))
colnames(read2, do.NULL = FALSE)
colnames(read2) <- c("Identifier","Device", "Num of times dev")
totaltraffic <- merge(traffic,read,by="Identifier")

trafficfull = read.csv("totaltrafficforanalysis_full.csv")
trafficfull_new<-trafficfull[!(trafficfull$Page=="buyersside" | trafficfull$Page == "Home" | trafficfull$Page == "Other" | trafficfull$Page == "Undefined"),]
write.csv(trafficfull_new, file = "totaltraffic_foranalysis2.csv", row.names = FALSE)

trafficfull2 = read.csv("totaltraffic_foranalysis_2.csv")

totalPCpages<-aggregate(PC ~ trafficfull2$ï..Identifier, trafficfull2, sum)
colnames(trafficfull2, do.NULL = FALSE)
colnames(totalPCpages) <- c("Identifier","PCpages")
colnames(trafficfull2)[1] <- "Identifier"
totaltraffic2 <- merge(trafficfull2,totalPCpages,by="Identifier")

totalphonepages<-aggregate(totaltraffic2$Smartphone ~ totaltraffic2$Identifier, totaltraffic2, sum)
colnames(totalphonepages, do.NULL = FALSE)
colnames(totalphonepages) <- c("Identifier","Phonepages")
totaltraffic3 <- merge(totaltraffic2,totalphonepages,by="Identifier")

totaltabpages<-aggregate(totaltraffic3$Tablet ~ totaltraffic3$Identifier, totaltraffic3, sum)
colnames(totaltabpages, do.NULL = FALSE)
colnames(totaltabpages) <- c("Identifier","Tabpages")
totaltraffic4 <- merge(totaltraffic3,totaltabpages,by="Identifier")

Cartpages<-aggregate(totaltraffic4$Page_Cart ~ totaltraffic4$Identifier, totaltraffic4, sum)
colnames(Cartpages, do.NULL = FALSE)
colnames(Cartpages) <- c("Identifier","Cartpages")
totaltraffic5 <- merge(totaltraffic4,Cartpages,by="Identifier")


Eventpages<-aggregate(totaltraffic5$Page_Event ~ totaltraffic5$Identifier, totaltraffic5, sum)
colnames(Eventpages, do.NULL = FALSE)
colnames(Eventpages) <- c("Identifier","Eventpages")
totaltraffic6 <- merge(totaltraffic5,Eventpages,by="Identifier")

Eventdetailspages<-aggregate(totaltraffic6$Page_Eventdetails ~ totaltraffic6$Identifier, totaltraffic6, sum)
colnames(Eventdetailspages, do.NULL = FALSE)
colnames(Eventdetailspages) <- c("Identifier","Eventdetailspages")
totaltraffic7 <- merge(totaltraffic6,Eventdetailspages,by="Identifier")


Seatmappages<-aggregate(totaltraffic7$Page_Seatmap ~ totaltraffic7$Identifier, totaltraffic7, sum)
colnames(Seatmappages, do.NULL = FALSE)
colnames(Seatmappages) <- c("Identifier","Seatmappages")
totaltraffic8 <- merge(totaltraffic7,Seatmappages,by="Identifier")

Checkoutpages<-aggregate(totaltraffic8$Page_Checkout ~ totaltraffic8$Identifier, totaltraffic8, sum)
colnames(Checkoutpages, do.NULL = FALSE)
colnames(Checkoutpages) <- c("Identifier","Checkoutpages")
totaltraffic9 <- merge(totaltraffic8,Checkoutpages,by="Identifier")

Orderplacedpages<-aggregate(totaltraffic9$Page_Orderplaced ~ totaltraffic9$Identifier, totaltraffic9, sum)
colnames(Orderplacedpages, do.NULL = FALSE)
colnames(Orderplacedpages) <- c("Identifier","Orderplacedpages")
totaltraffic10 <- merge(totaltraffic9,Orderplacedpages,by="Identifier")

totaltraffic10$date<-as.Date(as.POSIXct(totaltraffic10$baylor_traffic.hit_time_gmt, origin="1970-01-01"))
install.packages("chron")
library(chron)
totaltraffic10$weekend<-is.weekend(totaltraffic10$date)

write.csv(totaltraffic10, file = "totaltraffic_foranalysis_5.csv", row.names = FALSE)

dates = read.csv("Dates.csv")
totaltraffic11<-read.csv("totaltraffic_foranalysis_5.csv")
colnames(dates)[1] <- "EventDescription"
colnames(dates)[2] <- "EventDate"
totaltraffic12 <- merge(totaltraffic11,dates,by="EventDescription")
write.csv(totaltraffic12, file = "totaltraffic_foranalysis_6.csv", row.names = FALSE)
colnames(totaltraffic12)[2]<-"Identifier"


onlysessions<-data.frame(totaltraffic12$Identifier,totaltraffic12$Session)
colnames(onlysessions) <- c("Identifier","Session")

install.packages("magrittr")
library(magrittr)

firstsession <- onlysessions %>%
  group_by(Identifier) %>%
  arrange(Session) %>%
  slice(1) %>%
  ungroup
colnames(firstsession) <- c("Identifier","FirstSession")

lastsession <- onlysessions %>%
  group_by(Identifier) %>%
  arrange(Session) %>%
  slice(n()) %>%
  ungroup
colnames(lastsession) <- c("Identifier","LastSession")

firstandlast<-merge(firstsession,lastsession,by="Identifier")
totaltraffic13<-merge(totaltraffic12,firstandlast,by="Identifier")
write.csv(totaltraffic13, file = "totaltraffic_foranalysis_7.csv", row.names = FALSE)

totaltraffic14<-read.csv("totaltraffic_foranalysis_7.csv")
first<-read.csv("firstsession.csv")
colnames(first)[1]<-"Identifier"
firstsessions<-data.frame(first$Identifier, first$firstsessiondate)
colnames(firstsessions)[1]<-"Identifier"
colnames(firstsessions)[2]<-"firstsessiondate"

last<-read.csv("lastsession.csv")
colnames(last)[1]<-"Identifier"
lastsessions<-data.frame(last$Identifier, last$lastsessiondate)
colnames(lastsessions)[1]<-"Identifier"
colnames(lastsessions)[2]<-"lastsessiondate"

totaltraffic15 <- merge(totaltraffic14,firstsessions,by="Identifier")


import<-read.csv("import.csv")
colnames(import)[1]<-"Identifier"
transaction<-read.csv("transaction_match2.csv")
transaction_full<-merge(transaction,import,by="Identifier" )

write.csv(transaction_full, file = "transaction_match2.csv", row.names = FALSE)



totaltraffic16<-read.csv("trafficforanalysis_newest_June2018_withalldata.csv")
agg1<-aggregate(totaltraffic16$baylor_traffic.hit_time_gmt ~ totaltraffic16$Identifiersession, totaltraffic16, max)
colnames(agg1) <- c("Identifiersession","maxtime")

agg2<-aggregate(totaltraffic16$baylor_traffic.hit_time_gmt ~ totaltraffic16$Identifiersession, totaltraffic16, min)
colnames(agg2) <- c("Identifiersession","mintime")

totatltraffic17<-merge(totaltraffic16,agg1,by="Identifiersession")
totatltraffic18<-merge(totatltraffic17,agg2,by="Identifiersession")
totatltraffic18$sessiontime<-(totatltraffic18$maxtime-totatltraffic18$mintime)

colnames(totatltraffic18)[2]<-"Identifier"
agg3<-aggregate(totatltraffic18$sessiontime ~ totatltraffic18$Identifier, totatltraffic18, mean)
colnames(agg3)<-c("Identifier", "mean session time")
totatltraffic19<-merge(totatltraffic18, agg3, by="Identifier")

write.csv(totatltraffic19, file = "traffic_foranalysis_Junelatest.csv", row.names = FALSE)










transaction_march = read.csv("transaction_march2018.csv")
trafficjuly=read.csv("file to match traffic and transaction.csv")
transaction_march_new <- merge(transaction_march,trafficjuly,by="BUYER_ID")
write.csv(transaction_march_new, file = "transaction_march_new", row.names = FALSE)


trafficJuly = read.csv("trafficforanalysis_July2018.csv")
trafficcases2<-unique(trafficJuly$BUYER_ID)

traff = read.csv("trafficfile_foranalysis.csv")
traffcases<-unique(traff10$BUYER_ID)
traffpurchase<-subset(traff, traff$Purchase ==1)
traffnopurchase<-subset(traff, traff$Purchase ==0)
traffpurchasecases<-unique(traffpurchase$Identifier)
traffnopurchasecases<-unique(traffnopurchase$Identifier)


traff10= read.csv("traffic_descriptivestates_paper.csv")

library(plyr)
ff<-ddply(traff10, ~traff10$Identifier,summarise,number_of_sessions=unique(traff10$Session))


transaction = read.csv ("transaction_march2018.csv")
transactioncases<-unique(transaction$BUYER_ID.1)


length(transactioncases)






