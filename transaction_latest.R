library(foreign)
Baylor <- read.spss("Stubhub_Baylor/transaction.sav", to.data.frame= TRUE)
Baylor<-subset(Baylor, Baylor$Devicetype!="#N/A")
Baylor<-subset(Baylor, Baylor$Devicetype!="Unknown device name")
transaction_last<-read.csv("Stubhub_Baylor/transaction_last.csv")
transaction<-subset(transaction_last, transaction_last$Device!="#N/A")
transaction<-subset(transaction,transaction$Device!="Unknown device name")
## dummy for device
library(dummies)
Baylornew <- dummy.data.frame(Baylor, names = c("Devicetype") , sep = ".")

## remove outliers
Baylornew_withoutoutliers<-subset(Baylornew,ATP<= 150)
Stubhub<-subset(Baylornew_withoutoutliers, Baylornew_withoutoutliers$NUM_TICKETS_SOLD<=12)

transaction_withoutoutliers<-subset(transaction,ATP<= 150)
Stubhub_trans<-subset(transaction_withoutoutliers, transaction_withoutoutliers$NUM_TICKETS_SOLD<=12)
Stubhub_transaction<-as.data.frame(Stubhub_trans)
export(Stubhub_transaction, "D:/Stubhub_Baylor/Stubhub_transactions.csv")
trans_dummies<-read.spss("Stubhub_Baylor/transaction_withdummies_nooutlier.sav", to.data.frame= TRUE)
trans_dummies_withoutoutliers<-subset(trans_dummies,ATP<= 130)
trans_dummies<-subset(trans_dummies_withoutoutliers, trans_dummies_withoutoutliers$NUM_TICKETS_SOLD<=12)

nooutlier<-read.spss("Stubhub_Baylor/nooutlier_transaction.sav", to.data.frame= TRUE)
export(nooutlier, "D:/Stubhub_Baylor/nooutlier_transaction.csv")
# MLE for device type
library(nnet)
Stubhub1$Devicetype2 <- relevel(Stubhub1$Devicetype, ref = "Personal computer  ")
test <- multinom(Devicetype2 ~ Stubhub1$HOURS_TO_EVENT + Stubhub1$NUM_TICKETS_SOLD, Stubhub1$SEASON,  data = Stubhub1)
summary(test)
z <- summary(test)$coefficients/summary(test)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

## Now for ATP











ATP1<-Stubhub$ATP
PC1<-Stubhub$`Devicetype.Personal computer  `  
smartphone1<-Stubhub$`Devicetype.Smartphone         `
tab1<-Stubhub$`Devicetype.Tablet             `

r1aa<-lm(PC1~Stubhub$NUM_TICKETS_SOLD+Stubhub$HOURS_TO_EVENT)
summary(r1aa)
PChat<-fitted(r1aa)

r1bb<-lm(smartphone1~Stubhub$NUM_TICKETS_SOLD+Stubhub$HOURS_TO_EVENT)
summary(r1bb)
Phonehat<-fitted(r1bb)

r1cc<-lm(tab1~Stubhub$NUM_TICKETS_SOLD+Stubhub$HOURS_TO_EVENT)
summary(r1cc)
tabhat<-fitted(r1cc)

ATPtest<-lm(ATP1~PChat+Phonehat+Stubhub$`Devicetype.Tablet             `)
summary(ATPtest)

ATPtrue<-lm(Stubhub$ATP~Stubhub$`Devicetype.Personal computer  `+ Stubhub$`Devicetype.Smartphone         `+Stubhub$`Devicetype.Tablet             `)
summary(ATPtrue)

plot(ATPtrue)

round(cor(Phonehat, PChat),2)