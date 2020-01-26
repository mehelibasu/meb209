memory.limit()
memory.limit(size=80000)


Boston_4 <- read.csv("Boston_bigfile_new_April_new_7.csv")
Boston_4 <- Boston_3[ which(Boston_3$WeeklyCategorylevelPricepromos<=70),]

Boston_4$shopperhealthinessXweeklypricecutcategorypromoswithNuVal = Boston_4$shopperhealthiness * Boston_4$weeklycategorylevelpricecutwithNuValpromos.y
Boston_4$shopperhealthinessXweeklyfreewithcategorypromoswithNuVal = Boston_4$shopperhealthiness * Boston_4$weeklycategorylevelfreeproductwithNuValpromos.y
Boston_4$shopperspendingXweeklypricecutcategorypromoswithNuVal = Boston_4$monthlyaverage. * Boston_4$weeklycategorylevelpricecutwithNuValpromos.y
Boston_4$shopperspendingXweeklyfreewithcategorypromoswithNuVal = Boston_4$monthlyaverage. * Boston_4$weeklycategorylevelfreeproductwithNuValpromos.y


totalpromos = aggregate(Boston_3$WeeklyCategorylevelPricepromos ~ Boston_3$Departmentyearweek, data = Boston_3, FUN = sum)


Boston_3$ProportionofcategoryadswithNuVal = Boston_3$WeeklyCategoryleveNuValpromos/Boston_3$WeeklyCategorylevelPricepromos
Boston_3$ProportionofcategoryadswithNuVal[Boston_3$WeeklyCategorylevelPricepromos==0]=0
Boston_3$proportionof_pricecutpromoswithNuVal = Boston_3$weeklycategorylevelpricecutwithNuValpromos.y/Boston_3$weeklycategorylevelpricecutpromos.y
###Boston_3$proportionof_pricecutpromoswithNuVal_1 = aggregate(Boston_3$weeklycategorylevelpricecutwithNuValpromos.y/Boston_3$weeklycategorylevelpricecutpromos.y ~ Boston_3$Departmentyearweek, data = Boston_3)
Boston_3$proportionof_freeproductwithpromoswithNuVal = Boston_3$weeklycategorylevelfreeproductwithNuValpromos.y/Boston_3$weeklycategorylevelfreeproductpromos.y
Boston_3$proportionof_pricecutpromoswithNuVal[Boston_3$weeklycategorylevelpricecutpromos.y==0]=0
Boston_3$proportionof_freeproductwithpromoswithNuVal[Boston_3$weeklycategorylevelfreeproductpromos.y==0]=0
Boston_3$proportionof_page1promoswithNuVal = Boston_3$WeeklyCategorylevelNuValpromosonPage1/Boston_3$WeeklyCategorylevelpromosonPage1
Boston_3$proportionof_page1promoswithNuVal[Boston_3$WeeklyCategorylevelpromosonPage1==0]=0
dim(Boston_3)

Boston_3[is.na(Boston_3)] <- 0

Boston_5 <- Boston_4[ -c(1:4) ]
library('dplyr')
write.csv(Boston_5, file = 'Boston_bigfile_new_April_new_7.csv')

Boston_2$weeklycategoryadarea = 0.25*Boston_2$sizeAads + 0.166*Boston_2$sizeBads + 0.125*Boston_2$sizeCads
Boston_2$adareaXNuVal = Boston_2$categoryNuVal*Boston_2$weeklycategoryadarea
Boston_2$adsonfirstpageXNuVal = Boston_2$categoryNuVal * Boston_2$totaladsonfirst3pages
Boston_2$priceperunit = Boston_2$netSales/Boston_2$quantityPurchased
Boston_2$priceperunit[Boston_2$quantityPurchased==0]=0
Boston_2 <- Boston_2[, -c(1:6)]

promotype<-read.csv("promotype.csv")
Boston_2 <- left_join(Boston_2, promotype, by = "promotype_1")

freeproduct = aggregate(Boston_2$Free.Product ~ Boston_2$Departmentyearweek, data = Boston_2, FUN = count)
names(freeproduct) <- c("Departmentyearweek", "weeklycategorylevelfreeproductpromos")

pricecut = aggregate(Boston_2$Price.Cut ~ Boston_2$Departmentyearweek, data = Boston_2, FUN = count)
names(pricecut) <- c("Departmentyearweek", "weeklycategorylevelpricecutpromos")
dept_NuVal = aggregate(Boston_4$NuValscore ~ Boston_4$department, data = Boston_4, FUN = mean)

Boston_4$highernutritioncategory <- 0

Boston_4$highernutritioncategory[Boston_4$department== 'Produce' | Boston_4$department== 'Seafood'] <- 1
table(freq(Boston_4$highernutritioncategory))

Boston_4

Boston_4$PromotionXpricecut = Boston_4$Price.Cut * Boston_4$Promotion
Boston_4$NuValXpricecut = Boston_4$Price.Cut * Boston_4$NuVal_1
Boston_4$highernutritioncategoryXpricecut = Boston_4$highernutritioncategory * Boston_4$PromotionXpricecut
Boston_4$highernutritioncategoryXNuValpricecut = Boston_4$highernutritioncategory * Boston_4$NuValXpricecut


## effect of promotions on sales
library(car)
fit_totalmodel <- lm(Boston_4$netSales~ Boston_4$Promotion + Boston_4$NuVal_1 + Boston_4$PromotionXpricecut + Boston_4$NuValXpricecut + Boston_4$categorypromotion + Boston_4$categoryNuVal + Boston_4$categorypromowithnoproductpromo + Boston_4$categoryNuValwithnoproductpromo + Boston_4$highernutritioncategory + Boston_4$highernutritioncategoryXpricecut + Boston_4$highernutritioncategoryXNuValpricecut + Boston_4$shopperhealthiness + Boston_4$shopperhealthinesXpromo + Boston_4$shopperhealthinesXNuVal + Boston_4$monthlyaverage.+ Boston_4$monthlyaveragespendingXpromo + Boston_4$monthlyaveragespendingXNuVal + Boston_4$priceperunit + Boston_4$ï..weeknumber + factor(Boston_4$department), data = Boston_4)
summary(fit_totalmodel)
vif(fit_totalmodel)

Boston_4$NuValonpromo = Boston_4$Promotion * Boston_4$NuVal_1
fit_totalmodel_2 <- lm(Boston_4$netSales~ Boston_4$Promotion + Boston_4$NuValonpromo + Boston_4$PromotionXpricecut + Boston_4$NuValXpricecut + Boston_4$categorypromotion + Boston_4$categoryNuVal + Boston_4$categorypromowithnoproductpromo + Boston_4$categoryNuValwithnoproductpromo + Boston_4$highernutritioncategory + Boston_4$highernutritioncategoryXpricecut + Boston_4$highernutritioncategoryXNuValpricecut + Boston_4$shopperhealthiness + Boston_4$shopperhealthinesXpromo + Boston_4$shopperhealthinesXNuVal + Boston_4$monthlyaverage.+ Boston_4$monthlyaveragespendingXpromo + Boston_4$monthlyaveragespendingXNuVal + Boston_4$priceperunit + Boston_4$ï..weeknumber + factor(Boston_4$department), data = Boston_4)
summary(fit_totalmodel_2)
vif(fit_totalmodel_2)


fit_totalmodel_3 <- lm(Boston_4$netSales~ Boston_4$Promotion + Boston_4$NuValonpromo + Boston_4$NuValXpricecut + Boston_4$categoryNuVal + Boston_4$categoryNuValwithnoproductpromo + Boston_4$highernutritioncategory + Boston_4$highernutritioncategoryXNuValpricecut + Boston_4$shopperhealthiness + Boston_4$shopperhealthinesXNuVal + Boston_4$monthlyaverage.+ Boston_4$monthlyaveragespendingXNuVal + Boston_4$priceperunit + Boston_4$ï..weeknumber, data = Boston_4)
summary(fit_totalmodel_3)
vif(fit_totalmodel_3)





Boston_2 <- left_join(Boston_2, pricecut, by ="Departmentyearweek")
Boston_2 <- left_join(Boston_2, freeproduct, by ="Departmentyearweek")

Boston_2$freeproductwithNuVal = Boston_2$NuVal_1*Boston_2$Free.Product
Boston_2$pricecutwithNuVal = Boston_2$NuVal_1*Boston_2$Price.Cut

freeproductwithNuVal = aggregate(Boston_2$freeproductwithNuVal ~ Boston_2$Departmentyearweek, data = Boston_2, FUN = count)
names(freeproductwithNuVal) <- c("Departmentyearweek", "weeklycategorylevelfreeproductwithNuValpromos")

pricecutwithNuVal = aggregate(Boston_2$pricecutwithNuVal ~ Boston_2$Departmentyearweek, data = Boston_2, FUN = count)
names(pricecutwithNuVal) <- c("Departmentyearweek", "weeklycategorylevelpricecutwithNuValpromos")

Boston_2 <- left_join(Boston_2, pricecutwithNuVal, by ="Departmentyearweek")
Boston_2 <- left_join(Boston_2, freeproductwithNuVal, by ="Departmentyearweek")

Boston_2$noproductlevelpromo = 0
Boston_2$noproductlevelpromo[Boston_2$NuVal_1==0]=1

Boston_2$categorypromowithnoproductpromo = Boston_2$categorypromotion * Boston_2$noproductlevelpromo
Boston_2$categoryNuValwithnoproductpromo = Boston_2$categoryNuVal * Boston_2$noproductlevelpromo

Boston_2[is.na(Boston_2)]<-0
names(Boston_2)

Boston_3 <- Boston_2[,-c(66,67,70,71)]

write.csv(Boston_3, file = 'Boston_bigfile_new_April_new_2.csv')

somepromovariables <- read.csv("somepromovariables.csv")
library("dplyr")
Boston_2 <- left_join(Boston_2, somepromovariables, by ="Departmentyearweek")
## neural network

install.packages("tidyverse")
install.packages("neuralnet")
install.packages("GGally")

library(tidyverse)
library(neuralnet)
library(GGally)

Boston_2
na.omit(Boston_2)

X <- Boston_2[,c("categorypromotion", "categoryNuVal", " ",  "shopperhealthiness", "monthlyaverage.", "weeklycategoryadarea", "totaladsonfirst3pages")]



#data_train <- Boston_2[c(1:20000, 2320117:6320117), ]
#data_test <- Boston_2[20001:2320116, ]
X_train <- data.frame(data_train$categorypromotion, data_train$categoryNuVal, data_train$shopperhealthiness, data_train$monthlyaverage., data_train$sizeBads, data_train$sizeAads, data_train$totaladsonfirst3pages)
Y_train <- data.frame(data_train$netSales)
X_test <- data.frame(data_test$categorypromotion, data_test$categoryNuVal, data_test$shopperhealthiness, data_test$monthlyaverage., data_test$sizeBads, data_test$sizeAads, data_test$totaladsonfirst3pages)
Y_test <- data.frame(data_test$netSales)

install.packages('neuralnet')
require(neuralnet)
nn=neuralnet(netSales~categorypromotion +categoryNuVal+ shopperhealthiness+monthlyaverage.+ sizeBads+ sizeAads+ totaladsonfirst3pages  , data = data_train, hidden=3,
             linear.output = FALSE)
plot(nn)
Predict=compute(nn,X_test)
Predict$net.result
pr.nn_ <- Predict$net.result*(max(Boston_2$netSales)-min(Boston_2$netSales))+min(Boston_2$netSales)
test.r <- (data_test$netSales)*(max(Boston_2$netSales)-min(Boston_2$netSales))+min(Boston_2$netSales)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(data_test)

lm.fit <- lm(netSales~categorypromotion +categoryNuVal+ shopperhealthiness+monthlyaverage.+ sizeBads+ sizeAads+ totaladsonfirst3pages  , data = data_train)
summary(lm.fit)
pr.lm <- predict(lm.fit,data_test)
MSE.lm <- sum((pr.lm - data_test$netSales)^2)/nrow(data_test)

print(paste(MSE.lm,MSE.nn))



promo <- read.csv("Promo_2.csv")
memory.limit(size=90000)
promo[,3] = as.integer(promo[,3])
Boston <- read.csv("Boston_small_new.csv")

## steps to get the weekly sales per category
sales = aggregate(Boston$netSales ~ Boston$Departmentyearweek, data = Boston, FUN = sum)
names(sales) <- c("Departmentyearweek", "weeklysales")
library("dplyr")
Boston_2 <- left_join(Boston, sales, by ="Departmentyearweek")

## steps to get variables category promo and category NuVal
Boston_2 <- left_join(Boston_2, promo, by ="Departmentyearweek")
Boston_2$categorypromotion[is.na(Boston_2$categorypromotion)]<- 0
Boston_2$categoryNuVal[is.na(Boston_2$categoryNuVal)]<- 0
Boston_2$NuVal_1[is.na(Boston_2$NuVal_1)]<- 0
Boston_2$Promotion[is.na(Boston_2$Promotion)]<- 0

## steps to get weighted average shopper healthiness
Boston_2$purchasequality = Boston_2$quantityPurchased * Boston_2$NuValscore
householdpurchase = aggregate (Boston_2$purchasequality ~ Boston_2$shopperID, data =Boston_2, FUN = sum)
household_quantity = aggregate(Boston_2$quantityPurchased ~ Boston_2$shopperID, data = Boston_2, FUN = sum)
household <-merge(householdpurchase, household_quantity)
household$shopperhealthiness = household$`Boston_2$purchasequality`/household$`Boston_2$quantityPurchased`
shopper<-household[,c(1,4)]
names(shopper)<- c("shopperID", "shopperhealthiness")
Boston_2 <- left_join(Boston_2, shopper, by ="shopperID")
Boston_2$purchasequality = NULL



## interaction terms of shopper healthiness and promo
Boston_2$shopperhealthinesXpromo = Boston_2$shopperhealthiness * Boston_2$Promotion
Boston_2$shopperhealthinesXNuVal = Boston_2$shopperhealthiness * Boston_2$NuVal_1
Boston_2$shopperhealthinesXcategorypromo = Boston_2$shopperhealthiness * Boston_2$categorypromotion
Boston_2$shopperhealthinesXcategoryNuVal = Boston_2$shopperhealthiness * Boston_2$categoryNuVal


## steps to calculate monthly average spending of shopper

library(lubridate)
Boston_2$month <-month(as.POSIXlt(as.Date(Boston_2$date), format="%Y/%m/%d"))
Boston_2$yearmonth <- paste(Boston_2$year, Boston_2$month, collapse = NULL)
Boston_2$useryearmonth <- paste(Boston_2$shopperID, Boston_2$yearmonth, collapse = NULL)
monthlytotal = aggregate( Boston_2$netSales~ Boston_2$useryearmonth, data = Boston_2, FUN = sum)
names(monthlytotal) <- c("useryearmonth", "monthlytotal")
library('dplyr')
Boston_2 <-left_join(Boston_2,monthlytotal, by="useryearmonth")

monthlyaverage = aggregate( Boston_2$monthlytotal~ Boston_2$shopperID, data = Boston_2, FUN = mean)
names(monthlyaverage) <- c("shopperID", "monthlyaverage ")
library('dplyr')
Boston_2 <-left_join(Boston_2,monthlyaverage, by="shopperID")


## interaction terms of shopper monthlyaverage and promo
Boston_2$monthlyaveragespendingXpromo = Boston_2$`monthlyaverage ` * Boston_2$Promotion
Boston_2$monthlyaveragespendingXNuVal = Boston_2$`monthlyaverage ` * Boston_2$NuVal_1
Boston_2$monthlyaveragespendingXcategorypromo = Boston_2$`monthlyaverage ` * Boston_2$categorypromotion
Boston_2$monthlyaveragespendingXcategoryNuVal = Boston_2$`monthlyaverage ` * Boston_2$categoryNuVal

## steps to calculate NuVal diff

NuValmin = aggregate( Boston_2$NuValscore~ Boston_2$department, data = Boston_2, FUN = min)
NuValmax = aggregate( Boston_2$NuValscore~ Boston_2$department, data = Boston_2, FUN = max)
dep = merge(NuValmin , NuValmax, by ="Boston_2$department")
names(dep) <- c('department', 'NuValmin', 'NuValmax')
dep$NuValrange = dep$NuValmax - dep$NuValmin
dep2 <- dep[ ,c(1,4) ]
library('dplyr')
Boston_2 <-left_join(Boston_2,dep2, by="department")
Boston_2$NuValdiff = (Boston_2$NuValscore - Boston_2$meanNuVal)/ Boston_2$NuValrange


## interaction terms of item NuVal difference from mean NuVal and promo
Boston_2$NuValdiffXpromo = Boston_2$NuValdiff* Boston_2$Promotion
Boston_2$NuValdiffXNuVal = Boston_2$NuValdiff* Boston_2$NuVal_1
Boston_2$NuValdiffXcategorypromo = Boston_2$NuValdiff * Boston_2$categorypromotion
Boston_2$NuValdiffXcategoryNuVal = Boston_2$NuValdiff * Boston_2$categoryNuVal

write.csv(Boston_4, file = 'Boston_bigfile_4.csv')

## Impact of category level promotions on category level sales

fit <- lm(Boston_2$weeklysales ~ Boston_2$categorypromotion + Boston_2$categoryNuVal + Boston_2$weekofyear + factor(Boston_2$department), data=Boston_2)
summary(fit)

Boston_3[is.na(Boston_3 )] <- 0

## Impact of category level promotions on transaction level sales

##fit2a<- lm(Boston_2$netSales ~ Boston_2$categorypromotion + Boston_2$categoryNuVal + Boston_2$priceperunit +Boston_2$weekofyear + factor(Boston_2$department) + Boston_2$shopperhealthiness + Boston_2$monthlyaverage. + Boston_2$weeklycategoryadarea + Boston_2$totaladsonfirst3pages, data = Boston_2)
##summary(fit2a)

fit2<- lm(Boston_3$netSales ~ Boston_3$priceperunit + Boston_3$categorypromotion + Boston_3$categoryNuVal + Boston_3$ï..weeknumber + factor(Boston_3$department) + Boston_3$shopperhealthiness + Boston_3$shopperhealthinesXcategorypromo + Boston_3$shopperhealthinesXcategoryNuVal + Boston_3$monthlyaverage. + Boston_3$monthlyaveragespendingXcategorypromo + Boston_3$monthlyaveragespendingXcategoryNuVal+ Boston_3$WeeklyCategorylevelpromosonPage1 + Boston_3$WeeklyCategorylevelNuValpromosonPage1 + Boston_3$WeeklyCategorylevelsizeCpromos+ Boston_3$WeeklyCategorylevelsizeCNuValpromos +Boston_3$WeeklyCategorylevelsizeBpromos+ Boston_3$WeeklyCategorylevelsizeBNuValpromos +Boston_3$WeeklyCategorylevelsizeApromos+ Boston_3$WeeklyCategorylevelsizeANuValpromos + Boston_3$weeklycategorylevelpricecutpromos.y+ Boston_3$weeklycategorylevelpricecutwithNuValpromos.y+ Boston_3$weeklycategorylevelfreeproductpromos.y+ Boston_3$weeklycategorylevelfreeproductwithNuValpromos.y, data = Boston_3)
summary(fit2)

fit2b<- lm(Boston_3$netSales ~ Boston_3$priceperunit +Boston_3$categorypromotion + Boston_3$categoryNuVal + Boston_3$categorypromowithnoproductpromo+ Boston_3$categoryNuValwithnoproductpromo  +Boston_3$ï..weeknumber + factor(Boston_3$department) + Boston_3$shopperhealthiness + Boston_3$shopperhealthinesXcategorypromo + Boston_3$shopperhealthinesXcategoryNuVal + Boston_3$monthlyaverage. + Boston_3$monthlyaveragespendingXcategorypromo + Boston_3$monthlyaveragespendingXcategoryNuVal+ Boston_3$WeeklyCategorylevelpromosonPage1 + Boston_3$WeeklyCategorylevelNuValpromosonPage1 + Boston_3$WeeklyCategorylevelsizeCpromos+ Boston_3$WeeklyCategorylevelsizeCNuValpromos +Boston_3$WeeklyCategorylevelsizeBpromos+ Boston_3$WeeklyCategorylevelsizeBNuValpromos +Boston_3$WeeklyCategorylevelsizeApromos+ Boston_3$WeeklyCategorylevelsizeANuValpromos + Boston_3$weeklycategorylevelpricecutpromos.y + Boston_3$weeklycategorylevelpricecutwithNuValpromos.y+ Boston_3$weeklycategorylevelfreeproductpromos.y+ Boston_3$weeklycategorylevelfreeproductwithNuValpromos.y, data = Boston_3)
summary(fit2b)



library(car)
fit2c<- lm(Boston_4$netSales ~ Boston_4$priceperunit + Boston_4$ï..weeknumber + factor(Boston_4$department) + Boston_4$shopperhealthiness + Boston_4$shopperhealthiness * Boston_4$weeklycategorylevelpricecutpromos.y + Boston_4$shopperhealthiness * Boston_4$proportionof_pricecutpromoswithNuVal + Boston_4$shopperhealthiness * Boston_4$weeklycategorylevelfreeproductpromos.y + Boston_4$shopperhealthiness * Boston_4$proportionof_freeproductwithpromoswithNuVal +Boston_4$monthlyaverage. + Boston_4$monthlyaverage.* Boston_4$weeklycategorylevelpricecutpromos.y + Boston_4$monthlyaverage.* Boston_4$proportionof_pricecutpromoswithNuVal+ Boston_4$monthlyaverage.* Boston_4$weeklycategorylevelfreeproductpromos.y+ Boston_4$monthlyaverage.* Boston_4$proportionof_freeproductwithpromoswithNuVal + Boston_4$weeklycategorylevelpricecutpromos.y+ Boston_4$proportionof_pricecutpromoswithNuVal+ Boston_4$weeklycategorylevelfreeproductpromos.y+ Boston_4$proportionof_freeproductwithpromoswithNuVal, data = Boston_4)
summary(fit2c)
vif(fit2c)

fit2d<- lm(Boston_4$netSales ~ Boston_4$priceperunit + Boston_4$ï..weeknumber + factor(Boston_4$department) + Boston_4$shopperhealthiness + Boston_4$shopperhealthiness * Boston_4$weeklycategorylevelpricecutpromos.y + Boston_4$shopperhealthiness * Boston_4$weeklycategorylevelpricecutwithNuValpromos.y + Boston_4$shopperhealthiness * Boston_4$weeklycategorylevelfreeproductpromos.y + Boston_4$shopperhealthiness * Boston_4$weeklycategorylevelfreeproductwithNuValpromos.y +Boston_4$monthlyaverage. + Boston_4$monthlyaverage.* Boston_4$weeklycategorylevelpricecutpromos.y + Boston_4$monthlyaverage.* Boston_4$weeklycategorylevelpricecutwithNuValpromos.y+ Boston_4$monthlyaverage.* Boston_4$weeklycategorylevelfreeproductpromos.y+ Boston_4$monthlyaverage.* Boston_4$weeklycategorylevelfreeproductwithNuValpromos.y + Boston_4$weeklycategorylevelpricecutpromos.y+ Boston_4$weeklycategorylevelpricecutwithNuValpromos.y+ Boston_4$weeklycategorylevelfreeproductpromos.y+ Boston_4$weeklycategorylevelfreeproductwithNuValpromos.y, data = Boston_4)
summary(fit2d)
vif(fit2d)

fit2c_2<- lm(Boston_4$netSales ~ Boston_4$priceperunit + Boston_4$ï..weeknumber + factor(Boston_4$department) + Boston_4$shopperhealthiness + Boston_4$shopperhealthiness * Boston_4$proportionof_pricecutpromoswithNuVal + Boston_4$shopperhealthiness * Boston_4$proportionof_freeproductwithpromoswithNuVal +Boston_4$monthlyaverage. + Boston_4$monthlyaverage.* Boston_4$proportionof_pricecutpromoswithNuVal+ Boston_4$monthlyaverage.* Boston_4$proportionof_freeproductwithpromoswithNuVal + Boston_4$proportionof_pricecutpromoswithNuVal+ Boston_4$proportionof_freeproductwithpromoswithNuVal, data = Boston_4)
summary(fit2c_2)
vif(fit2c_2)

fit2d_2<- lm(Boston_4$netSales ~ Boston_4$priceperunit + Boston_4$ï..weeknumber + factor(Boston_4$department) + Boston_4$shopperhealthiness + Boston_4$shopperhealthiness * Boston_4$weeklycategorylevelpricecutwithNuValpromos.y + Boston_4$shopperhealthiness * Boston_4$weeklycategorylevelfreeproductwithNuValpromos.y +Boston_4$monthlyaverage.  + Boston_4$monthlyaverage.* Boston_4$weeklycategorylevelpricecutwithNuValpromos.y + Boston_4$monthlyaverage.* Boston_4$weeklycategorylevelfreeproductwithNuValpromos.y + Boston_4$weeklycategorylevelpricecutwithNuValpromos.y + Boston_4$weeklycategorylevelfreeproductwithNuValpromos.y, data = Boston_4)
summary(fit2d_2)
vif(fit2d_2)

fit2e<- lm(Boston_4$weeklysales ~ Boston_4$categorypromotion + Boston_4$categoryNuVal + Boston_4$ï..weeknumber + factor(Boston_4$department), data = Boston_4)
summary(fit2e)
library(car)
vif(fit2e)

## Impact of product level promotions on transaction level sales




fit3<- lm(Boston_4$netSales ~ Boston_4$priceperunit + Boston_4$Promotion + Boston_4$NuVal_1 + Boston_4$Promotion:Boston_4$Price.Cut + Boston_4$NuVal_1:Boston_4$Price.Cut + Boston_4$weekofyear + factor(Boston_4$department) + Boston_4$shopperhealthiness + Boston_4$shopperhealthinesXpromo:Boston_4$Price.Cut + Boston_4$shopperhealthinesXNuVal:Boston_4$Price.Cut + Boston_4$monthlyaverage. + Boston_4$monthlyaveragespendingXpromo:Boston_4$Price.Cut + Boston_4$monthlyaveragespendingXNuVal:Boston_4$Price.Cut, data = Boston_4)
summary(fit3)
vif(fit3)




### hlm on model 3

install.packages('lme4')
install.packages('language4')
library('lme4')
library('language4')

mymodel <- lmer(netSales~ weeklycategorylevelpricecutwithNuValpromos.y + weeklycategorylevelfreeproductwithNuValpromos.y +shopperhealthiness + shopperhealthiness * weeklycategorylevelpricecutwithNuValpromos.y + shopperhealthiness * weeklycategorylevelfreeproductwithNuValpromos.y + monthlyaverage. + monthlyaverage. * weeklycategorylevelpricecutwithNuValpromos.y + monthlyaverage. *  weeklycategorylevelfreeproductwithNuValpromos.y + (1 + shopperhealthiness +  monthlyaverage.|department), data = Boston_4)
summary(mymodel)

table(freq(Boston_2$Promotion))

## steps to get weekly A, B and C size ads per category

adsize<- read.csv("adsize.csv")
sizeC = aggregate(adsize$SizeCads ~ adsize$ï..Departmentyearweek, data = adsize, FUN = sum)
names(sizeC) <- c("Departmentyearweek", "sizeCads")
sizeB = aggregate(adsize$SizeBads ~ adsize$ï..Departmentyearweek, data = adsize, FUN = sum)
names(sizeB) <- c("Departmentyearweek", "sizeBads")
sizeA = aggregate(adsize$SizeAads ~ adsize$ï..Departmentyearweek, data = adsize, FUN = sum)
names(sizeA) <- c("Departmentyearweek", "sizeAads")
totalsize <- merge(sizeC, sizeB, by="Departmentyearweek")
totalsize2 <- merge(totalsize, sizeA, by="Departmentyearweek")
library("dplyr")
Boston_3 <- left_join(Boston_2, totalsize2, by ="Departmentyearweek")

## steps to get page 1/2/3 weekly ads per category
first3pageads = aggregate(adsize$Page1or2or3ads ~ adsize$Departmentyearweek, data = adsize, FUN = sum)
first1pageads = aggregate(adsize$Page1ads ~ adsize$Departmentyearweek, data = adsize, FUN = sum)
names(first3pageads) <- c("Departmentyearweek", "totaladsonfirst3pages")
names(first1pageads) <- c("Departmentyearweek", "totaladsonfirstpage")
Boston_4 <- left_join(Boston_3, first3pageads, by ="Departmentyearweek")
Boston_4 <- left_join(Boston_3, first1pageads, by ="Departmentyearweek")
Boston_4$totaladsonfirst3pages[is.na(Boston_4$totaladsonfirst3pages )] <- 0
write.csv(Boston_4, file = 'Boston_bigfile_new_April_new_5.csv')

## some descriptives
Boston_2$yearweek <- paste(Boston_2$year, Boston_2$weekofyear, collapse = NULL)

meat = subset(Boston_2, Boston_2$department == "Meat")
meat_weeks = length(unique(meat$yearweek))
meatl1 = length(unique(meat$yearweek[which(meat$sizeCads == 0)]))
meatl2 = length(unique(meat$yearweek[which(meat$sizeBads == 0)]))
meatl3 = length(unique(meat$yearweek[which(meat$sizeAads == 0)]))
meat_l4 = length(unique(meat$yearweek[which(meat$totaladsonfirst3pages == 0)]))
meat_avgCads = mean(meat$sizeCads)
meat_rangeCads = range(meat$sizeCads)
meat_avgBads = mean(meat$sizeBads)
meat_rangeBads = range(meat$sizeBads)
meat_avgAads = mean(meat$sizeAads)
meat_avgfirst3pageads = mean(meat$totaladsonfirst3pages)
mean(meat$)

produce = subset(Boston_2, Boston_2$department == "Produce")
produce_weeks = length(unique(produce$yearweek))
producel1 = length(unique(produce$yearweek[which(produce$sizeCads == 0)]))
producel2 = length(unique(produce$yearweek[which(produce$sizeBads == 0)]))
producel3 = length(unique(produce$yearweek[which(produce$sizeAads == 0)]))
produce_avgCads = mean(produce$sizeCads)
produce_avgBads = mean(produce$sizeBads)
produce_avgAads = mean(produce$sizeAads)
produce_avgfirst3pageads = mean(produce$totaladsonfirst3pages)

Seafood = subset(Boston_2, Boston_2$department == "Seafood")
Seafood_weeks = length(unique(Seafood$yearweek))
Seafoodl1 = length(unique(Seafood$yearweek[which(Seafood$sizeCads == 0)]))
Seafoodl2 = length(unique(Seafood$yearweek[which(Seafood$sizeBads == 0)]))
Seafoodl3 = length(unique(Seafood$yearweek[which(Seafood$sizeAads == 0)]))
Seafood_avgCads = mean(Seafood$sizeCads)
Seafood_avgBads = mean(Seafood$sizeBads)
Seafood_avgAads = mean(Seafood$sizeAads)
Seafood_avgfirst3pageads = mean(Seafood$totaladsonfirst3pages)

Bakery = subset(Boston_2, Boston_2$department == "Bakery")
Bakery_weeks = length(unique(Bakery$yearweek))
Bakeryl1 = length(unique(Bakery$yearweek[which(Bakery$sizeCads == 0)]))
Bakeryl2 = length(unique(Bakery$yearweek[which(Bakery$sizeBads == 0)]))
Bakeryl3 = length(unique(Bakery$yearweek[which(Bakery$sizeAads == 0)]))
Bakery_avgCads = mean(Bakery$sizeCads)
Bakery_avgBads = mean(Bakery$sizeBads)
Bakery_avgAads = mean(Bakery$sizeAads)
Bakery_avgfirst3pageads = mean(Bakery$totaladsonfirst3pages)

meat_noNa = subset (meat, meat$pagenum_1!= "NA")
range(meat_noNa$pagenum_1)

meatpromo = length(unique(meat$yearweek[which(meat$categoryNuVal != 0)]))