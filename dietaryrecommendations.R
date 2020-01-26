getwd()
setwd("D:/R files")
mydata <- read.csv("dietary1.csv") # read your own data set here; please see the attached format of data you need to enter
attach(mydata)

## data cleaning for assigning NA to missing data
mydata$zeroct <- rowSums(mydata[,3:17]==0)
mydata[mydata$zeroct >= 3, 3:17] <- NA


library(foreign)
library(gplots)

## cleaned data with no missing rows
data_cleaned <- na.omit(mydata)
## subsetting the 1st id
id1<-subset(data_cleaned, USERID=1050)

## check heterogeneity of weights recorded in 3 years for a particular id
library(ggplot2)
plot_1050 <- plot(y = WEIGHT, x = TIMEDL, data = id1)
plot_1050

#Checking for heterogeneity across mean weight recorded over 3 years for 150 patients
plotmeans(WEIGHT ~ USERID, main="Heterogeineity across Fat", data= data_cleaned)

#Check number of counts for observations in cleaned data (data_cleaned)
summary.factor(data_cleaned$USERID)
class(summary.factor(mydata$TIMEDL))


##We now start building predictive statistical models on the data set to predict weight at the end of 3 years based on combination of nutrients absorbed over the time


#Regular OLS regression
Model1 <-lm(WEIGHT ~ PROT+FAT+SFAT+CARBS+FIBER+CHOL+SUGARS, data=mydata)
summary(Model1)
# we sww all components protein, fat, saturated fat, carbs, fiber, cholesterol and sugar are significant predictors of weight
fit1<-Model1$fitted
plot(fit1)

# checking regression plot of weight on protein
p1<-plot(mydata$PROT, mydata$WEIGHT, pch=19, xlab="PROTEIN", ylab="WEIGHT")
abline(lm(mydata$WEIGHT~mydata$PROT),lwd=3, col="red")

# checking regression plot of weight on fat
plot(mydata$FAT, mydata$WEIGHT, pch=19, xlab="FAT", ylab="WEIGHT")
abline(lm(mydata$WEIGHT~mydata$FAT),lwd=3, col="red")

# checking regression plot of weight on saturated fat
plot(mydata$SFAT, mydata$WEIGHT, pch=19, xlab="SATURATED FAT", ylab="WEIGHT")
abline(lm(mydata$WEIGHT~mydata$SFAT),lwd=3, col="red")

#Regular OLS regression does not consider heterogeneity across user ids or time
Model1<-lm(WEIGHT ~ PROT+FAT+SFAT+CARBS+FIBER+CHOL+SUGARS, data=mydata)
summary(Model1)

#Fixed effects model across individuals

Model2 <-lm(WEIGHT ~ PROT+FAT+SFAT+CARBS+FIBER+CHOL+SUGARS + factor(USERID) - 1, data=mydata)
summary(Model2)
#we see that protein, fat, carbs and cholesterol are significant predictors of weight
fit2<- Model2$fitted
install.packages("car")


#Comparing Model1 and Model2 to show significance of predictors protein, fat, saturated fat, carbs, fiber, cholesterol and sugar across both models in predicting weight
install.packages('apsrtable')
library(apsrtable)
apsrtable(Model1, Model2, model.names = c("Model1", "Model2"))
# We see all predictors are significant in Model1 whereas, only protein, fat, carbs and cholesterol are significant predictors in model 2

# time to build predictive model over panel data of 150 patients and dates spread over 3 years
install.packages("plm")
library(plm)

#fixed time effects: which dates' recorded values are significantly contributing to the predictive fixed effects model
fixed.time <- plm(WEIGHT ~ PROT+FAT+SFAT+CARBS+FIBER+CHOL+SUGARS+ factor(TIMEDL), data=mydata, index=c("USERID", "TIMEDL"), model="within")
summary(fixed.time)

#Fixed effects model accross individuals and time
mydata[-c(2, 4, 6), ]
Model3 <- plm(WEIGHT ~ PROT+FAT+SFAT+CARBS+FIBER+CHOL+SUGARS, data=mydata, index=c("USERID", "TIMEDL"), model="within")
summary(Model3)
fixef(Model3)

#fixed time effects: which dates' recorded values are significantly contributing to the predictive fixed effects model
fixed.time <- plm(WEIGHT ~ PROT+FAT+SFAT+CARBS+FIBER+CHOL+SUGARS+ factor(TIMEDL), data=mydata, index=c("USERID", "TIMEDL"), model="within")
summary(fixed.time)

# Comparing OLS and PLM Models
m <- pFtest(Model3,Model1)
m

# which is better? OLS or Fixed effects
if(m[6]=='significant effects') {
  fixedorols<-Model3
} else {
  fixedorols<-Model1
}


#Random effects model
Model4 <- plm(WEIGHT ~ PROT+FAT+SFAT+CARBS+FIBER+CHOL+SUGARS, data=mydata, index=c("USERID", "TIMEDL"), model="random")
summary(Model4)


# Which model works best? System outputs the best among Fixed, OLS and random effect
finaltest<-phtest(Model4,fixedorols)
if( finaltest$p.value<0.05) {
   bestmodel<-Model4
} else {
  bestmodel<-fixedorols
}
bestmodel

# outputting most efficient food nutrients
importantnutrients<-max(bestmodel$coefficients[-1])
importantnutrients
class(importantnutrients)

w <- as.list(bestmodel$coefficients)
w

j <- as.list(w=importantnutrients)
j
j$FAT
n <- names(j)



##Optimization function for amount of specific food items to be consumed

#We build a function that takes a matrix which has the calories for every food item and the beta
#coefficients that denote the weight sensitivities. The output is an interimdiatery plan that the
#patient should follow

#The function to be minimised

## enter the number of rows for food item e.g., whole wheat, fruit, flour, etc..
n=7

# 7 nutrients considered: Protein, Fat, Sat Fats, Carbs, Fiber, Cholesterol, Sugars

# Enter your own doctor's nutrition data here row wise
datahere<-c(1:49 ) #input your own numbers here

food_matrix <- matrix(datahere, nrow=n, ncol= 7) ## required as an input
## Enter names of your food items: an example is given here
row.names(food_matrix) <- c('Whole Grain', 'Rice', 'Flour', 'Fish', 'Mixed veg', 'Meat', 'Milk') # you can enter your count and names of food items
colnames(food_matrix) <- c('Protein', 'Fat','Sat Fats','Carbs','Fiber', 'Cholesterol', 'Sugars' )

food_matrix

weight_sens <- c(bestmodel$coefficients)    #these are the beta coefficients that come from the patient data
min_intake <- c(45, 50, 55, 60, 65, 70, 75)#doctor recommended daily intake of nutrients
names(min_intake)<-names(weight_sens)

cost_perunit<-c(5, 3, 5, 4, 8, 7, 9)  # cost per unit of each food item, whole grain,...., milk
names(cost_perunit)<-row.names(food_matrix)
maxcost<-500


#Adjustments for non-negativity constraints in the constraint matrix

k1 <- numeric(dim(food_matrix)[1]*dim(food_matrix)[2])
mat_app <- matrix(k1, nrow = dim(food_matrix)[1])
diag(mat_app) <- 1
mat_app

food_app_matrix <- matrix(append(food_matrix, mat_app), nrow = dim(food_matrix)[1])
food_app_matrix

#Adjustments for non-negativity constraints in vector of weight sensitivities (beta's)
weight_sens_app <- append(weight_sens, numeric(dim(food_matrix)[2]))
weight_sens_app

#Adjustments for non-negativity constraints in vector of cost_perunit
cost_perunit_app <- append(cost_perunit, numeric(dim(food_matrix)[2]))
cost_perunit_app


#Adjustments for non-negativity constraints in the constraint values vector
k2 <- numeric(dim(food_matrix)[2])
min_app_intake <- append(min_intake, k2)
min_app_intake
max_app_cost<-append(maxcost, k2)
max_app_cost
# Let us first just consider the nutritional value fulfilment and optimize by not considering costs
# If money is not a factor for the patient, use these optimized values
#Total weight function where total weight = weight sensitivity per nutrient (beta coefficients) *
#nutrient per food item * quantity of food items (decision variable)

total_weight1 <- function(x, m = food_app_matrix, n = weight_sens_app) {
  stopifnot(dim(m)[1]==length(x), dim(m)[2]==length(weight_sens_app))
  sum(x%*%m%*%n)
}

cons1 <- constrOptim(theta = c(100, 100, 100, 100, 100, 100, 100),grad = NULL, f = total_weight1, ui = t(food_app_matrix),
                     ci = min_app_intake)

## here are you final values of number of units you should consume for each food item
recommended1_withoutcostconcern<-cons1$par
names(recommended1_withoutcostconcern)<-row.names(food_matrix)
recommended1_withoutcostconcern


# Let us then optimize nutritional fulfilment by considering per unit cost of food items consumed
# If money is a factor for the patient, use these optimized values
#Total weight function where total weight = 0.6*weight sensitivity per nutrient (beta coefficients) *
#nutrient per food item * quantity of food items (decision variable) + 0.4* cost per unit of food item * quantity of food items (decision variable)

total_weight2 <- function(x, m = food_app_matrix, n = weight_sens_app, p= cost_perunit) {
  stopifnot(dim(m)[1]==length(x), dim(m)[2]==length(weight_sens_app))
  0.6*sum(x%*%m%*%n)+ 0.4*sum(x%*%p)
}


cons2 <- constrOptim(theta = c(1, 1, (-maxcost/cost_perunit[3])-100, 1, 1, 1, 1),grad = NULL, f = total_weight, ui = t(food_app_matrix),
                     ci = min_app_intake)

## here are you final values of number of units you should consume for each food item
recommended2_withcostconcern<-cons2$par
names(recommended2_withcostconcern)<-row.names(food_matrix)
recommended2_withcostconcern

## print recommended intake of food items without cost factor
recommended1_withoutcostconcern

## print recommended intake of food items with cost factor
recommended2_withcostconcern


## Difference in the food amounts with and without cost constraint
difference_in_optimized_values<-cons2$par-cons1$par
names(difference_in_optimized_values)<-row.names(food_matrix)
difference_in_optimized_values




