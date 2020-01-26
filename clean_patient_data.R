## Package Diet Plan
## Our two-fold aim is as follows:
## to build a predictive time-series model to measure weight sensitivity towards nutrients;
## to optimize daily food intake.


## Clean_patient_data:

## assign your data set to x
## k is the number of allowable unreported nutrients
data_clean<-function(x,k) {
  ## data cleaning for assigning NA to missing data
  x$zeroct <- rowSums(x[,3:17]==0)
  x[x$zeroct >= k, 3:17] <- NA
  return(x)
}

## plot_patient_data:
## Check heteroscedasticity
##Protein Vs Weight
##Fat Vs Weight
##Saturated Fat Vs Weight

plot_patient <- function(x) {
  library(foreign)
  library(gplots)
  library(ggplot2)
  #xhetero <- plot(x$WEIGHT, x$TIMEDL,xlab = 'Dates', ylab = 'Weight', main = 'Weight variation per day')
  protein_weight <- plot(x$PROT, x$WEIGHT, pch=19, xlab="PROTEIN", ylab="WEIGHT")+
    abline(lm(x$WEIGHT~x$PROT),lwd=3, col="red")
  fat_weight <- plot(x$FAT, x$WEIGHT, pch=19, xlab="FAT", ylab="WEIGHT")+
    abline(lm(x$WEIGHT~x$FAT),lwd=3, col="red")
  sfat_weight <- plot(mydata$SFAT, mydata$WEIGHT, pch=19, xlab="SATURATED FAT", ylab="WEIGHT")+
    abline(lm(mydata$WEIGHT~mydata$SFAT),lwd=3, col="red")
  #windows()
  par(mfrow = c(1,3))
  #xhetero
  #return(xhetero)
  return(protein_weight)
  return(fat_weight)
  return(sfat_weight)
}

## weight_sensitivity
## Finding the sensitivity to weight for each nutrient given past history of patients consumption
## Run 3 models- OLS, Fixed effects and Random effects model and choose best model to build predictive model for measuring weight sensitivity towards nutrients

weight_sensitivity <- function(x) {
  Model1 <-lm(x$WEIGHT ~ x$PROT+x$FAT+x$SFAT+x$CARBS+x$FIBER+x$CHOL+x$SUGARS, data=x)
  Model2 <-lm(x$WEIGHT ~ x$PROT+x$FAT+x$SFAT+x$CARBS+x$FIBER+x$CHOL+x$SUGARS + factor(x$USERID) - 1, data=x)
  Model3 <-plm(x$WEIGHT ~ x$PROT+x$FAT+x$SFAT+x$CARBS+x$FIBER+x$CHOL+x$SUGARS, data=x, index=c("USERID", "TIMEDL"), model="within")

  m <- pFtest(Model3,Model1)

  #which model to use between OLS an Fixed effect
  if(m[6]=='significant effects') {
    fixedorols<-Model3
  } else {
    fixedorols<-Model1
  }

  #Random effects model
  Model4 <- plm(x$WEIGHT ~ x$PROT+x$FAT+x$SFAT+x$CARBS+x$FIBER+x$CHOL+x$SUGARS, data=x, index=c("USERID", "TIMEDL"), model="random")

  # Which model works best? System outputs the best among Fixed, OLS and random effect
  finaltest<-phtest(Model4,fixedorols)
  if( finaltest$p.value<0.05) {
    bestmodel<-Model4
  } else {
    bestmodel<-fixedorols
  }

  return(bestmodel$coefficients)

}


## diet_plan
## to optimize daily intake of food items based on weight sensitivity towards nutrients

## x is the food_matrix
##food_matrix <- matrix(datahere, nrow=n, ncol= 7)
##row.names(food_matrix) <- c('Whole Grain', 'Rice', 'Flour', 'Fish', 'Mixed veg', 'Meat', 'Milk') # you can enter your count and names of food items
##colnames(food_matrix) <- c('Protein', 'Fat','Sat Fats','Carbs','Fiber', 'Cholesterol', 'Sugars' )

## y is the patients reported data to calculate weight sensitivity
## min_intake is the doctor recommended daily intake of nutrients
## cost_per_unit is the cost per unit of each food item, whole grain,...., milk
## maxcost is the maximum cost willing to be paid for food

## n  number of rows for food item e.g., whole wheat, fruit, flour, etc..
## k number of nutrients considered: Protein, Fat, Sat Fats, Carbs, Fiber, Cholesterol, Sugars

diet_without_cost <- function(n ,k, x, y, min_intake) {
  k1 <- numeric(dim(x)[1]*dim(x)[2])
  mat_app <- matrix(k1, nrow = dim(x)[1])
  diag(mat_app) <- 1
  mat_app

  food_app_matrix <- matrix(append(x, mat_app), nrow = dim(x)[1])
  food_app_matrix


  #Adjustments for non-negativity constraints in vector of weight sensitivities (beta's)

  weight_sens <- weight_sensitivity(y)
  weight_sens_app <- append(weight_sens, numeric(dim(x)[2]))
  weight_sens_app

  #Adjustments for non-negativity constraints in the constraint values vector
  k2 <- numeric(dim(x)[2])
  min_app_intake <- append(min_intake, k2)
  min_app_intake



  # Let us optimize nutritional fulfilment by considering per unit cost of food items consumed
  # If money is a factor for the patient, use these optimized values
  #Total weight function where total weight = 0.6*weight sensitivity per nutrient (beta coefficients) *
  #nutrient per food item * quantity of food items (decision variable) + 0.4* cost per unit of food item * quantity of food items (decision variable)

  total_weight1 <- function(x1, m = food_app_matrix, n = weight_sens_app) {
    stopifnot(dim(m)[1]==length(x1), dim(m)[2]==length(weight_sens_app))
    sum(x1%*%m%*%n)
  }


  cons1 <- constrOptim(theta = c(100,100,100,100, 100, 100, 100),grad = NULL, f = total_weight1, ui = t(food_app_matrix),
                       ci = min_app_intake)

  ## here are you final values of number of units you should consume for each food item
  recommended1_withcostconcern<-cons1$par
  names(recommended1_withcostconcern)<-row.names(x)
  return(recommended1_withcostconcern)

}

## An example of defining required arguments and running the function is as follows

#x1<- matrix(c(1:49 ), nrow=7, ncol= 7)
#row.names(x1) <- c('Whole Grain', 'Rice', 'Flour', 'Fish', 'Mixed veg', 'Meat', 'Milk')
#colnames(x1) <- c('Protein', 'Fat','Sat Fats','Carbs','Fiber', 'Cholesterol', 'Sugars' )
#y1<-mydata
#diet_without_cost (7,7, x1, y1, c(45, 50, 55, 60, 65, 70, 75))
# gives appropriate result


##cost_optimization
## to optimize daily intake of food items based on weight sensitivity, taking into account patient's food budget
## x is the food_matrix
##food_matrix <- matrix(datahere, nrow=n, ncol= 7)
##row.names(food_matrix) <- c('Whole Grain', 'Rice', 'Flour', 'Fish', 'Mixed veg', 'Meat', 'Milk') # you can enter your count and names of food items
##colnames(food_matrix) <- c('Protein', 'Fat','Sat Fats','Carbs','Fiber', 'Cholesterol', 'Sugars' )

## y is the patients reported data to calculate weight sensitivity
## min_intake is the doctor recommended daily intake of nutrients
## cost_per_unit is the cost per unit of each food item, whole grain,...., milk
## maxcost is the maximum cost willing to be paid for food

## n  number of rows for food item e.g., whole wheat, fruit, flour, etc..
## k number of nutrients considered: Protein, Fat, Sat Fats, Carbs, Fiber, Cholesterol, Sugars

diet_with_cost <- function(n ,k, x, y, min_intake, cost_per_unit, maxcost) {
  k1 <- numeric(dim(x)[1]*dim(x)[2])
  mat_app <- matrix(k1, nrow = dim(x)[1])
  diag(mat_app) <- 1
  mat_app

  food_app_matrix <- matrix(append(x, mat_app), nrow = dim(x)[1])
  food_app_matrix

  #Define the matrix of constraints
  constr_matrix <- matrix(append(food_app_matrix, cost_perunit), nrow = dim(x)[1])
  constr_matrix

  #Adjustments for non-negativity constraints in vector of weight sensitivities (beta's)

  weight_sens <- weight_sensitivity(y)
  weight_sens_app <- append(weight_sens, numeric(dim(x)[2]))
  weight_sens_app

  #Adjustments for non-negativity constraints in the constraint values vector
  k2 <- numeric(dim(x)[2])
  min_app_intake <- append(min_intake, k2)
  min_app_intake

  #Adjustment to accomodate the budget constraint

  constr_bounds <- append(min_app_intake, -maxcost)
  constr_bounds


  # Let us optimize nutritional fulfilment by considering per unit cost of food items consumed
  # If money is a factor for the patient, use these optimized values
  #Total weight function where total weight = 0.6*weight sensitivity per nutrient (beta coefficients) *
  #nutrient per food item * quantity of food items (decision variable) + 0.4* cost per unit of food item * quantity of food items (decision variable)

  total_weight2 <- function(x1, m = food_app_matrix, n = weight_sens_app, p= cost_perunit) {
    stopifnot(dim(m)[1]==length(x1), dim(m)[2]==length(weight_sens_app))
    0.6*sum(x1%*%m%*%n)+ 0.4*sum(x1%*%p)
  }


  cons2 <- constrOptim(theta = c(1, 1, (-maxcost/cost_perunit[3])-100, 1, 1, 1, 1) ,grad = NULL, f = total_weight2, ui = t(constr_matrix),
                       ci = constr_bounds)

  ## here are you final values of number of units you should consume for each food item
  recommended2_withcostconcern<-cons2$par
  names(recommended2_withcostconcern)<-row.names(x)
  return(recommended2_withcostconcern)

}

## An example of defining required arguments and running the function is as follows

#x1<- matrix(c(1:49 ), nrow=7, ncol= 7)
#row.names(x1) <- c('Whole Grain', 'Rice', 'Flour', 'Fish', 'Mixed veg', 'Meat', 'Milk')
#colnames(x1) <- c('Protein', 'Fat','Sat Fats','Carbs','Fiber', 'Cholesterol', 'Sugars' )
#y1<-mydata
#diet_with_cost (7,7, x1, y1, c(45, 50, 55, 60, 65, 70, 75),-c(5, 3, 2, 10, 2, 5, 2), 500)
# gives appropriate result












