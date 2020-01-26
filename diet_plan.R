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


