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

