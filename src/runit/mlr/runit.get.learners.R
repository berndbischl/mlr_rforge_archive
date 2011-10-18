test.getLearners <- function() {
  if (!use.package) {
    # dont do this now, we might load the package....
    
    num1 <- getLearners(type = "classif", doubles = TRUE)
    for(i in seq(length=length(num1))){
      checkTrue(makeLearner(num1[i])@desc@feat["numerics"], checkNames=FALSE)
    }	
    num2 <- getLearners(type = "regr", doubles = TRUE)
    for(i in seq(length=length(num2))){
      checkTrue(makeLearner(num2[i])@desc@feat["numerics"], checkNames=FALSE)
    }
    
    fac1 <- getLearners(type = "classif", factors = TRUE)
    for(i in seq(length=length(fac1))){
      checkTrue(makeLearner(fac1[i])["factors"])
    }	
    fac2 <- getLearners(type = "regr", factors = TRUE)
    for(i in seq(length=length(fac2))){
      checkTrue(makeLearner(fac2[i])["factors"])
    }
    
    miss1 <- getLearners(type = "classif", missing = TRUE)
    for(i in seq(length=length(miss1))){
      checkTrue(makeLearner(miss1[i])["missings"])
    }	
    miss2 <- getLearners(type = "regr", missing = TRUE)
    for(i in seq(length=length(miss2))){
      checkTrue(makeLearner(miss2[i])["missings"])
    }
    
    multi1 <- getLearners(type = "classif", multiclass = TRUE)
    for(i in seq(length=length(multi1))){
      checkTrue(makeLearner(multi1[i])["multiclass"])
    }	
    multi2 <- getLearners(type = "regr", multiclass = TRUE)
    for(i in seq(length=length(multi2))){
      checkTrue(makeLearner(multi2[i])["multiclass"])
    }
    
    weights1 <- getLearners(type = "classif", weights = TRUE)
    for(i in seq(length=length(weights1))){
      checkTrue(makeLearner(weights1[i])["weights"])
    }	
    weights2 <- getLearners(type = "regr", weights = TRUE)
    for(i in seq(length=length(weights2))){
      checkTrue(makeLearner(weights2[i])["weights"])
    }
    
    probs1 <- getLearners(type = "classif", probs = TRUE)
    for(i in seq(length=length(probs1))){
      checkTrue(makeLearner(probs1[i])["probs"])
    }	
    probs2 <- getLearners(type = "regr", probs = TRUE)
    for(i in seq(length=length(probs2))){
      checkTrue(makeLearner(probs2[i])["probs"])
    }
  }
}