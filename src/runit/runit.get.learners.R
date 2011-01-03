test.get.learners <- function() {
  if (!use.package) {
    # dont do this now, we might load the package....
    
    num1 <- get.learners(type = "classif", doubles = TRUE)
    for(i in seq(length=length(num1))){
      checkTrue(make.learner(num1[i])["doubles"])
    }	
    num2 <- get.learners(type = "regr", doubles = TRUE)
    for(i in seq(length=length(num2))){
      checkTrue(make.learner(num2[i])["doubles"])
    }
    
    fac1 <- get.learners(type = "classif", factors = TRUE)
    for(i in seq(length=length(fac1))){
      checkTrue(make.learner(fac1[i])["factors"])
    }	
    fac2 <- get.learners(type = "regr", factors = TRUE)
    for(i in seq(length=length(fac2))){
      checkTrue(make.learner(fac2[i])["factors"])
    }
    
    miss1 <- get.learners(type = "classif", missing = TRUE)
    for(i in seq(length=length(miss1))){
      checkTrue(make.learner(miss1[i])["missings"])
    }	
    miss2 <- get.learners(type = "regr", missing = TRUE)
    for(i in seq(length=length(miss2))){
      checkTrue(make.learner(miss2[i])["missings"])
    }
    
    multi1 <- get.learners(type = "classif", multiclass = TRUE)
    for(i in seq(length=length(multi1))){
      checkTrue(make.learner(multi1[i])["multiclass"])
    }	
    multi2 <- get.learners(type = "regr", multiclass = TRUE)
    for(i in seq(length=length(multi2))){
      checkTrue(make.learner(multi2[i])["multiclass"])
    }
    
    weights1 <- get.learners(type = "classif", weights = TRUE)
    for(i in seq(length=length(weights1))){
      checkTrue(make.learner(weights1[i])["weights"])
    }	
    weights2 <- get.learners(type = "regr", weights = TRUE)
    for(i in seq(length=length(weights2))){
      checkTrue(make.learner(weights2[i])["weights"])
    }
    
    probs1 <- get.learners(type = "classif", probs = TRUE)
    for(i in seq(length=length(probs1))){
      checkTrue(make.learner(probs1[i])["probs"])
    }	
    probs2 <- get.learners(type = "regr", probs = TRUE)
    for(i in seq(length=length(probs2))){
      checkTrue(make.learner(probs2[i])["probs"])
    }
    
    decis1 <- get.learners(type = "classif", decision = TRUE)
    for(i in seq(length=length(decis1))){
      checkTrue(make.learner(decis1[i])["decision"])
    }	
    decis2 <- get.learners(type = "regr", decision = TRUE)
    for(i in seq(length=length(decis2))){
      checkTrue(make.learner(decis2[i])["decision"])
    }
    
#	costs1 <- get.learners(type = "classif", costs = TRUE)
#	for(i in seq(length=length(costs1))){
#		checkTrue(make.learner(costs1[i])["costs"])
#	}	
#	costs2 <- get.learners(type = "regr", costs = TRUE)
#	for(i in seq(length=length(costs2))){
#		checkTrue(make.learner(costs2[i])["costs"])
#	}
  }
}