test.get.learners <- function() {
	
	num1 <- get.learners(type = "classif", supports.numerics = TRUE)
	for(i in seq(length=length(num1))){
		checkTrue(make.learner(num1[i])["supports.numerics"])
	}	
	num2 <- get.learners(type = "regr", supports.numerics = TRUE)
	for(i in seq(length=length(num2))){
		checkTrue(make.learner(num2[i])["supports.numerics"])
	}
	
	fac1 <- get.learners(type = "classif", supports.factors = TRUE)
	for(i in seq(length=length(fac1))){
		checkTrue(make.learner(fac1[i])["supports.factors"])
	}	
	fac2 <- get.learners(type = "regr", supports.factors = TRUE)
	for(i in seq(length=length(fac2))){
		checkTrue(make.learner(fac2[i])["supports.factors"])
	}
	
#	char1 <- get.learners(type = "classif", supports.characters = TRUE)
#	for(i in seq(length=length(char1))){
#		checkTrue(make.learner(char1[i])["supports.characters"])
#	}	
#	char2 <- get.learners(type = "regr", supports.characters = TRUE)
#	for(i in seq(length=length(char2))){
#		checkTrue(make.learner(char2[i])["supports.characters"])
#	}
		
	miss1 <- get.learners(type = "classif", supports.missing = TRUE)
	for(i in seq(length=length(miss1))){
		checkTrue(make.learner(miss1[i])["supports.missings"])
	}	
	miss2 <- get.learners(type = "regr", supports.missing = TRUE)
	for(i in seq(length=length(miss2))){
		checkTrue(make.learner(miss2[i])["supports.missings"])
	}
	
	multi1 <- get.learners(type = "classif", supports.multiclass = TRUE)
	for(i in seq(length=length(multi1))){
		checkTrue(make.learner(multi1[i])["supports.multiclass"])
	}	
	multi2 <- get.learners(type = "regr", supports.multiclass = TRUE)
	for(i in seq(length=length(multi2))){
		checkTrue(make.learner(multi2[i])["supports.multiclass"])
	}
	
	weights1 <- get.learners(type = "classif", supports.weights = TRUE)
	for(i in seq(length=length(weights1))){
		checkTrue(make.learner(weights1[i])["supports.weights"])
	}	
	weights2 <- get.learners(type = "regr", supports.weights = TRUE)
	for(i in seq(length=length(weights2))){
		checkTrue(make.learner(weights2[i])["supports.weights"])
	}
	
	probs1 <- get.learners(type = "classif", supports.probs = TRUE)
	for(i in seq(length=length(probs1))){
		checkTrue(make.learner(probs1[i])["supports.probs"])
	}	
	probs2 <- get.learners(type = "regr", supports.probs = TRUE)
	for(i in seq(length=length(probs2))){
		checkTrue(make.learner(probs2[i])["supports.probs"])
	}
	
	decis1 <- get.learners(type = "classif", supports.decision = TRUE)
	for(i in seq(length=length(decis1))){
		checkTrue(make.learner(decis1[i])["supports.decision"])
	}	
	decis2 <- get.learners(type = "regr", supports.decision = TRUE)
	for(i in seq(length=length(decis2))){
		checkTrue(make.learner(decis2[i])["supports.decision"])
	}
	
#	costs1 <- get.learners(type = "classif", supports.costs = TRUE)
#	for(i in seq(length=length(costs1))){
#		checkTrue(make.learner(costs1[i])["supports.costs"])
#	}	
#	costs2 <- get.learners(type = "regr", supports.costs = TRUE)
#	for(i in seq(length=length(costs2))){
#		checkTrue(make.learner(costs2[i])["supports.costs"])
#	}
}