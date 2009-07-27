
test.randomForest <- function() {
	
	parset.list <- list(
			list(),
			list(ntree=50,  mtry=2),
			list(ntree=50, mtry=4),
			list(ntree=200, mtry=2),
			list(ntree=2000, mtry=4)
	)
	
	
	old.predicts.list = list()
	old.probs.list = list()
	
	for (i in 1:length(parset.list)) {
		
		parset <- parset.list[[i]]
		pars <- list(formula=testsuite.formula, data=testsuite.train)
		pars <- c(pars, parset)
		set.seed(debug.seed)
		m <- do.call(randomForest, pars)
		p <- predict(m, newdata=testsuite.test, type="response")
		p2 <- predict(m, newdata=testsuite.test, type="prob")
		old.predicts.list[[i]] <- p
		old.probs.list[[i]] <- p2
	}
	
	simple.test.parsets("randomForest.classif", testsuite.df, testsuite.formula, testsuite.train.inds, old.predicts.list, parset.list)
	prob.test.parsets("randomForest.classif", testsuite.df, testsuite.formula, testsuite.train.inds, old.probs.list, parset.list)
	
	tt <- randomForest
	
	cv.test.parsets("randomForest.classif", testsuite.df, testsuite.formula, tune.train=tt, parset.list=parset.list)
}

