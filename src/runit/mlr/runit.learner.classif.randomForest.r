
test.randomForest <- function() {
  library(randomForest)
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
		pars <- list(formula=multiclass.formula, data=multiclass.train)
		pars <- c(pars, parset)
		set.seed(debug.seed)
		m <- do.call(randomForest, pars)
		set.seed(debug.seed)
		p <- predict(m, newdata=multiclass.test, type="response")
		set.seed(debug.seed)
		p2 <- predict(m, newdata=multiclass.test, type="prob")
		old.predicts.list[[i]] <- p
		old.probs.list[[i]] <- p2
	}
	
	simple.test.parsets("classif.randomForest", multiclass.df, multiclass.target, multiclass.train.inds, old.predicts.list, parset.list)
	prob.test.parsets("classif.randomForest", multiclass.df, multiclass.target, multiclass.train.inds, old.probs.list, parset.list)
	
	tt <- randomForest
	
	cv.test.parsets("classif.randomForest", multiclass.df, multiclass.target, tune.train=tt, parset.list=parset.list)
}

