
test.ridge <- function() {
	
	parset.list <- list(
			list(),
			list(lambda2 = 0.3),
			list(lambda2 = 1),
			list(lambda2 = 2)
	)
	
	parset.list2 <- list(
			list(),
			list(lambda = 0.3),
			list(lambda = 1),
			list(lambda = 2)
	)
	
	old.predicts.list = list()
	old.probs.list = list()
	
	for (i in 1:length(parset.list)) {
		parset <- parset.list[[i]]
		pars <- list(regr.formula, data=regr.train)
		pars <- c(pars, parset)
		set.seed(debug.seed)
		m <- do.call(penalized, pars)
		p <- predict.penalized.ridge(m, newdata=regr.test)
		old.predicts.list[[i]] <- p
	}
	
	simple.test.parsets("penalized.ridge", regr.df, regr.formula, regr.train.inds, old.predicts.list, parset.list)
	simple.test.parsets("penalized.ridge", regr.df, regr.formula, regr.train.inds, old.predicts.list, parset.list2)
	
	
	#extra cv test	
	folds=5
	cvl.res <- cvl(regr.formula, data=regr.df, lambda2=0.3, fold=folds)
	cv.i <- make.cv.instance(size=nrow(regr.df), iters=folds)
	for (i in 1:folds)
		cv.i@inds[[i]] <- setdiff(1:nrow(regr.df), which(cvl.res$fold == i))
	ct <- make.regr.task("penalized.ridge", formula=regr.formula, data=regr.df)
	rf <- resample.fit(ct, cv.i, parset=list(lambda=0.3))
	for (i in 1:folds) {
		test.i <- cv.i["test.inds", i]
		rf.p <- rf@preds[[i]]
		names(rf.p) <- NULL
		checkEquals(rf.p, cvl.res$predictions[test.i])		
	}
}
