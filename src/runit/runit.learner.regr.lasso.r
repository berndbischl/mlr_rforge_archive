
test.lasso <- function() {
	
	parset.list <- list(
			list(),
			list(lambda1 = 0.3),
			list(lambda1 = 1),
			list(lambda1 = 2)
	)
	
	old.predicts.list = list()
	old.probs.list = list()
	
	for (i in 1:length(parset.list)) {
		parset <- parset.list[[i]]
		pars <- list(regr.formula, data=regr.data.train)
		pars <- c(pars, parset)
		set.seed(debug.seed)
		m <- do.call(penalized, pars)
		p <- predict.penalized(m, newdata=regr.data.test)
		old.predicts.list[[i]] <- p
	}
	
	simple.test.parsets("penalized.lasso", regr.data, regr.formula, testsuite.train.inds, old.predicts.list, parset.list)
	
	
	
	#extra cv test	
	folds=5
	cvl.res <- cvl(regr.formula, data=regr.data, lambda1=0.3, fold=folds)
	cv.i <- make.cv.instance(size=nrow(regr.data), iters=folds)
	for (i in 1:folds)
		cv.i@inds[[i]] <- setdiff(1:nrow(regr.data), which(cvl.res$fold == i))
	ct <- make.regr.task("penalized.lasso", formula=regr.formula, data=regr.data)
	rf <- resample.fit(ct, cv.i, parset=list(lambda2=0.3))
	for (i in 1:folds) {
		test.i <- cv.i["test.inds", i]
		rf.p <- rf@preds[[i]]
		names(rf.p) <- NULL
		checkEquals(rf.p, cvl.res$predictions[test.i])		
	}
}
