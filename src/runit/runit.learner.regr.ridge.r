
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
		capture.output(
			m <- do.call(penalized, pars)
		)	
		set.seed(debug.seed)
		p = predict(m, data=regr.test)
		old.predicts.list[[i]] <- p[,"mu"]
	}
	
	simple.test.parsets("regr.ridge", regr.df, regr.target, regr.train.inds, old.predicts.list, parset.list)
	simple.test.parsets("regr.ridge", regr.df, regr.target, regr.train.inds, old.predicts.list, parset.list2)
	
	
	#extra cv test	
	folds=5
	cvl.res <- cvl(regr.formula, data=regr.df, lambda2=0.3, fold=folds)
	cv.i <- make.res.instance("cv", regr.task, iters=folds)
	for (i in 1:folds)
		cv.i@inds[[i]] <- setdiff(1:nrow(regr.df), which(cvl.res$fold == i))
	wl = make.learner("regr.ridge", lambda=0.3)
	rf = resample.fit(wl, regr.task, cv.i)
	for (i in 1:folds) {
		test.i = get.test.set(cv.i, i)
		xs = as.list(rf)
		rf.p = xs[[i]]["response"]
		names(rf.p) <- NULL
		checkEquals(rf.p, cvl.res$predictions[test.i])		
	}
}
