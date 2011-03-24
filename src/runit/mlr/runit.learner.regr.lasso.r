
test.lasso <- function() {
	library(penalized)
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
		pars <- list(regr.formula, data=regr.train)
		pars <- c(pars, parset)
		set.seed(debug.seed)
		capture.output(
			m <- do.call(penalized, pars)
		)
		p <- predict(m, data=regr.test)
		old.predicts.list[[i]] <- p[,"mu"]
	}
	simple.test.parsets("regr.lasso", regr.df, regr.target, regr.train.inds, old.predicts.list, parset.list)
	
	#extra cv test	
	folds=5
	cvl.res <- cvl(regr.formula, data=regr.df, lambda1=0.3, fold=folds)
  res = makeResampleInstance(makeResampleDesc("CV", iters=folds), task=regr.task)
	for (i in 1:folds) {
    res@train.inds[[i]] = setdiff(1:nrow(regr.df), which(cvl.res$fold == i))
    res@test.inds[[i]] = which(cvl.res$fold == i)
  }
	wl = makeLearner("regr.lasso", lambda1=0.3)
	r = resample(wl, regr.task, res)
  p = as.data.frame(r$pred)
#	print(rf@preds[[1]])
	for (i in 1:folds) {
    test.i = res["test.inds"][[i]]
    rf.p = subset(p, subset=(iter==i), select="response", drop=TRUE)    
		checkEquals(rf.p, cvl.res$predictions[test.i])		
	}
}
