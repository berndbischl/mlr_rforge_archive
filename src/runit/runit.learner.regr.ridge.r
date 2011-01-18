
test.ridge <- function() {
  library(penalized)
	parset.list <- list(
			list(),
			list(lambda2 = 0.3),
			list(lambda2 = 1),
			list(lambda2 = 2)
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
	
	#extra cv test	
	folds=5
	cvl.res <- cvl(regr.formula, data=regr.df, lambda2=0.3, fold=folds)
  res = make.res.instance(make.res.desc("cv", iters=folds), task=regr.task)
  for (i in 1:folds) {
    res@train.inds[[i]] = setdiff(1:nrow(regr.df), which(cvl.res$fold == i))
    res@test.inds[[i]] = which(cvl.res$fold == i)
  }
  wl = make.learner("regr.ridge", lambda=0.3)
  r = resample(wl, regr.task, res)
  p = as.data.frame(r$pred)
	for (i in 1:folds) {
    test.i = res["test.inds"][[i]]
    rf.p = subset(p, subset=(iter==i), select="response", drop=TRUE)    
		checkEquals(rf.p, cvl.res$predictions[test.i])		
	}
}
