
test.knn.regr <- function() {
	
	parset.list <- list(
			list(),
			list(k=1),
			list(k=4),
			list(k=10)
	)
	
	old.predicts.list = list()
	
	for (i in 1:length(parset.list)) {
		
		parset <- parset.list[[i]]
		pars <- list(formula=regr.formula, train=regr.train, test=regr.test)
		pars <- c(pars, parset)
		set.seed(debug.seed)
		m <- do.call(kknn, pars)
		p <- predict(m, newdata=regr.test)
		old.predicts.list[[i]] <- p
	}
	
	simple.test.parsets("kknn.regr", regr.df, regr.formula, regr.train.inds, old.predicts.list, parset.list)
	
	tt <- function (formula, data, k=7) {
		return(list(formula=formula, data=data, k=k))
	}
	tp <- function(model, newdata) {
		kknn(model$formula, train=model$data, test=newdata, k=model$k)$fitted
	}
	
	cv.test.parsets("kknn.regr", regr.df, regr.formula, tune.train=tt, tune.predict=tp, parset.list=parset.list)
}

