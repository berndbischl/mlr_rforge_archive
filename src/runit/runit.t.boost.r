

test.boost <- function() {
	
	parset.list <- list(
			list(mfinal=3),
			list(mfinal=6, cp=0.2)
	)
	
	# does not support probs
	old.predicts.list = list()
	
	for (i in 1:length(parset.list)) {
		
		parset <- parset.list[[i]]
		pars <- list(formula=testsuite.formula, data=testsuite.train)
		pars <- c(pars, parset)
		set.seed(debug.seed)
		m <- do.call(adaboost.M1, pars)
		p <- predict(m, newdata=testsuite.test)
		old.predicts.list[[i]] <- as.factor(p$class)
	}
	
	
	simple.test.parsets("adaboost", testsuite.df, testsuite.formula, testsuite.train.inds, old.predicts.list, parset.list)
	
	
	tt <- function (formula, data, subset=1:nrow(data), ...) {
		adaboost.M1(formula, data[subset,], ...)
	}
	
	tp <- function(model, newdata) as.factor(predict(model, newdata)$class)
	
	cv.test.parsets("adaboost", testsuite.df, testsuite.formula, tune.train=tt, tune.predict=tp, parset.list=parset.list)
}

