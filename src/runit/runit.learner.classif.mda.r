
test.mda <- function() {
	
	parset.list <- list(
			list(),
			list(subclasses=2),
			list(subclasses=3)
	)
	
	old.predicts.list = list()
	old.probs.list = list()
	
	for (i in 1:length(parset.list)) {
		parset <- parset.list[[i]]
		pars <- list(formula=multiclass.formula, data=multiclass.train)
		pars <- c(pars, parset)
		set.seed(debug.seed)
		m <- do.call(mda, pars)
		set.seed(debug.seed)
		p <-  predict(m, newdata=multiclass.test)
		set.seed(debug.seed)
		p2 <- predict(m, newdata=multiclass.test, type="posterior")
		old.predicts.list[[i]] <- p
		old.probs.list[[i]] <- p2
	}
	
	simple.test.parsets("classif.mda", multiclass.df, multiclass.formula, multiclass.train.inds, old.predicts.list, parset.list)
	prob.test.parsets  ("classif.mda", multiclass.df, multiclass.formula, multiclass.train.inds, old.probs.list, parset.list)
	
	tt <- "mda"
	tp <- function(model, newdata) predict(model, newdata)
	
	cv.test.parsets("classif.mda", multiclass.df, multiclass.formula, tune.train=tt, tune.predict=tp, parset.list=parset.list)
	cv.test("classif.mda", multiclass.df, multiclass.formula, tune.train=tt, tune.predict=tp, parset=list(subclasses=17))
	
}

