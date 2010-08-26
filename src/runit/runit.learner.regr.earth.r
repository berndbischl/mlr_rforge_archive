
test.earth.regr <- function() {
  library(earth)
	parset.list <- list(
			list(),
			list(degree=2),
			list(penalty=4)
	)
	
	old.predicts.list = list()
	
	for (i in 1:length(parset.list)) {
		parset = parset.list[[i]]
		pars = list(regr.formula, data=regr.train)
		pars = c(pars, parset)
		set.seed(debug.seed)
		m = do.call(earth, pars)
		set.seed(debug.seed)
		old.predicts.list[[i]] = predict(m, newdata=regr.test)[,1]
	}
	
	simple.test.parsets("regr.earth", regr.df, regr.target, regr.train.inds, old.predicts.list, parset.list)
}
