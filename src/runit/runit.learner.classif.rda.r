


test.rda <- function() {

	set.seed(debug.seed)
	m <- rda(formula=testsuite.formula, data=testsuite.train)
	p <- predict(m, newdata=testsuite.test)$class
	
	
	simple.test("rda", testsuite.df, testsuite.formula, 
			testsuite.train.inds, p)
	
	
	parset.list <- list(
			list(gamma=0.1, lambda=0.1),
			list(gamma=0.5, lambda=1),
			list(gamma=1, lambda=0)
	)
	
	old.predicts.list = list()
	old.probs.list = list()
	
	for (i in 1:length(parset.list)) {
		parset <- parset.list[[i]]
		pars <- list(formula=testsuite.formula, data=testsuite.train)
		pars <- c(pars, parset)
		set.seed(debug.seed)
		m <- do.call(rda, pars)
		p <- predict(m, newdata=testsuite.test)
		old.predicts.list[[i]] <- p$class
		old.probs.list[[i]] <- p$posterior
	}
	
	simple.test.parsets("rda", testsuite.df, testsuite.formula, testsuite.train.inds, old.predicts.list, parset.list)
	prob.test.parsets  ("rda", testsuite.df, testsuite.formula, testsuite.train.inds, old.probs.list, parset.list)
	
	tt <- "rda"
	tp <- function(model, newdata) predict(model, newdata)$class
	
	cv.test.parsets("rda", testsuite.df, testsuite.formula, tune.train=tt, tune.predict=tp, parset.list=parset.list)
}
