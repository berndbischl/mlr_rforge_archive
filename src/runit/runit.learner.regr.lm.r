
test.lm <- function() {
	pars <- list(regr.formula, data=regr.train)
	set.seed(debug.seed)
	m <- do.call(lm, pars)
	p <- predict(m, newdata=regr.test)

	simple.test("stats.lm", regr.df, regr.formula, regr.train.inds, p)
}
