
test.lm <- function() {
	pars <- list(regr.formula, data=regr.train)
	set.seed(debug.seed)
	m <- do.call(lm, pars)
	p <- predict(m, newdata=regr.test)

	simple.test("regr.lm", regr.df, regr.target, regr.train.inds, p)
}
