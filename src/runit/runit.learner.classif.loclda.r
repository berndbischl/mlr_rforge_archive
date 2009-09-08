


test.loclda <- function() {
	
	m <- loclda(formula=multiclass.formula, data=multiclass.train)
	p <- predict(m, newdata=multiclass.test)
	
	simple.test("loclda", multiclass.df, multiclass.formula, multiclass.train.inds, p$class)
	prob.test  ("loclda", multiclass.df, multiclass.formula, multiclass.train.inds, p$posterior)
	
	tt <- "loclda"
	tp <- function(model, newdata) predict(model, newdata)$class
	
	cv.test("loclda", multiclass.df, multiclass.formula, tune.train=tt, tune.predict=tp )
	
}
