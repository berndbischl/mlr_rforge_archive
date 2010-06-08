


test.loclda <- function() {
	
	m <- loclda(formula=multiclass.formula, data=multiclass.train)
	p <- predict(m, newdata=multiclass.test)
	
	simple.test("classif.loclda", multiclass.df, multiclass.target, multiclass.train.inds, p$class)
	prob.test  ("classif.loclda", multiclass.df, multiclass.target, multiclass.train.inds, p$posterior)
	
	tt <- "loclda"
	tp <- function(model, newdata) predict(model, newdata)$class
	
	cv.test("classif.loclda", multiclass.df, multiclass.target, tune.train=tt, tune.predict=tp )
	
}
