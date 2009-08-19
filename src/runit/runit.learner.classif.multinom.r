

test.multinom <- function() {
	
	m <- multinom(formula = multiclass.formula, data = multiclass.train)
	p <- predict(m, newdata=multiclass.test)
	
	simple.test("nnet.multinom", multiclass.df, multiclass.formula, multiclass.train.inds, p)
	
	p <- predict(m, newdata=multiclass.test, type="probs")
	prob.test  ("nnet.multinom", multiclass.df, multiclass.formula, multiclass.train.inds, p)
	
	tt <- "multinom"
	tp <- function(model, newdata) predict(model, newdata)
	
	cv.test("nnet.multinom", multiclass.df, multiclass.formula, tune.train=tt, tune.predict=tp )
	
}