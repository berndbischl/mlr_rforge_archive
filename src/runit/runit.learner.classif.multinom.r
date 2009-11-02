

test.multinom <- function() {
	
	set.seed(debug.seed)
	capture.output(
			m <- multinom(formula = multiclass.formula, data = multiclass.train)
	)
			
	set.seed(debug.seed)
	p <- predict(m, newdata=multiclass.test)
	
	simple.test("nnet.multinom", multiclass.df, multiclass.formula, multiclass.train.inds, p)
	
	set.seed(debug.seed)
	p <- predict(m, newdata=multiclass.test, type="probs")
	prob.test  ("nnet.multinom", multiclass.df, multiclass.formula, multiclass.train.inds, p)
	
	tt <- "multinom"
	tp <- function(model, newdata) predict(model, newdata)
	
	cv.test("nnet.multinom", multiclass.df, multiclass.formula, tune.train=tt, tune.predict=tp )
	
}