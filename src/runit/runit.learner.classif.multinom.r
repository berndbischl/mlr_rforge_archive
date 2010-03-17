

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
	
	# test multinom for 2 classes
	m = train("nnet.multinom", binaryclass.task)
	p = predict(m, newdata=binaryclass.df, type=c("response", "prob"))
	rr = p["response"]
	pp = p["prob"]
	i = max.col(pp)
	labs = as.factor(colnames(pp)[i]) 
	checkEquals(rr, labs)	
}