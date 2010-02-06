

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
	p1 = predict(m, newdata=binaryclass.df, type="class")
	p2 = predict(m, newdata=binaryclass.df, type="prob")
	i = max.col(p2)
	labs = as.factor(colnames(p2)[i]) 
	checkEquals(p1, labs)	
}