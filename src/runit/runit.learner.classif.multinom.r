

test.multinom <- function() {
  library(nnet)
	set.seed(debug.seed)
	capture.output(
			m <- multinom(formula = multiclass.formula, data = multiclass.train)
	)
			
	set.seed(debug.seed)
	p <- predict(m, newdata=multiclass.test)
	
	simple.test("classif.multinom", multiclass.df, multiclass.target, multiclass.train.inds, p)
	
	set.seed(debug.seed)
	p <- predict(m, newdata=multiclass.test, type="probs")
	prob.test  ("classif.multinom", multiclass.df, multiclass.target, multiclass.train.inds, p)

	
	tt <- "multinom"
	tp <- function(model, newdata) predict(model, newdata)
	
	cv.test("classif.multinom", multiclass.df, multiclass.target, tune.train=tt, tune.predict=tp )
	
	# test multinom for 2 classes
	m = train("classif.multinom", binaryclass.task)
	p = predict(m, newdata=binaryclass.df, type="prob")
	rr = p["response"]
	pp = p["prob"]
	i = as.integer(pp < 0.5) + 1
	labs = as.factor(binaryclass.task["class.levels"][i]) 
	checkEquals(rr, labs)	
}