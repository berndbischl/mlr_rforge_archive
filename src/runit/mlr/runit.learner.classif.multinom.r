

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
  wl = makeLearner("classif.multinom", predict.type="prob")
	m = train(wl, binaryclass.task)
	p = predict(m, newdata=binaryclass.df)
	rr = p@df$response
	pp = getScore(p)
	i = as.integer(pp < 0.5) + 1
	labs = as.factor(getClassLevels(binaryclass.task)[i]) 
	checkEquals(rr, labs)	
}