
test.nnet <- function() {
	
	set.seed(debug.seed)
	m = nnet(multiclass.formula, size=7, data=multiclass.train)
	set.seed(debug.seed)
	p = as.factor(predict(m, newdata=multiclass.test, type="class"))
	set.seed(debug.seed)
	p2 = predict(m, newdata=multiclass.test, type="raw")
	set.seed(debug.seed)
	m = nnet(binaryclass.formula, size=7, data=binaryclass.train)
	set.seed(debug.seed)
	p3 = predict(m, newdata=binaryclass.test, type="raw")[,1]
	simple.test("classif.nnet", multiclass.df, multiclass.formula, multiclass.train.inds, p, parset=list(size=7))
	prob.test("classif.nnet", multiclass.df, multiclass.formula, multiclass.train.inds, p2, parset=list(size=7))
	prob.test("classif.nnet", binaryclass.df, binaryclass.formula, binaryclass.train.inds, p3, parset=list(size=7))	
	
	tt <- function (formula, data, subset=1:150, ...) {
		nnet(formula, data=data[subset,], size=3, maxit=50)
	}
	tp <- function(model, newdata) as.factor(predict(model, newdata, type="class"))
	
	cv.test("classif.nnet", multiclass.df, multiclass.formula, tune.train=tt, tune.predict=tp, parset=list(size=3, maxit=50))
	
}
