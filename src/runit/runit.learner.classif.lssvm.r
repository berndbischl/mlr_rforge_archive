
test.lssvm <- function() {
	
	set.seed(debug.seed)
	m <- lssvm(x=multiclass.formula, data=multiclass.train, kernel="rbfdot", kpar=list(sigma=20))
	p <-  predict(m, newdata=multiclass.test)
	simple.test("classif.lssvm", multiclass.df, multiclass.target, multiclass.train.inds, p,  parset=list(sigma=20))
	
	set.seed(debug.seed)
	m <- lssvm(x=multiclass.formula, data=multiclass.train, kernel="laplacedot", kpar=list(sigma=10))
	p <- predict(m, newdata=multiclass.test)
	simple.test("classif.lssvm",multiclass.df, multiclass.target, multiclass.train.inds, p,  parset=list(kernel="laplacedot", sigma=10))
	
# Bug in kernel = "polydot"	
#	set.seed(debug.seed)
#	m <- lssvm(x=multiclass.formula, data=multiclass.train, kernel="polydot", kpar=list(degree=3, offset=2, scale=1.5))
#	p <- predict(m, newdata=multiclass.test)
#	simple.test("classif.lssvm", multiclass.df, multiclass.target, multiclass.train.inds, p,  parset=list(kernel="polydot", degree=3, offset=2, scale=1.5))
	
	tt <- function (formula, data, subset=1:150, ...) {
		lssvm(x=formula, data=data[subset,], kernel="rbfdot", kpar=list(sigma=20))
	}
	
	cv.test("classif.lssvm", multiclass.df, multiclass.target, tune.train=tt, parset=list(kernel="rbfdot", sigma=20))
	
}