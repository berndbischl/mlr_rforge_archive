
test.svm <- function() {
	
	set.seed(debug.seed)
	m <- ksvm(x=testsuite.formula, data=testsuite.train, kernel="rbfdot", kpar=list(sigma=20), prob.model = T)
	p <-  predict(m, newdata=testsuite.test)
	p2 <- predict(m, newdata=testsuite.test, type="prob")
	simple.test("kernlab.svm.classif", testsuite.df, testsuite.formula, testsuite.train.inds, p,  parset=list(sigma=20))
	prob.test  ("kernlab.svm.classif", testsuite.df, testsuite.formula, testsuite.train.inds, p2, parset=list(sigma=20))
	
	set.seed(debug.seed)
	m <- ksvm(x=testsuite.formula, data=testsuite.train, kernel="laplacedot", kpar=list(sigma=10), prob.model = T)
	p <- predict(m, newdata=testsuite.test)
	p2 <- predict(m, newdata=testsuite.test, type="prob")
	simple.test("kernlab.svm.classif",testsuite.df, testsuite.formula, testsuite.train.inds, p,  parset=list(kernel="laplacedot", sigma=10))
	prob.test  ("kernlab.svm.classif",testsuite.df, testsuite.formula, testsuite.train.inds, p2, parset=list(kernel="laplacedot", sigma=10))
	
	set.seed(debug.seed)
	m <- ksvm(x=testsuite.formula, data=testsuite.train, kernel="polydot", kpar=list(degree=3, offset=2, scale=1.5), prob.model = T)
	p <- predict(m, newdata=testsuite.test)
	p2 <- predict(m, newdata=testsuite.test, type="prob")
	simple.test("kernlab.svm.classif", testsuite.df, testsuite.formula, testsuite.train.inds, p,  parset=list(kernel="polydot", degree=3, offset=2, scale=1.5))
	prob.test  ("kernlab.svm.classif", testsuite.df, testsuite.formula, testsuite.train.inds, p2, parset=list(kernel="polydot", degree=3, offset=2, scale=1.5))
	
	tt <- function (formula, data, subset=1:150, ...) {
		ksvm(x=formula, data=data[subset,], kernel="polydot", kpar=list(degree=3, offset=2, scale=1.5))
	}
	
	cv.test("kernlab.svm.classif", testsuite.df, testsuite.formula, tune.train=tt, parset=list(kernel="polydot", degree=3, offset=2, scale=1.5))
}
