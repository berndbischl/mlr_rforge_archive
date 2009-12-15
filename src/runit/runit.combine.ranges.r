test.combine.ranges <- function() {
	cv.i <- make.cv.instance(size=nrow(binaryclass.df), iters=2)
	ct <- make.classif.task(data=binaryclass.df, formula=binaryclass.formula)
	r1 <- list(kernel="polydot", C=c(1,2), degree=c(2,3))
	r2 <- list(kernel="rbfdot", C=c(1,2), sigma=c(4,6))
	ranges=combine.ranges(r1,r2)
	tr<- tune("kernlab.svm.classif", ct, cv.i, method="grid", control=list(ranges=ranges))
}	
	