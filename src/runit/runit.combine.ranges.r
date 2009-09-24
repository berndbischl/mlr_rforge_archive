test.combine.ranges <- function() {
	cv.i <- make.cv.instance(size=nrow(binaryclass.df), iters=2)
	ct <- make.classif.task("kernlab.svm.classif", data=binaryclass.df, formula=binaryclass.formula)
	r1 <- list(kernel="polydot", C=c(1,2), degree=c(2,3))
	r2 <- list(kernel="rbfdot", C=c(1,2), sigma=c(4,6))
	tr<- tune(ct, cv.i, ranges=combine.ranges(r1,r2))
}	
	