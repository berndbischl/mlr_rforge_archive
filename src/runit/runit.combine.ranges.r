test.combine.ranges <- function() {
	cv.i <- make.res.instance("cv", binaryclass.task, iters=2)
	r1 <- list(kernel="polydot", C=c(1,2), degree=c(2,3))
	r2 <- list(kernel="rbfdot", C=c(1,2), sigma=c(4,6))
	ranges=combine.ranges(r1,r2)
	tr<- tune("kernlab.svm.classif", binaryclass.task, cv.i, method="grid", control=list(ranges=ranges))
}	
	