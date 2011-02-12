#test.combine.ranges <- function() {
#	cv.i <- make.res.desc("cv", iters=2)
#	r1 <- list(kernel="polydot", C=c(1,2), degree=c(2,3))
#	r2 <- list(kernel="rbfdot", C=c(1,2), sigma=c(4,6))
#	ranges=combine.ranges(r1,r2)
#	tr<- tune("classif.ksvm", binaryclass.task, cv.i, control=grid.control(ranges=ranges))
#	
#	svm.tuner <- makeTuneWrapper("classif.ksvm", resampling=cv.i, control=grid.control(ranges=ranges))
#	be = bench.exp(svm.tuner, binaryclass.task, resampling=cv.i)
#}	
#	