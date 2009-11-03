test.benchmark <- function() {
	
	# check empty ranges 
	ct <- make.classif.task("rpart.classif", data=iris, formula=Species ~.)
	outer.instance <- make.cv.instance(size=nrow(iris), iters=2) 
	inner.instance <- new("cv.desc", iters=3)
	cbr <- benchmark(ct, outer.resampling=outer.instance, inner.resampling=inner.instance)
	
	
	# normal benchmark
	ct <- make.classif.task("rpart.classif", data=iris, formula=Species ~.)
	ranges <- list(minsplit=3:7, cp=seq(0.1, 0.5, by=0.1))
	outer.instance <- make.cv.instance(size=nrow(iris), iters=2) 
	inner.instance <- new("cv.desc", iters=3)
	cbr <- benchmark(ct, ranges=ranges, outer.resampling=outer.instance, inner.resampling=inner.instance)

	# complete results
	ct <- make.classif.task("kernlab.svm.classif", data=iris, formula=Species ~.)
	C.seq <- c(0.5, 1, 2)
	ranges <- list(C=C.seq, kernel=c("polydot"), degree=1)
	outer.instance <- make.cv.instance(size=nrow(iris), iters=2) 
	inner.instance <- new("cv.desc", iters=3)
	cbr <- benchmark(ct, ranges=ranges, outer.resampling=outer.instance, inner.resampling=inner.instance, all.tune.results=T)
}
