test.benchmark <- function() {
	
	# check empty ranges 
#	ct <- new("classif.task", new("rpart.classif"), data=iris, formula=Species ~.)
#	outer.instance <- make.cv.instance(size=nrow(iris), iters=2) 
#	inner.instance <- new("cv.desc", iters=3)
#	cbr <- benchmark(ct, outer.resampling=outer.instance, inner.resampling=inner.instance)
	
	
	# normal benchmark
	ct <- new("classif.task", new("rpart.classif"), data=iris, formula=Species ~.)
	ranges <- list(minsplit=3:7, cp=seq(0.1, 0.5, by=0.1))
	outer.instance <- make.cv.instance(size=nrow(iris), iters=2) 
	inner.instance <- new("cv.desc", iters=3)
	cbr <- benchmark(ct, ranges=ranges, outer.resampling=outer.instance, inner.resampling=inner.instance)

#	# list of list of ranges
#	ct <- new("classif.svm", data=iris, formula=Species ~.)
#	C.seq <- c(0.5, 1, 2)
#	r1 <- list(C=C.seq, kernel=c("polydot"), degree=1)
#	r2 <- list(C=C.seq, kernel="vanilladot")
#	ranges = list(r1, r2)	
#	outer.instance <- make.cv.instance(size=nrow(iris), iters=2) 
#	inner.instance <- new("cv.desc", iters=3)
#	cbr <- benchmark(ct, ranges=ranges, outer.instance=outer.instance, inner.instance=inner.instance)
#    print(cbr)		
#
#	# list of list of ranges with complete results
#	ct <- new("classif.svm", data=iris, formula=Species ~.)
#	C.seq <- c(0.5, 1, 2)
#	r1 <- list(C=C.seq, kernel=c("polydot"), degree=1)
#	r2 <- list(C=C.seq, kernel="vanilladot")
#	ranges = list(r1, r2)	
#	outer.instance <- make.cv.instance(size=nrow(iris), iters=2) 
#	inner.instance <- new("cv.desc", iters=3)
#	cbr <- benchmark(ct, ranges=ranges, outer.instance=outer.instance, inner.instance=inner.instance, all.tune.results=T)
#	print(cbr)		
#	
}
