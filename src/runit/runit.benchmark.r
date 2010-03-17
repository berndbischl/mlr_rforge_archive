test.benchmark <- function() {
	

	ct <- make.classif.task(data=iris, target="Species")
	outer <- make.res.instance("cv", ct, iters=5) 
	inner <- new("cv.desc", iters=3)
	
	# check empty ranges 
	cbr <- benchmark("rpart.classif", task=ct, resampling=outer)

	# normal benchmark - one par
	ranges <- list(minsplit=seq(3,10,2))
	wl = make.tune.wrapper("rpart.classif", resampling=inner, control=grid.control(ranges=ranges))
	cbr <- benchmark(wl, ct, outer)
	cat("\n")
	print(cbr)

	# normal benchmark - 2 par
	ranges <- list(minsplit=seq(3,10,2), cp=c(0.1, 0.11 , 0.09))
	wl = make.tune.wrapper("rpart.classif", resampling=inner, control=grid.control(ranges=ranges))
	cbr <- benchmark(wl, ct, outer)
	cat("\n")
	print(cbr)

	
#	# complete results
#	C.seq <- c(0.5, 1, 2)
#	ranges <- list(C=C.seq, kernel=c("polydot"), degree=1)
#	cbr <- benchmark("kernlab.svm.classif", ct, outer, inner=inner, all.tune.results=T)
}
