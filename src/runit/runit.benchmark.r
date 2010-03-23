test.benchmark <- function() {
	

	ct <- make.classif.task("iris", data=iris, target="Species")
	outer <- make.res.instance("cv", ct, iters=5) 
	inner <- new("cv.desc", iters=3)
	
	# check empty ranges 
	cbr <- benchmark("rpart.classif", task=ct, resampling=outer)

	# normal benchmark - one par
	ranges <- list(minsplit=seq(3,10,2))
	wl = make.tune.wrapper("rpart.classif", resampling=inner, control=grid.control(ranges=ranges))
	cbr <- benchmark(wl, ct, outer)

	# normal benchmark - 2 par
	ranges <- list(minsplit=seq(3,10,2), cp=c(0.1, 0.11 , 0.09))
	wl = make.tune.wrapper("rpart.classif", resampling=inner, control=grid.control(ranges=ranges))
	cbr <- benchmark(wl, ct, outer)
	
}
