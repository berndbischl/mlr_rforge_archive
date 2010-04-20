test.benchmark <- function() {
	

	ct <- make.task("iris", data=iris, target="Species")
	outer <- make.res.instance("cv", ct, iters=5) 
	inner <- new("cv.desc", iters=3)
	
	# check empty ranges 
	cbr <- benchmark("classif.rpart", task=ct, resampling=outer, models=T, opts=T, paths=T)

	# normal benchmark - one par
	ranges <- list(minsplit=seq(3,10,2))
	wl = make.tune.wrapper("classif.rpart", resampling=inner, control=grid.control(ranges=ranges))
	cbr = benchmark(wl, ct, outer, models=T, opts=T, paths=T)

	# normal benchmark - 2 par
	ranges <- list(minsplit=seq(3,10,2), cp=c(0.1, 0.11 , 0.09))
	wl = make.tune.wrapper("classif.rpart", resampling=inner, control=grid.control(ranges=ranges))
	cbr = benchmark(wl, ct, outer, models=F, opts=F, paths=F)
	
}
