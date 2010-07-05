test.benchmark <- function() {
	ct = make.task("iris", data=iris, target="Species")
	outer = make.res.desc("holdout") 
	inner = new("cv.desc", iters=3L)
	
	# check empty ranges 
	cbr <- benchmark("classif.rpart", task=ct, resampling=outer, conf.mat=T, models=T, paths=T)

	# normal benchmark - one par
	ranges <- list(minsplit=seq(3,10,2))
	wl = make.tune.wrapper("classif.rpart", resampling=inner, control=grid.control(ranges=ranges))
	bm = benchmark(wl, ct, outer, conf.mat=T, models=T, paths=T)
	checkTrue(is.list(bm$ors))
	checkEquals(length(bm$ors), 1)
	checkTrue(is.list(bm$ors[[1]]@path))
	checkTrue(length(bm$ors[[1]]@path) > 1)
	checkTrue(is.list(bm$models))
	checkEquals(length(bm$models), 1)
	checkTrue(is(bm$models[[1]], "wrapped.model"))
	checkTrue(is(bm$models[[1]]["learner.model"], "rpart"))
	checkTrue(is.matrix(bm$conf.mat))
	
	# dont save models and paths and conf.mats
	wl = make.tune.wrapper("classif.rpart", resampling=inner, control=grid.control(ranges=ranges))
	bm = benchmark(wl, ct, outer, conf.mat=F, models=F, paths=F)
	checkTrue(is.list(bm$ors))
	checkEquals(length(bm$ors), 1)
	checkTrue(is.list(bm$ors[[1]]@opt$par))
	checkEquals(names(bm$ors[[1]]@opt$par), "minsplit")
	checkEquals(names(bm$ors[[1]]@opt$perf), c("mean.mmce", "sd.mmce"))
	checkTrue(is.list(bm$ors[[1]]@path))
	checkTrue(length(bm$ors[[1]]@path) == 0)
	checkTrue(is.null(bm$models))
	checkTrue(is.null(bm$conf.mat))
	
	
	# normal benchmark - 2 par
	ranges <- list(minsplit=seq(3,10,2), cp=c(0.1, 0.11 , 0.09))
	wl = make.tune.wrapper("classif.rpart", resampling=inner, control=grid.control(ranges=ranges))
	cbr = benchmark(wl, ct, outer, conf.mat=F, models=F, paths=F)
	
}
