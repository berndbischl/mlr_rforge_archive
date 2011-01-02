test.benchmark <- function() {
	if (!use.package) {
		ct = make.task("iris", data=iris, target="Species")
		outer = make.res.desc("holdout") 
		inner = new("cv.desc", iters=3L)
		
		# check empty ranges 
		cbr <- .mlr.benchmark("classif.rpart", task=ct, resampling=outer, models=TRUE)
	
		# normal benchmark - tune wrapper with one par
		ranges <- list(minsplit=seq(3,10,2))
		wl = make.tune.wrapper("classif.rpart", resampling=inner, control=grid.control(ranges=ranges))
		bm = .mlr.benchmark(wl, ct, outer, models=TRUE)
		checkTrue(is.list(bm$ors))
		checkEquals(length(bm$ors), 1)
    checkTrue(is.list(bm$ors[[1]]@opt$par))
    checkEquals(names(bm$ors[[1]]@opt$par), "minsplit")
    checkEquals(names(bm$ors[[1]]@opt$perf), c("mmce.test.mean", "mmce.test.sd"))
		checkTrue(is.list(bm$ors[[1]]@path))
		checkTrue(length(bm$ors[[1]]@path) == length(ranges$minsplit))
		checkTrue(is.list(bm$res.result))
		checkEquals(length(bm$res.result$models), 1)
		checkTrue(is(bm$res.result$models[[1]], "wrapped.model"))
		checkTrue(is(bm$res.result$models[[1]]["learner.model"], "rpart"))
    checkTrue(is.null(bm$res.result$models))
		
		# normal benchmark - 2 par
		ranges <- list(minsplit=seq(3,10,2), cp=c(0.1, 0.11 , 0.09))
		wl = make.tune.wrapper("classif.rpart", resampling=inner, control=grid.control(ranges=ranges))
		cbr = .mlr.benchmark(wl, ct, outer, models=FALSE)
	}
}
