test.benchmark <- function() {
	if (!use.package) {
		ct = makeClassifTask("iris", data=iris, target="Species")
		outer = makeResampleDesc("holdout") 
		inner = new("cv.desc", iters=3L)
		
		# check empty ranges 
		cbr <- .mlr.benchmark("classif.rpart", task=ct, resampling=outer, models=TRUE)
	
		# normal benchmark - tune wrapper with one par
		ps1 = makeParameterSet(
      makeDiscreteParameter("minsplit", vals=seq(3,10,2))
    ) 
		wl = makeTuneWrapper("classif.rpart", resampling=inner, par.set=ps1, control=grid.control())
		bm = .mlr.benchmark(wl, ct, outer, models=TRUE)
		checkTrue(is.list(bm$ors))
		checkEquals(length(bm$ors), 1)
    checkTrue(is.list(bm$ors[[1]]@opt$x))
    checkEquals(names(bm$ors[[1]]@opt$x), "minsplit")
    checkEquals(names(bm$ors[[1]]@opt$perf), c("mmce.test.mean", "mmce.test.sd"))
		checkTrue(is.list(bm$ors[[1]]@path))
		checkTrue(length(bm$ors[[1]]@path) == length(ranges$minsplit))
		checkTrue(is.list(bm$res.result))
		checkEquals(length(bm$res.result$models), 1)
		checkTrue(is(bm$res.result$models[[1]], "WrappedModel"))
		checkTrue(is(bm$res.result$models[[1]]["learner.model"], "rpart"))
    
    bm = .mlr.benchmark(wl, ct, outer, models=FALSE)
    checkTrue(is.null(bm$res.result$models))

    # normal benchmark - 2 par
		ranges <- list(minsplit=seq(3,10,2), cp=c(0.1, 0.11 , 0.09))
		wl = makeTuneWrapper("classif.rpart", resampling=inner, control=grid.control(ranges=ranges))
		cbr = .mlr.benchmark(wl, ct, outer, models=FALSE)
	}
}
