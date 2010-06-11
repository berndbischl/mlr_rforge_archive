test.benchexp <- function() {
	outer = make.res.desc("cv", iters=3)
	inner = make.res.desc("cv", iters=2)

	checkException(bench.exp(list(), multiclass.task, resampling=outer), silent=TRUE)
	s = geterrmessage()
	checkTrue(length(grep("No learners were", s)) >0 )
	checkException(bench.exp("", multiclass.task, resampling=outer), silent=TRUE)
	s = geterrmessage()
	checkTrue(length(grep("Cannot create learner", s)) >0 )
	checkException(bench.exp("classif.lda", list(), resampling=outer), silent=TRUE)
	s = geterrmessage()
	checkTrue(length(grep("No tasks were", s)) >0 )
	
	
	r = list(minsplit=seq(3,10,2))
	rpart.tuner = make.tune.wrapper("classif.rpart", resampling=inner, control=grid.control(ranges=r))
	learners = list("classif.lda", rpart.tuner)

	be = bench.exp("classif.lda", multiclass.task, resampling=outer)
	checkEquals(mean(be@perf[[1]][1:3,,"mmce"]), be["perf", aggr="mean"])
	
	outer2 = make.res.desc("holdout")
	be = bench.exp("classif.lda", multiclass.task, resampling=outer2)
	x = be["perf", aggr="mean"]
	checkTrue(!is.na(x))
	
	wl = make.learner("classif.lda")
	be = bench.exp(wl,  multiclass.task, resampling=outer)
	print(be)	
	be = bench.exp(rpart.tuner,  multiclass.task, resampling=outer)
	print(be)
	ms = list("acc", time="time", foo=function(x,task) 1)
	be = bench.exp(learners, multiclass.task, resampling=outer, measures=ms)
	print(be)	
	x = be["perf", learner=c("classif.lda", "classif.rpart")]
	checkTrue(is.list(x))
	checkEquals(length(x), 1)
	checkEquals(dim(x[[1]]), c(3, 2, 3))	
	x = be["perf", learner=c("classif.lda", "classif.rpart"), measure="acc", drop=F]
	checkEquals(length(x), 1)
	checkEquals(dim(x[[1]]), c(3, 2, 1))	
	
	be = bench.exp("regr.lm", regr.task, resampling=outer)
	print(be)
}