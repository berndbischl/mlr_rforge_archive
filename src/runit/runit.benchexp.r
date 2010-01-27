test.benchexp <- function() {
	
	outer = make.res.desc("cv", iters=2)
	inner = make.res.desc("cv", iters=2)
	
	r = list(minsplit=seq(3,10,2))
	rpart.tuner = make.tune.wrapper("rpart.classif", resampling=inner, control=grid.control(ranges=r))
	learners = list("lda", rpart.tuner)

	bench.exp("lda", multiclass.task, resampling=outer)
	wl = make.learner("lda")
	be = bench.exp(wl,  multiclass.task, resampling=outer)
	be = bench.exp(rpart.tuner,  multiclass.task, resampling=outer)
	be = bench.exp(learners, multiclass.task, resampling=outer)
	print(be)	
	
	be = bench.exp("stats.lm", regr.task, resampling=outer)
	print(be)
}