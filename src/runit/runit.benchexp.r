test.benchexp <- function() {
	
	outer = make.cv.desc(iters=2)
	inner = make.cv.desc(iters=2)
	
	ct = make.classif.task(data=multiclass.df, target=multiclass.target)
	
	r = list(minsplit=seq(3,10,2))
	rpart.tuner = make.tune.wrapper("rpart.classif", resampling=inner, control=grid.control(ranges=r))
	learners = list("lda", rpart.tuner)

	bench.exp("lda", ct, resampling=outer)
	wl = make.learner("lda")
	be = bench.exp(wl, ct, resampling=outer)
	be = bench.exp(rpart.tuner, ct, resampling=outer)
	be = bench.exp(learners, ct, resampling=outer)
	print(be)	
	
	rt = make.regr.task(data=regr.df, target=regr.target)
	be = bench.exp("stats.lm", rt, resampling=outer)
	print(be)
}