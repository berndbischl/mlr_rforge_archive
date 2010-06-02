



test.opt.wrapper <- function() {
	
	outer = make.res.instance("holdout", task=ct)
	inner = make.res.desc("cv", iters=2)
	
	ranges.svm = list(kernel="rbfdot", C=1:2)
	control.svm = grid.control(ranges=ranges.svm)
	svm.tuner = make.tune.wrapper("classif.ksvm", resampling=inner, control=control.svm)
	
	m = train(svm.tuner, task=ct)
	
	checkEquals(m["opt"]$par, m["opt.par"])
	checkEquals(m["tuned.par"], m["opt.par"])
	checkEquals(m["sel.var"], NULL)
	checkEquals(m["opt"]$perf, m["opt.perf"])
	
	checkTrue(!is.null(m["path"]))

}