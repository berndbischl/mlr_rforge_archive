test.opt.wrapper <- function() {
	
	outer = make.res.instance(make.res.desc("holdout"), task=multiclass.task)
  inner = make.res.desc("cv", iters=2)
	
	ps = makeParameterSet(makeDiscreteParameter(id="C", vals=c(1,100)))
	svm.tuner = makeTuneWrapper("classif.ksvm", resampling=inner, par.set=ps, control=grid.control())
	
	m = train(svm.tuner, task=multiclass.task)
	
	or = m["opt.result"]
	checkEquals(or@x, list(C=1))
	
	checkTrue(!is.null(or["y"]))
	checkTrue(!is.null(or["path"]))
  
  p = predict(m, task=multiclass.task)
  checkTrue(!any(is.na(p["response"])))
}
