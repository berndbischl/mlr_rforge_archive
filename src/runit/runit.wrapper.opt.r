test.OptWrapper <- function() {
	
	outer = makeResampleDesc("Holdout")
  inner = makeResampleDesc("CV", iters=2)
	
	ps = makeParameterSet(makeDiscreteParameter(id="C", vals=c(1,100)))
	svm.tuner = makeTuneWrapper("classif.ksvm", resampling=inner, par.set=ps, control=makeTuneGridControl())
	
	m = train(svm.tuner, task=multiclass.task)
	
	or = m@opt.result
	checkEquals(or@x, list(C=1))
	
  p = predict(m, task=multiclass.task)
  checkTrue(!any(is.na(p@df$response)))

  ps = makeParameterSet(
    makeNumericParameter(id="C", trafo=function(x) 2^x),
    makeNumericParameter(id="epsilon", trafo=function(x) 2^x),
    makeNumericParameter(id="sigma", trafo=function(x) 2^x)
  )
  svm.tuner = makeTuneWrapper("regr.ksvm", resampling=inner, par.set=ps, 
    control=makeTuneOptimControl(start=c(0,0,0), maxit=5))
  
  m = train(svm.tuner, task=regr.task)
  or = m@opt.result
  checkEquals(length(as.list(or@path)), 5+1)
  checkTrue(!any(is.na(as.data.frame(or@path)$mse.test.mean)))
  
  
  # check that predict.type is taken from base learner
  w = makeLearner("classif.ksvm", predict.type="prob")
  svm.tuner = makeTuneWrapper(w, resampling=makeResampleDesc("Holdout"), par.set=ps, control=makeTuneGridControl())
  checkEquals(svm.tuner@predict.type, "prob")
  r = resample(svm.tuner, binaryclass.task, makeResampleDesc("Holdout"), measures=auc)
  checkTrue(!is.na(r$aggr["auc.test.mean"]))
}
