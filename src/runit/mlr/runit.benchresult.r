test.benchresult = function() {
	
	outer.len = 3
	inner = makeResampleDesc("CV", iter=2)

	ps = makeParameterSet(makeDiscreteParameter("C", vals=1:2))
	ctrl = makeTuneControlGrid()
	svm.tuner = makeTuneWrapper("classif.ksvm", resampling=inner, par.set=ps, control=ctrl)
	
	wl = makeLearner("classif.ksvm", id="foo")
	blubb = makeTuneWrapper(wl, resampling=inner, control=ctrl, par.set=ps)
	
	learners = c("classif.rpart", svm.tuner, blubb)
	res = makeResampleDesc("Subsample", iter=outer.len)
	be = bench.exp(tasks=multiclass.task, learners=learners, resampling=res)
	
	x = as.array(be, learner="classif.rpart", sets="test", drop=TRUE)
	checkTrue(is.numeric(x) && length(x) == outer.len)
  x = as.array(be, learner="classif.ksvm", sets="test", drop=TRUE)
  checkTrue(is.numeric(x) && length(x) == outer.len)
  x = as.array(be, learner="foo", sets="test", drop=TRUE)
  checkTrue(is.numeric(x) && length(x) == outer.len)
  x = as.array(be)
  checkEquals(dimnames(x), 
    list(as.character(1:outer.len), c("test", "train"), c("classif.rpart","classif.ksvm","foo"), "mmce", "multiclass"))
  
  x1 = as.array(be, learner="classif.rpart", sets="test", drop=TRUE)
  x2 = as.array(be, learner="classif.ksvm", sets="test", drop=TRUE)
  y = as.array(be, learner=c("classif.rpart", "classif.ksvm"), sets="test", drop=TRUE)
	checkEquals(cbind(classif.rpart=x1,classif.ksvm=x2), y)
	
	x = be@opt.results[[1]]
	checkTrue(is.list(x) && length(x) == 3)
  x1 = x[["classif.rpart"]]
  x2 = x[["classif.ksvm"]]
  x3 = x[["foo"]]
  checkTrue(is.null(x1) && !is.null(x2) && !is.null(x3))
	checkTrue(is.list(x2) && length(x2) == 3)
	checkTrue(is.list(x3) && length(x3) == 3)
	checkTrue(!any(sapply(x2, is.null)))
	checkTrue(!any(sapply(x3, is.null)))
	
  x = getTunedParameters(be, learner="classif.ksvm", as.data.frame=TRUE)
  checkTrue(is.data.frame(x))
  checkEquals(dim(x), c(outer.len, 1))
  checkEquals(colnames(x), c("C"))
  checkTrue(all(x$C == 1 | x$C == 2))
  x = getTunedParameters(be, learner="classif.ksvm", as.data.frame=FALSE)
  checkTrue(is.list(x))
  checkEquals(length(x), outer.len)
  checkTrue(all(sapply(x, is.list)))
  checkTrue(all(sapply(x, function(y) length(y)==1)))
  checkTrue(all(sapply(x, function(y) names(y)=="C")))
  checkTrue(all(sapply(x, function(y) y$C == 1 || y$C == 2)))
  tp = getTunedParameters(be, learner="foo")
	checkTrue(all(x$C == 1 | x$C == 2))

	ctrl = makeVarselControlSequential(method="sbs", beta=100)
	vs = makeVarselWrapper("classif.lda", resampling=inner, control=ctrl)
	
	learners = c("classif.rpart", vs)
	be = bench.exp(tasks=multiclass.task, learners=learners, resampling=res)
	x = replicate(3, getFeatureNames(multiclass.task), F)
	y = getSelectedFeatures(be, learner.id="classif.lda", as.data.frame=FALSE)
	checkEquals(x, y)  
  x = as.data.frame(matrix(1, 3, 4))
  colnames(x) = getFeatureNames(multiclass.task)
  y = getSelectedFeatures(be, learner.id="classif.lda", as.data.frame=TRUE)
  checkEquals(x, y)  
  
	tasks = list(multiclass.task, binaryclass.task)
	learners = c("classif.rpart", svm.tuner)
	be = bench.exp(tasks=tasks, learners=learners, resampling=res)
	x = as.array(be, learner="classif.rpart", sets="test", drop=TRUE)
	checkEquals(colnames(x), c("multiclass", "binary"))  
	checkTrue(all(apply(x, 2, function(y) is.numeric(y) && length(y) == outer.len)))  
	x = be@opt.results
	checkEquals(names(x), c("multiclass", "binary"))  
  checkTrue(all(sapply(x, function(y) is.list(y) && names(y) == c("classif.rpart", "classif.ksvm"))))  
  checkTrue(all(sapply(x, function(y) is.null(y[[1]]) && is.list(y[[2]]) && length(y[[2]]) == outer.len)))

  x = be@opt.results
	checkEquals(names(x), c("multiclass", "binary"))  
	checkTrue(all(sapply(x, function(y) is.list(y) && names(y) == c("classif.rpart", "classif.ksvm"))))
	checkTrue(is.null(x[[1]][[1]]))  
	checkTrue(is.null(x[[2]][[1]]))  
	checkTrue(is(x[[1]][[2]][[1]], "opt.result"))  
  checkTrue(is(x[[1]][[2]][[2]], "opt.result"))  
  checkTrue(is(x[[1]][[2]][[3]], "opt.result"))  
  checkTrue(is(x[[2]][[2]][[1]], "opt.result"))  
  checkTrue(is(x[[2]][[2]][[2]], "opt.result"))  
  checkTrue(is(x[[2]][[2]][[3]], "opt.result"))  
  
  x = be["conf.mats"]
  checkTrue(is.list(x) && length(x) == 2 && is.list(x[[1]]) && length(x[[1]]) == 2)  
  x1 = x[[multiclass.task@desc@id]]
  checkTrue(is.list(x1) && length(x1) == 2 && is.matrix(x1[[1]]))  
  x1 = x[[multiclass.task@desc@id]][["classif.rpart"]]
  checkTrue(is.matrix(x1))  
  
	ps = makeParameterSet(makeDiscreteParameter("C", vals=1:2), makeDiscreteParameter("sigma", vals=1:2))
	svm.tuner = makeTuneWrapper("classif.ksvm", resampling=inner, par.set=ps, control=makeTuneControlGrid())
	learners = c(svm.tuner)
	be = bench.exp(tasks=tasks, learners=learners, resampling=res)
  
  #tp
#	x = be["opt.par", as.data.frame=TRUE]
#	checkEquals(names(x), c("multiclass", "binary"))  
#	checkTrue(is.data.frame(x[[1]]))  
#	checkTrue(is.data.frame(x[[2]]))  
	
	x = be["predictions"]
	checkEquals(names(x), c("multiclass", "binary"))
	x1 = x[[1]]
	checkTrue(is.list(x1))
	checkEquals(names(x1), c("classif.ksvm"))
	x1 = x1[[1]]
	checkTrue(is(x1, "ResamplePrediction"))
	checkEquals(x1@instance@desc@iters, outer.len)
	x1 = x[[2]]
	checkTrue(is.list(x1))
	checkEquals(names(x1), c("classif.ksvm"))
	x1 = x1[[1]]
	checkTrue(is(x1, "ResamplePrediction"))
	checkEquals(x1@instance@desc@iters, outer.len)

	x = be["conf.mats"]
	checkEquals(names(x), c("multiclass", "binary"))
	x1 = x[[1]]
	checkTrue(is.list(x1))
	checkEquals(names(x1), c("classif.ksvm"))
	x1 = x1[[1]]
	checkTrue(is(x1, "matrix"))
	checkEquals(dim(x1), c(length(getClassLevels(multiclass.task))+1, length(getClassLevels(multiclass.task))+1))
	x1 = x[[2]]
	checkTrue(is.list(x1))
	checkEquals(names(x1), c("classif.ksvm"))
	x1 = x1[[1]]
	checkTrue(is(x1, "matrix"))
	checkEquals(dim(x1), c(length(getClassLevels(binaryclass.task))+1, length(getClassLevels(binaryclass.task))+1))
	
  # check aggregation
  res = makeResampleInstance(makeResampleDesc("CV", iters=3), task=multiclass.task) 
  wl = "classif.lda"
  m = setAggregation(mmce, test.sd)
  r = resample(wl, multiclass.task, res, measure=m)
  be = bench.exp(learners=wl, tasks=multiclass.task, resampling=res)
  x = as.array(be, sets="test", drop=TRUE)
  checkEquals(as.numeric(r$aggr["mmce.test.sd"]), sd(x)) 
}	