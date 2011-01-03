test.benchresult = function() {
	
	outer.len = 3
	inner = make.res.desc("cv", iter=2)

	ranges.svm = list(C=1:2)
	ctrl = grid.control(ranges=ranges.svm)
	svm.tuner = make.tune.wrapper("classif.ksvm", resampling=inner, control=ctrl)
	
	wl = make.learner("classif.ksvm", id="foo")
	blubb = make.tune.wrapper(wl, resampling=inner, control=ctrl)
	
	learners = c("classif.rpart", svm.tuner, blubb)
	res = make.res.desc("subsample", iter=outer.len)
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
	
	x = be["opt.results"][[1]]
	checkTrue(is.list(x) && length(x) == 3)
  x1 = x[["classif.rpart"]]
  x2 = x[["classif.ksvm"]]
  x3 = x[["foo"]]
  checkTrue(is.null(x1) && !is.null(x2) && !is.null(x3))
	checkTrue(is.list(x2) && length(x2) == 3)
	checkTrue(is.list(x3) && length(x3) == 3)
	checkTrue(!any(sapply(x2, is.null)))
	checkTrue(!any(sapply(x3, is.null)))
	
  x = tuned.pars(be, learner="classif.ksvm")
  checkTrue(all(x$C == 1 | x$C == 2))
  tp = tuned.pars(be, learner="foo")
	checkTrue(all(x$C == 1 | x$C == 2))

	ctrl = sequential.control(method="sbs", beta=100)
	vs = make.varsel.wrapper("classif.lda", resampling=inner, control=ctrl)
	
	learners = c("classif.rpart", vs)
	be = bench.exp(tasks=multiclass.task, learners=learners, resampling=res)
	x = replicate(3, multiclass.task["input.names"], F)
	y = sel.vars(be, learner.id="classif.lda", as.data.frame=FALSE)
	checkEquals(x, y)  
  x = as.data.frame(matrix(1, 3, 4))
  colnames(x) = multiclass.task["input.names"]
  y = sel.vars(be, learner.id="classif.lda")
  checkEquals(x, y)  
  
	tasks = list(multiclass.task, binaryclass.task)
	learners = c("classif.rpart", svm.tuner)
	be = bench.exp(tasks=tasks, learners=learners, resampling=res)
	x = as.array(be, learner="classif.rpart", sets="test", drop=TRUE)
	checkEquals(colnames(x), c("multiclass", "binary"))  
	checkTrue(all(apply(x, 2, function(y) is.numeric(y) && length(y) == outer.len)))  
	x = be["opt.results"]
	checkEquals(names(x), c("multiclass", "binary"))  
  checkTrue(all(sapply(x, function(y) is.list(y) && names(y) == c("classif.rpart", "classif.ksvm"))))  
  checkTrue(all(sapply(x, function(y) is.null(y[[1]]) && is.list(y[[2]]) && length(y[[2]]) == outer.len)))

  x = be["opt.results"]
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
  x1 = x[[multiclass.task["id"]]]
  checkTrue(is.list(x1) && length(x1) == 2 && is.matrix(x1[[1]]))  
  x1 = x[[multiclass.task["id"]]][["classif.rpart"]]
  checkTrue(is.matrix(x1))  
  
	ranges.svm = list(C=1:2, sigma=1:2)
	ctrl = grid.control(ranges=ranges.svm)
	svm.tuner = make.tune.wrapper("classif.ksvm", resampling=inner, control=ctrl)
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
	checkTrue(is(x1, "resample.prediction"))
	checkEquals(x1["iters"], outer.len)
	x1 = x[[2]]
	checkTrue(is.list(x1))
	checkEquals(names(x1), c("classif.ksvm"))
	x1 = x1[[1]]
	checkTrue(is(x1, "resample.prediction"))
	checkEquals(x1["iters"], outer.len)

	x = be["conf.mats"]
	checkEquals(names(x), c("multiclass", "binary"))
	x1 = x[[1]]
	checkTrue(is.list(x1))
	checkEquals(names(x1), c("classif.ksvm"))
	x1 = x1[[1]]
	checkTrue(is(x1, "matrix"))
	checkEquals(dim(x1), c(multiclass.task["class.nr"]+1, multiclass.task["class.nr"]+1))
	x1 = x[[2]]
	checkTrue(is.list(x1))
	checkEquals(names(x1), c("classif.ksvm"))
	x1 = x1[[1]]
	checkTrue(is(x1, "matrix"))
	checkEquals(dim(x1), c(binaryclass.task["class.nr"]+1, binaryclass.task["class.nr"]+1))
	
  # check aggregation
  res = make.res.desc("cv", iters=3)
  wl = "classif.lda"
  m = set.aggr(mmce, test.sd)
  r = resample(wl, multiclass.task, res, measure=m)
  be = bench.exp(learners=wl, tasks=multiclass.task, resampling=res)
  x = as.array(be, sets="test", drop=TRUE)
  checkEquals(as.numeric(r$aggr["mmce.test.sd"]), sd(x)) 

  res = make.res.desc("bs", iters=3)
  wl = "classif.lda"
  m = set.aggr(mmce, b632plus)
  r = resample(wl, multiclass.task, res, measures=m)
  be = bench.exp(learners=wl, tasks=multiclass.task, resampling=res)
  checkEquals(perf$aggr[,1], be["perf", aggr="resampling"], checkNames = FALSE) 
}	
