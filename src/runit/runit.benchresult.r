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
	
	x = be["perf", learner="classif.rpart"]
	checkTrue(is.numeric(x) && length(x) == outer.len)
	x = be["perf", learner="classif.ksvm"]
	checkTrue(is.numeric(x) && length(x) == outer.len)
	x = be["perf", learner="foo"]
	checkTrue(is.numeric(x) && length(x) == outer.len)

	x = be["perf", learner="classif.rpart", drop=FALSE]
	y = be["perf", learner="classif.rpart"]
	y = array(y, dim=c(outer.len,1,1), dimnames=list(1:outer.len, "classif.rpart", "mmce"))
	y = list(multiclass=y)
	checkEquals(x, y)
	x = be["perf", learner="classif.ksvm", drop=FALSE]
	y = be["perf", learner="classif.ksvm"]
	y = array(y, dim=c(outer.len,1,1), dimnames=list(1:outer.len, "classif.ksvm", "mmce"))
	y = list(multiclass=y)
	checkEquals(x, y)
	x = be["perf", learner="foo", drop=FALSE]
	y = be["perf", learner="foo"]
	y = array(y, dim=c(outer.len,1,1), dimnames=list(1:outer.len, "foo", "mmce"))
	y = list(multiclass=y)
	checkEquals(x, y)

	x1 = be["perf", learner="classif.rpart"]
	x2 = be["perf", learner="classif.ksvm"]
	y =  be["perf", learner=c("classif.rpart", "classif.ksvm")]
	checkEquals(cbind(classif.rpart=x1,classif.ksvm=x2), y)
	
	x = be["tuned.par"]
	checkTrue(is.list(x) && length(x) == 3)
	checkTrue(is.null(x[[1]]) && !is.null(x[[2]]) && !is.null(x[[3]]))
	x1 = be["tuned.par", learner="classif.ksvm"]
	x2 = be["tuned.par", learner="foo"]
	checkEquals(x[[2]], x1)		
	checkEquals(x[[3]], x2)		
	checkTrue(is.list(x1) && length(x1) == 3)
	checkTrue(is.list(x2) && length(x2) == 3)
	checkTrue(!any(sapply(x1, is.null)))
	checkTrue(!any(sapply(x2, is.null)))
	
	checkTrue(all(sapply(x1, function(z) z == 1 || z == 2)))
	checkTrue(all(sapply(x2, function(z) z == 1 || z == 2)))

	checkEquals(be["opt.result", learner="classif.ksvm"][[1]]["par"]$C, be["tuned.par", learner="classif.ksvm"][[1]])		
	checkEquals(be["opt.result", learner="classif.ksvm"][[2]]["par"]$C, be["tuned.par", learner="classif.ksvm"][[2]])		
	checkEquals(be["opt.result", learner="classif.ksvm"][[3]]["par"]$C, be["tuned.par", learner="classif.ksvm"][[3]])
	

	ctrl = sequential.control(method="sbs", beta=100)
	vs = make.varsel.wrapper("classif.lda", resampling=inner, control=ctrl)
	
	learners = c("classif.rpart", vs)
	be = bench.exp(tasks=multiclass.task, learners=learners, resampling=res)
	x = replicate(3, multiclass.task["input.names"], F)
	y = be["sel.var", learner="classif.lda"]
	checkEquals(x, y, checkNames=FALSE)  

	tasks = list(multiclass.task, binaryclass.task)
	learners = c("classif.rpart", svm.tuner)
	be = bench.exp(tasks=tasks, learners=learners, resampling=res)
	x = be["perf", learner="classif.rpart"]
	checkEquals(names(x), c("multiclass", "binary"))  
	checkTrue(all(sapply(x, function(y) is.numeric(y) && length(y) == outer.len)))  
	x = be["tuned.par", learner="classif.ksvm"]
	checkEquals(names(x), c("multiclass", "binary"))  
	checkTrue(all(sapply(x, function(y) is.list(y) && length(y) == outer.len)))
	
	x = be["tuned.par"]
	checkEquals(names(x), c("multiclass", "binary"))  
	checkTrue(all(sapply(x, function(y) is.list(y) && names(y) == c("classif.rpart", "classif.ksvm"))))  

	x = be["tuned.par", as.data.frame=TRUE]
	checkEquals(names(x), c("multiclass", "binary"))  
	checkTrue(all(sapply(x, function(y) is.list(y) && names(y) == c("classif.rpart", "classif.ksvm"))))
	checkTrue(is.null(x[[1]][[1]]))  
	checkTrue(is.null(x[[2]][[1]]))  
	checkTrue(is.numeric(x[[1]][[2]]))  
	checkTrue(is.numeric(x[[2]][[2]]))  

	ranges.svm = list(C=1:2, sigma=1:2)
	ctrl = grid.control(ranges=ranges.svm)
	svm.tuner = make.tune.wrapper("classif.ksvm", resampling=inner, control=ctrl)
	learners = c(svm.tuner)
	be = bench.exp(tasks=tasks, learners=learners, resampling=res, predictions=TRUE, conf.mats=TRUE)
	
	x = be["tuned.par", as.data.frame=TRUE]
	checkEquals(names(x), c("multiclass", "binary"))  
	checkTrue(is.data.frame(x[[1]]))  
	checkTrue(is.data.frame(x[[2]]))  
	
	x = be["prediction", drop=FALSE]
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

	x = be["conf.mats", drop=FALSE]
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
	
}	
