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

	x = be["perf", learner="classif.rpart", drop=F]
	y = be["perf", learner="classif.rpart"]
	y = array(y, dim=c(outer.len,1,1), dimnames=list(1:outer.len, "classif.rpart", "mmce"))
	y = list(multiclass=y)
	checkEquals(x, y)
	x = be["perf", learner="classif.ksvm", drop=F]
	y = be["perf", learner="classif.ksvm"]
	y = array(y, dim=c(outer.len,1,1), dimnames=list(1:outer.len, "classif.ksvm", "mmce"))
	y = list(multiclass=y)
	checkEquals(x, y)
	x = be["perf", learner="foo", drop=F]
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
	
	checkTrue(all(sapply(x1, function(z) z$C == 1 || z$C == 2)))
	checkTrue(all(sapply(x2, function(z) z$C == 1 || z$C == 2)))

	checkEquals(be["opt.result", learner="classif.ksvm"]["par"], be["opt.result", learner="classif.ksvm"]["tuned.par"])		
	checkEquals(be["opt.result", learner="classif.ksvm"][[1]]["par"], be["tuned.par", learner="classif.ksvm"][[1]])		
	checkEquals(be["opt.result", learner="classif.ksvm"][[2]]["par"], be["tuned.par", learner="classif.ksvm"][[2]])		
	checkEquals(be["opt.result", learner="classif.ksvm"][[3]]["par"], be["tuned.par", learner="classif.ksvm"][[3]])
	

	ctrl = varsel.control(beta=100)
	vs = make.varsel.wrapper("classif.lda", resampling=inner, method="sbs", control=ctrl)
	
	learners = c("classif.rpart", vs)
	be = bench.exp(tasks=multiclass.task, learners=learners, resampling=res)
	x = replicate(3, multiclass.task["input.names"], F)
	y = be["sel.var", learner="classif.lda"]
	checkEquals(x, y, checkNames=F)  
}	
	