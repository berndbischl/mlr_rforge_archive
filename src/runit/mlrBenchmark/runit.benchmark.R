test.benchmark <- function() {
	outer = makeResampleDesc("CV", iters=3)
	inner = makeResampleDesc("CV", iters=2)

	checkException(benchmark(list(), multiclass.task, resampling=outer), silent=TRUE)
	s = geterrmessage()
	checkTrue(length(grep("No learners were", s)) >0 )
	checkException(benchmark("", multiclass.task, resampling=outer), silent=TRUE)
	s = geterrmessage()
	checkTrue(length(grep("Cannot create learner", s)) >0 )
	checkException(benchmark("classif.lda", list(), resampling=outer), silent=TRUE)
	s = geterrmessage()
	checkTrue(length(grep("No tasks were", s)) >0 )
	
  ps1 = makeParameterSet(
    makeDiscreteParameter("minsplit", vals=seq(3,10,2))
  )
	rpart.tuner = makeTuneWrapper("classif.rpart", resampling=inner, par.set=ps1, control=makeTuneControlGrid())
	learners = list("classif.lda", rpart.tuner)

	be = benchmark("classif.lda", multiclass.task, resampling=outer)
  a = as.array(be)
  checkEquals(mean(a[,"test","classif.lda","mmce",multiclass.task@desc@id]), 
    be["aggrs"][[multiclass.task@desc@id]][["classif.lda"]]["mmce.test.mean"],
    checkNames=FALSE)
 
	outer2 = makeResampleDesc("Holdout")
	be = benchmark("classif.lda", multiclass.task, resampling=outer2)
  checkTrue(!is.na(be["aggrs"]["mmce.test.mean"]))
  
	wl = makeLearner("classif.lda")
	be = benchmark(wl,  multiclass.task, resampling=outer)
	print(be)	
	be = benchmark(rpart.tuner,  multiclass.task, resampling=outer)
	print(be)
  foo = makeMeasure(id="foo", minimize=TRUE,  
    fun=function(task, model, pred, extra.pars) {
      tt = pred
      1
    }
  )
	be = benchmark(learners, multiclass.task, resampling=outer, measures=list(acc, timeboth, foo))
  # check that same resample instance was used for both learners
  checkEquals(
    be@res.results[[1]][[1]]$pred@df$id, 
    be@res.results[[1]][[2]]$pred@df$id 
  )  
	print(be)
  a = as.array(be)
	checkEquals(dim(a), c(outer@iters, 2, length(learners), 3, 1))	
	
	be = benchmark("regr.lm", regr.task, resampling=outer)
	print(be)
}
