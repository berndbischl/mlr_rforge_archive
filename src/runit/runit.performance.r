test.performance <- function() {
	
	res = make.res.desc("holdout")
	rf = resample("classif.rpart", task=binaryclass.task, resampling=res, measures=list(acc, time.all))
  
	res = make.res.desc("bs", iters=3)
	rf = resample("classif.rpart", task=binaryclass.task, resampling=res, measures=list(acc, time.all))
  m = set.aggr(acc, test.median)
  rf = resample("classif.rpart", task=binaryclass.task, resampling=res, measures=m)
  
	# custom measure
	res = make.res.desc("cv", iters=3)
	r = resample("classif.rpart", task=binaryclass.task, resampling=res)
	
	mymeasure = make.measure(id="mym", minimize=TRUE,  
    fun=function(task, model, pred, extra.pars) {
		# normal test error
		e1 = mean(pred["truth"] != pred["response"])
		# we do this manually 
		id = pred["id"]
		t2 = task["targets"][id]
		e2 = mean(t2 != pred["response"])
		checkEquals(e1, e2)
	})
	
	performance(r$pred, measure=mymeasure, task=binaryclass.task)
	
	# losses
	
	p = performance(rf, measures="mmce", losses="zero-one", aggr=c("combine"))
	checkEquals(p$aggr["combine", "mmce"], mean(p$losses[,"zero-one"]))
	
	m = train("classif.lda", task=multiclass.task)
	pred = predict(m, newdata=multiclass.df)
	p = performance(pred, measures="mmce", losses="zero-one")
	checkEquals(as.numeric(p$measures["mmce"]), mean(p$losses[,"zero-one"]))
	
	res = make.res.desc("bs632", iters=2)
	p = resample("classif.rpart", task=binaryclass.task, resampling=res)
	perf1 = performance(p, measures=c("mmce"), losses=c("zero-one"))
	ls = perf1$losses
#	ls11 = ls[ls$group == "train" & ls$iter==1, "zero-one"]
#	ls12 = ls[ls$group == "test" &  ls$iter==1, "zero-one"]
#	ls1 = 0.368*mean(ls11) + 0.632*mean(ls12)
#	ls21 = ls[ls$group == "train" & ls$iter==2, "zero-one"]
#	ls22 = ls[ls$group == "test" &  ls$iter==2, "zero-one"]
#	ls2 = 0.368*mean(ls21) + 0.632*mean(ls22)
#	ag = perf1$aggr.group
#	checkEquals(ls1, ag[1, "mmce"])
#	checkEquals(ls2, ag[2, "mmce"])
#	checkEquals(mean(c(ls1, ls2)), perf1$aggr[1, "mmce"])
#  # check that combine works at least
#  p2 = as(p, "grouped.prediction")
#  checkTrue(setequal(colnames(p2@df), c("truth", "response", "id", "group")))
#	perf2 = performance(p, measures=c("mmce"), aggr="combine")
}	
