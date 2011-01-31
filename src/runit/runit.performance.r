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
		id = pred@id
		t2 = task["targets"][id]
		e2 = mean(t2 != pred["response"])
		checkEquals(e1, e2)
	})
	
	performance(r$pred, measure=mymeasure, task=binaryclass.task)
}	
