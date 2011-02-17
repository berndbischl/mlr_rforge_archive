test.performance <- function() {
	
	res = makeResampleDesc("holdout")
	rf = resample("classif.rpart", task=binaryclass.task, resampling=res, measures=list(acc, time.all))
  
	res = makeResampleDesc("bs", iters=3)
	rf = resample("classif.rpart", task=binaryclass.task, resampling=res, measures=list(acc, time.all))
  m = setAggr(acc, test.median)
  rf = resample("classif.rpart", task=binaryclass.task, resampling=res, measures=m)
  
	# custom measure
	res = makeResampleDesc("cv", iters=3)
	r = resample("classif.rpart", task=binaryclass.task, resampling=res)
	
	mymeasure = makeMeasure(id="mym", minimize=TRUE,  
    fun=function(task, model, pred, extra.pars) {
		# normal test error
		e1 = mean(pred@df$truth != pred@df$response)
		# we do this manually 
		id = pred["id"]
		t2 = targets(task)[id]
		e2 = mean(t2 != pred@df$response)
		checkEquals(e1, e2)
	})
	
	performance(r$pred, measure=mymeasure, task=binaryclass.task)
}	
