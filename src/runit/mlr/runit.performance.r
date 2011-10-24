test.performance <- function() {
	
	res = makeResampleDesc("Holdout")
	rf = resample("classif.rpart", task=binaryclass.task, resampling=res, measures=list(acc, timeboth))
  
	res = makeResampleDesc("BS", iters=3)
	rf = resample("classif.rpart", task=binaryclass.task, resampling=res, measures=list(acc, timeboth))
  m = setAggregation(acc, test.median)
  rf = resample("classif.rpart", task=binaryclass.task, resampling=res, measures=m)
  
	# custom measure
	res = makeResampleDesc("CV", iters=3)
	r = resample("classif.rpart", task=binaryclass.task, resampling=res)
	
	mymeasure = makeMeasure(id="mym", minimize=TRUE, classif=TRUE, allowed.pred.types=c("response"),
    fun=function(task, model, pred, extra.args) {
		# normal test error
		e1 = mean(pred@df$truth != pred@df$response)
		# we do this manually 
		id = pred@df$id
		t2 = getTargets(task)[id]
		e2 = mean(t2 != pred@df$response)
		checkEquals(e1, e2)
	})
	
	performance(r$pred, measure=mymeasure, task=binaryclass.task)
  
  
  # custom measure
  
  mymeasure = makeCustomResampledMeasure(id="mym", fun=function(task, group, pred, extra.args) {
    mean(pred@df$truth != pred@df$response)
  })
  rdesc = makeResampleDesc("Holdout")
  r = resample("classif.rpart", binaryclass.task, rdesc, measures=list(mmce, mymeasure))
  checkEquals(as.numeric(r$aggr["mmce.test.mean"]), as.numeric(r$aggr["custom.mym"]))
}	
