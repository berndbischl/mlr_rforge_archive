test.performance <- function() {
	
	res = make.res.desc("holdout")
	rf = resample.fit("classif.rpart", task=binaryclass.task, resampling=res)
	performance(rf, measures=c("acc", "time"))

	res = make.res.desc("bs", iters=3)
	rf = resample.fit("classif.rpart", task=binaryclass.task, resampling=res)
	performance(rf, measures=c("acc", "time"))

	# custom measure
	res = make.res.desc("cv", iters=3)
	rf = resample.fit("classif.rpart", task=binaryclass.task, resampling=res)
	
	mymeasure = function(x, task) {
		# normal test error
		e1 = mean(x["truth"] != x["response"])
		# we do this manually 
		id = x["id"]
		tn = x["task.desc"]["target"]
		t2 = task["data"][id, tn]
		e2 = mean(t2 != x["response"])
		checkEquals(e1, e2)
	}
	attr(mymeasure, "name") = "mym"
	
	performance(rf, measures=mymeasure, task=binaryclass.task)
	
	# losses
	
	p = performance(rf, measures="mmce", losses="zero-one")
	checkEquals(p$measures["combine", "mmce"], mean(p$losses[,"zero-one"]))
	
	m = train("classif.lda", task=multiclass.task)
	pred = predict(m, newdata=multiclass.df)
	p = performance(pred, measures="mmce", losses="zero-one")
	checkEquals(as.numeric(p$measures["mmce"]), mean(p$losses[,"zero-one"]))
}	
