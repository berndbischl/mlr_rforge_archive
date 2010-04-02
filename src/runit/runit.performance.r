test.performance <- function() {
	
	res = make.res.desc("holdout")
	rf = resample.fit("rpart", task=binaryclass.task, resampling=res)
	performance(rf, measures=c("acc", "time"))

	res = make.res.desc("bs", iters=3)
	rf = resample.fit("rpart", task=binaryclass.task, resampling=res)
	performance(rf, measures=c("acc", "time"))

	# custom measure
	res = make.res.desc("cv", iters=3)
	rf = resample.fit("rpart", task=binaryclass.task, resampling=res)
	
	mymeasure = function(x, task) {
		# normal test error
		e1 = mean(x["truth"] != x["response"])
		# we do this manually 
		id = x["id"]
		tn = x["task.desc"]["target"]
		print(tn)
		t2 = task["data"][id, tn]
		e2 = mean(t2 != x["response"])
		print(c(e1, e2))
		checkEquals(e1, e2)

	}
	attr(mymeasure, "name") = "mym"
	
	performance(rf, measures=mymeasure, task=binaryclass.task)
	
	
}	
