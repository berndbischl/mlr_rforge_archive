test.performance <- function() {
	
	res = make.res.desc("holdout")
	rf = resample("classif.rpart", task=binaryclass.task, resampling=res)
	performance(rf, measures=c("acc", "time"))

	res = make.res.desc("bs", iters=3)
	rf = resample("classif.rpart", task=binaryclass.task, resampling=res)
	performance(rf, measures=c("acc", "time"))
	performance(rf, measures=c("acc", "time"), aggr="mean")
	performance(rf, measures=c("acc", "time"), aggr=c("mean", "combine"))
	
	# custom measure
	res = make.res.desc("cv", iters=3)
	rf = resample("classif.rpart", task=binaryclass.task, resampling=res)
	
	mymeasure = function(x, task) {
		# normal test error
		e1 = mean(x["truth"] != x["response"])
		# we do this manually 
		id = x["id"]
		tn = x["data.desc"]["target"]
		t2 = task["data"][id, tn]
		e2 = mean(t2 != x["response"])
		checkEquals(e1, e2)
	}
	attr(mymeasure, "id") = "mym"
	
	performance(rf, measures=mymeasure, task=binaryclass.task)
	
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
