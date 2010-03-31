test.measures <- function() {
	
	ct = binaryclass.task
	
	learners = c("lda", "rpart.classif")
	ms = c("mmce", "acc", "tp", "fp", "tn", "fn", "tpr", "fpr", "tnr", "fnr", "ppv", "npv", "mcc", "f1" )
	res = make.res.desc("cv", iters=10)
	m = train("rpart.classif", task=ct, subset=binaryclass.train.inds)
	pred = predict(m, task=ct, subset=binaryclass.test.inds)
	perf = performance(pred, measures=ms)
	print(perf)
	
	be = bench.exp(tasks=ct, learners=learners, measures = ms, resampling=res)
	print(be[aggr=list(combine="combine")])
}