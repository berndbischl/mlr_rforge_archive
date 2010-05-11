test.measures <- function() {
	
	ct = binaryclass.task
	
	learners = c("classif.lda", "classif.rpart")
	ms = c("mmce", "acc", "tp", "fp", "tn", "fn", "tpr", "fpr", "tnr", "fnr", "ppv", "npv", "mcc", "f1" )
	res = make.res.desc("cv", iters=3)
	m = train("classif.rpart", task=ct, subset=binaryclass.train.inds)
	pred = predict(m, task=ct, subset=binaryclass.test.inds)
	perf = performance(pred, measures=ms)
	print(perf)
	ms = c("mmce", bla=function(x, task) 1)
	perf = performance(pred, measures=ms)
	checkEquals(names(perf$measures), c("mmce", "bla"))
}