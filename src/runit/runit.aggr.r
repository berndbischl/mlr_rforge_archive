#test.aggr <- function() {
#  
#  if (!use.package) {
#	  a = make.aggrs(mean)
#	  checkEquals(a, list(mean=mean))
#  }
#	
#	ct = binaryclass.task
#	
#	ms = c("mmce", "acc", "tp", "fp", "tn", "fn", "tpr", "fpr", "tnr", "fnr", "ppv", "npv", "mcc", "f1" )
#	ms = c("mmce", "tpr", "fpr")
#
#	res = makeResampleDesc("cv", iters=3)
#	rf = resample("classif.lda", task=ct, resampling=res)
#	f = mean
#	attr(f, "id") = "foo"
#	perf = performance(rf, aggr=list("combine", "mean", f))
#	print(perf)
#	
##	m = train("classif.rpart", task=ct, subset=binaryclass.train.inds)
##	pred = predict(m, task=ct, subset=binaryclass.test.in
#}
