#test.losses <- function() {
#
#	costs = matrix(c(0,1,2,0), 2, 2)
#	rownames(costs) = colnames(costs) = c("M", "R")
#	ct = makeClassifTask(data=binaryclass.df, target=binaryclass.target, costs=costs, positive="M")
#
#	m = train("classif.lda", task=ct, subset=binaryclass.train.inds)
#	pred = predict(m, task=ct, subset=binaryclass.test.inds)
#	perf1 = performance(pred, losses="zero-one")
#  loss1 = loss(pred, function(x,y) x!=y)
#  checkEquals(perf1$measures["mmce"], mean(perf1$losses[, "zero-one"]), checkNames=FALSE)
#	perf2 = performance(pred, task=ct, measures=c("mmce", "fp", "fn", "costs"), losses=c("zero-one", "costs"))
#	checkEquals(perf2$measures["mmce"], mean(perf2$losses[, "zero-one"]), checkNames=FALSE)
#	checkEquals((perf2$measures["fp"] + perf2$measures["fn"]) / length(binaryclass.test.inds), perf2$measures["mmce"], checkNames=FALSE)
#	checkEquals(1*perf2$measures["fp"] + 2*perf2$measures["fn"], perf2$measures["costs"], checkNames=FALSE)
#	
#	m = train("regr.lm", task=regr.task, subset=regr.train.inds)
#	pred = predict(m, task=regr.task, subset=regr.test.inds)
#	perf3 = performance(pred, measures=c("sse", "mse", "medse", "sae", "mae", "medae"), losses=c("squared", "abs", "residual"))
#	checkEquals(perf3$measures["sse"], sum(perf3$losses[, "squared"]), checkNames=FALSE)
#	checkEquals(perf3$measures["sae"], sum(perf3$losses[, "abs"]), checkNames=FALSE)
#	checkEquals(perf3$measures["mse"], mean(perf3$losses[, "squared"]), checkNames=FALSE)
#	checkEquals(perf3$measures["mae"], mean(perf3$losses[, "abs"]), checkNames=FALSE)
#	checkEquals(perf3$measures["medse"], median(perf3$losses[, "squared"]), checkNames=FALSE)
#	checkEquals(perf3$measures["medae"], median(perf3$losses[, "abs"]), checkNames=FALSE)
#	checkEquals(perf3$losses[, "abs"], abs(perf3$losses[, "residual"]), checkNames=FALSE)
#	
#	
#	
#	
##	
##	res = makeResampleDesc("cv", iters=3)
##	
##	perf = performance(pred, measures=ms)
##	print(perf)
##	ms = c("mmce", bla=function(x, task) 1)
##	perf = performance(pred, measures=ms)
##	checkEquals(names(perf$measures), c("mmce", "bla"))
#}
