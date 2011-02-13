test.measures <- function() {
	
	ct = binaryclass.task
  
  mymeasure = makeMeasure(id="foo", minimize=TRUE,  
    fun=function(task, model, pred, extra.pars) {
      tt = pred
      1
    }
  )
	ms = c(mmce, acc, tp, fp, tn, fn, tpr, fpr, tnr, fnr, ppv, npv, mcc, f1, mymeasure)
	
  res = makeResampleDesc("cv", iters=3)
	
  m = train("classif.rpart", task=ct, subset=binaryclass.train.inds)
	pred = predict(m, task=ct, subset=binaryclass.test.inds)
  for (m in ms)
    perf = performance(pred, measure=m)
	
  r = resample("classif.rpart", ct, res, measures=ms)
	checkEquals(names(r$measures.test), c("iter", "mmce", "acc", "tp", "fp", "tn", "fn", "tpr", "fpr", "tnr", "fnr", "ppv", "npv", "mcc", "f1", "foo"))
}