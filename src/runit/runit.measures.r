test.measures <- function() {
	
	ct = binaryclass.task
  
  mymeasure = make.measure(id="foo", minimize=TRUE,  
    fun=function(task, model, pred.test, pred.train, pars) {
      tt = pred.test
      1
    }
  )
	ms = c(mmce, acc, tp, fp, tn, fn, tpr, fpr, tnr, fnr, ppv, npv, mcc, f1, mymeasure)
	
  res = make.res.desc("cv", iters=3)
	
  m = train("classif.rpart", task=ct, subset=binaryclass.train.inds)
	pred = predict(m, task=ct, subset=binaryclass.test.inds)
  for (m in ms)
    perf = performance(pred, measure=m)
	
  r = resample("classif.rpart", ct, res, measures=ms)
	checkEquals(names(r$measures), c("mmce", "acc", "tp", "fp", "tn", "fn", "tpr", "fpr", "tnr", "fnr", "ppv", "npv", "mcc", "f1", "foo"))
}