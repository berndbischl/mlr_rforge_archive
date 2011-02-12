test.thresh.wrapper <- function() {
  cl = multiclass.task["class.levels"]
  v = makeLearner("classif.lda", predict.type="prob")
  v = makeProbthWrapper(v, classes=cl)
  pv = as.list(rep(1,3))
  names(pv) = paste("probth", cl, sep=".")
  
  res = make.res.desc("cv", iters=2)
  for (i in 1:length(cl)) {
    pv2 = pv; pv2[[i]] = 0
    w = setHyperPars(v, par.vals=pv2)
    r = resample(w, multiclass.task, res)
    checkTrue(all(r$pred["response"] == cl[i]))
  }
}
