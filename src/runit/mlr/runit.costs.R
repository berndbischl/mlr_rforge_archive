testCosts = function() {
  cc = diag(1, 2) 
  rownames(cc) = colnames(cc) = getClassLevels(binaryclass.task)
  ms = makeCostMeasure(cc, task=binaryclass.task)
  r = resample("classif.rpart", makeResampleDesc("Holdout"), task=binaryclass.task, measures=list(mmce, cc))
  checkEquals(r$aggr[1],r$aggr[2]) 
}