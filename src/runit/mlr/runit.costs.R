testCosts = function() {
  lrn = makeLearner("classif.rpart")
  rdesc = makeResampleDesc("Holdout")
  task = binaryclass.task
  task@desc@positive = "M"
  
  cc = 1 - diag(1, 2) 
  rownames(cc) = colnames(cc) = task@desc@class.levels
  ms = makeCostMeasure(costs=cc, task=task)
  r = resample(lrn, rdesc, task=task, measures=list(mmce, ms))
  checkEquals(r$aggr[1],r$aggr[2], checkNames=FALSE) 
  
  cc = matrix(0, 2, 2)
  rownames(cc) = colnames(cc) = task@desc@class.levels
  cc["R","M"] = 1
  ms = makeCostMeasure(costs=cc, task=task, aggregate=sum)
  r = resample(lrn, rdesc, task=task, measures=list(fp, ms))
  checkEquals(r$aggr[1], r$aggr[2], checkNames=FALSE) 
}