test.rocr = function() {
  learners = list(
    makeLearner("classif.lda", predict.type="prob"),
    makeLearner("classif.rpart", predict.type="prob")
  )    
  
  be = bench.exp(learners, binaryclass.task, resampling=makeResampleDesc("Holdout"))
  ROCR.plot.task(be)
}