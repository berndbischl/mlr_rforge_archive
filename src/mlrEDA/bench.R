bench = function(data, target, measures) {
  # todo remove const feat
  # merge fact levels
  # adapt CV to size
  makeClassifTask(data=data, target=target)
  learners = c("classif.lda", "classif.qda", "classif.multinom", "classif.naiveBayes",
    "classif.rpart", "classif.randomForest")
  be = bench.exp(tasks=task, learners=learners, resampling=res, measures=measures)
}