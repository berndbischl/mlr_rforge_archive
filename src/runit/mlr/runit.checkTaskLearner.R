test.checkTaskLearner <- function() {
  df = multiclass.df
  df[1,1] = NA
  task = makeClassifTask(data=df, target=multiclass.target)
  checkError(train("classif.lda", task), "missing values, but")
  checkError(train("regr.km", regr.task), "factor inputs, but")
  checkError(train("classif.gbm", multiclass.task), "multiclass-problem, but")
  checkError(train("classif.gbm", regr.task), "is regression, but")
  checkError(train("regr.gbm", multiclass.task), "is classification, but")
}