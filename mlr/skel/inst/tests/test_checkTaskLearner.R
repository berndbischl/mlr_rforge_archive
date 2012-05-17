context("checkTaskLearner")

test_that("checkTaskLearner", {
  df = multiclass.df
  df[1,1] = NA
  task = makeClassifTask(data=df, target=multiclass.target)
  expect_error(train("classif.lda", task), "missing values, but")
  expect_error(train("regr.km", regr.task), "factor inputs, but")
  expect_error(train("classif.gbm", multiclass.task), "multiclass-problem, but")
  expect_error(train("classif.gbm", regr.task), "is regression, but")
  expect_error(train("regr.gbm", multiclass.task), "is classification, but")
})