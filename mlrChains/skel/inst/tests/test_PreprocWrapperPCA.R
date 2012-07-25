context("PreprocWrapperPCA")

test_that("PreprocWrapperPCA", {
  lrn1 = makeLearner("classif.rpart", minsplit=10)
  lrn2 = makePreprocWrapperPCA(lrn1)
  m = train(lrn2, multiclass.task)  
  p = predict(m, multiclass.task)
  perf = performance(p, mmce)
  expect_true(perf < 0.1)

})

