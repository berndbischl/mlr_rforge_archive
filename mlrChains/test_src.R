library("devtools")
library("testthat")
library(mlbench)
data(BostonHousing)

load_all("skel")
source("skel/inst/tests/objects.R")


configureMlr(show.learner.output=TRUE)
  
lrn1 = makeLearner("classif.rpart", minsplit=10)
lrn2 = makePreprocWrapperPCA(lrn1)
#lrn3 = makePreprocWrapperRemoveOutliers(lrn3, ro.alpha=1)
  
m = train(lrn2, multiclass.task)

p = predict(m, multiclass.task)
print(66)


  
#  lrn1 = makeLearner("classif.rpart", minsplit=10)
 # lrn2 = makeFilterWrapper(lrn1)
#  lrn3 = makePreprocWrapperPCA(lrn2)
#  lrn4 = makePreprocWrapperRemoveOutliers(lrn3, ro.alpha=1)
#  m = train(lrn4, multiclass.task)
#  
#  expect_true(inherits(m, "PreprocModel"))
#  expect_true(inherits(m$learner.model, "PreprocModel"))
#  expect_true(inherits(m$learner.model$learner.model, "PreprocModel"))
#  
#  #p = predict(m, multiclass.task)
#  #perf = performance(p, mmce)
#  #expect_true(perf < 0.1)
