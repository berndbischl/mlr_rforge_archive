library("devtools")
library("testthat")
library(mlbench)
data(BostonHousing)

load_all("skel")
source("skel/inst/tests/objects.R")


configureMlr(show.learner.output=TRUE)
  
#lrn1 = makeLearner("classif.lda")  
#lrn2 = makeFilterWrapper(lrn1, fw.method="random.forest.importance", fw.perc=1)
#m = train(lrn2, multiclass.task)
  

  
  f = function() as.factor(sample(1:2, 100, replace=TRUE))
  data = data.frame(x1=f(), x2=f(), y=f())
  task = makeClassifTask(data=data, target="y")
  lrn1 = makeLearner("classif.multinom")
  lrn2 = makePreprocWrapperRemoveOutliers(lrn1)
  m = train(lrn2, task)  
  p = predict(m, task)
  perf = performance(p, mmce)
  expect_true(!is.na(perf))

  