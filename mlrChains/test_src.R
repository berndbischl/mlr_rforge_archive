library("devtools")
library("testthat")
library(mlbench)
data(BostonHousing)

load_all("skel")

configureMlr(show.learner.output=FALSE)
  
  # check that predict.type is taken from base learner
  lrn1 = makeLearner("classif.ksvm", predict.type="prob")
  lrn2 = makeTuneWrapper(lrn1, resampling=makeResampleDesc("Holdout"), 
    par.set=ps1, control=makeTuneControlGrid())
  expect_equal(lrn2$predict.type, "prob")
  r = resample(lrn2, binaryclass.task, makeResampleDesc("Holdout"), measures=auc, show.info=TRUE)
  expect_true(!is.na(r$aggr["auc.test.mean"]))