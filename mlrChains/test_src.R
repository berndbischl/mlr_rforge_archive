library("devtools")
library("testthat")
library(mlbench)
data(BostonHousing)

load_all("skel")

configureMlr(show.learner.output=FALSE)
  
 outer = makeResampleDesc("Holdout")
  inner = makeResampleDesc("CV", iters=2)

  ps1 = makeParamSet(makeDiscreteParam(id="C", values=c(1, 0.000001)))


  # check that predict.type is taken from base learner
  lrn1 = makeLearner("classif.ksvm", predict.type="prob")
  lrn2 = makeTuneWrapper(lrn1, resampling=makeResampleDesc("Holdout"), par.set=ps1, control=makeTuneControlGrid())
  expect_equal(lrn2$predict.type, "prob")
  r = resample(lrn2, binaryclass.task, makeResampleDesc("Holdout"), measures=auc)
  expect_true(!is.na(r$aggr["auc.test.mean"]))