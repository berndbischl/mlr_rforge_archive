context("BaseWrapper")

test_that("BaseWrapper", {
  lrn1 = makeLearner("classif.rpart", minsplit=3)
  ps = makeParamSet(makeNumericLearnerParam("foo"))
  pv = list(foo=3)
  lrn2 = makeBaseWrapper(lrn1, par.set=ps, par.vals=pv)
  pars = getHyperPars(lrn2)
  print(pars)
})
