context("BaseWrapper")

if (interactive()) {
test_that("BaseWrapper", {
  lrn1 = makeLearner("classif.rpart", minsplit=2)
  ps = makeParamSet(makeNumericLearnerParam("foo"))
  pv = list(foo=3)
  lrn2 = makeBaseWrapper(lrn1, par.set=ps, par.vals=pv, cl="mywrapper")
  expect_equal(getHyperPars(lrn2), list(minsplit=2, foo=3))
  lrn2 = setHyperPars(lrn2, minsplit=11)
  expect_equal(getHyperPars(lrn2), list(minsplit=11, foo=3))
  lrn2 = setHyperPars(lrn2, foo=12)
  expect_equal(getHyperPars(lrn2), list(minsplit=11, foo=12))
})
}