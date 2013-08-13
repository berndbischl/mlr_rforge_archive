context("simple optims")

test_that("simple optims", {
  if(isExpensiveExampleOk()) {
    fit = function(x) {
      -(x$x1 - is.null(x$x2) * 30 + 5 * (x$x3 == "a") + runif(1, 1, 5))
    }

    easy = makeParamSet(
      makeNumericParam("x1", lower = 0, upper = 100),
      makeNumericParam("x2", lower = 0, upper = 100, requires = quote(x1 > 50)),
      makeDiscreteParam("x3", values = c("a", "blasdfkjaslkdjaslkdjflkjsdafj")))

    surrogate = makeLearner("regr.rpart")
    opt = mbo(fit, easy, learner = surrogate, control = makeMBOControl(random.n.points=10))
    expect_true(opt$x$x1 > 85 && opt$x$x2 < 50 && opt$x$x3 == "a")
  }
})
