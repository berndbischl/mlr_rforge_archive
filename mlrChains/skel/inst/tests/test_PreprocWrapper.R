context("PreprocWrapper")

test_that("PreprocWrapper", {
  f1 = function(data, targetvar, args) {
    data[,2] = args$x * data[,2]
    return(list(data=data, control=list()))
  }
  f2 = function(data, targetvar, args, control) {
    data[,2] = args$x * data[,2]
    return(data)
  }  
  ps = makeParamSet(
    makeNumericLearnerParam(id="x"),
    makeNumericLearnerParam(id="y")
  )
  w1 = makeLearner("classif.rpart", minsplit=10)
  w2 = makePreprocWrapper(w1, train=f1, predict=f2, par.set=ps, par.vals=list(x=1,y=2))
  print(w2)
  
  expect_true(setequal(getHyperPars(w2), list(minsplit=10, x=1, y=2))) 
  expect_true(setequal(getHyperPars(w2, "train"), list(minsplit=10, x=1, y=2))) 
  expect_true(setequal(w2$par.vals, list(x=1, y=2))) 
  
  wl3 = setHyperPars(w2, minsplit=77, x=88)
  expect_true(setequal(getHyperPars(wl3), list(minsplit=77, x=88, y=2))) 
  expect_true(setequal(wl3$par.vals, list(x=88, y=2))) 
  
  m = train(w2, task=multiclass.task)
  capture.output(print(m))
  expect_true(setequal(getHyperPars(m$learner), list(minsplit=10, x=1, y=2))) 
})

