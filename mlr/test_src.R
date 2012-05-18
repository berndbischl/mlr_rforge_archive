library("devtools")
library("testthat")
library(mlbench)

load_all("skel")
source("skel/inst/tests/objects.R")

	wl1 = makeLearner("classif.rpart", minsplit=10)
	expect_equal(getHyperPars(wl1), list(minsplit=10)) 
	
	m = train(wl1, task=multiclass.task)
  expect_true(!is(m, "FailureModel"))
	expect_equal(getHyperPars(m$learner), list(minsplit=10)) 
	
  # check warnings
  setupErrorHandler(on.par.without.desc="warn")  
  expect_warning(makeLearner("classif.rpart", foo=1), "Setting par foo without")  
  setupErrorHandler(on.par.without.desc="quiet")
  expect_warning(makeLearner("classif.rpart", foo=1), FALSE)  

  #FIXME dec values!!!
#task = makeClassifTask(data=iris, target="Species")
#print(task)

#data(BostonHousing)
#task = makeRegrTask(data=BostonHousing, target="medv")
#print(task)

#lrn = makeLearner("classif.lda")
#m = train(lrn, task)
#print(m)
#p = predict(m, task=task)