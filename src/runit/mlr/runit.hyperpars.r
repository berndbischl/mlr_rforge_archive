test.hyperpars <- function() {
	wl1 = makeLearner("classif.rpart", minsplit=10)
	checkEquals(getHyperPars(wl1), list(minsplit=10)) 
	
	m = train(wl1, task=multiclass.task)
  checkTrue(!is(m, "FailureModel"))
	checkEquals(getHyperPars(m@learner), list(minsplit=10)) 
	
  # check warnings
  setupErrorHandler(on.par.without.desc="warn")  
  checkWarning(makeLearner("classif.rpart", foo=1), "Setting par foo without")  
  setupErrorHandler(on.par.without.desc="quiet")
  checkWarning(makeLearner("classif.rpart", foo=1), FALSE)  
  setupErrorHandler()
  
}

