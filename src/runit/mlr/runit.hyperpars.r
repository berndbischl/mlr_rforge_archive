test.hyperpars <- function() {
	wl1 = makeLearner("classif.rpart", minsplit=10)
	checkEquals(getParameterValues(wl1), list(minsplit=10)) 
	
	m = train(wl1, task=multiclass.task)
  checkTrue(!is(m, "FailureModel"))
	checkEquals(getParameterValues(m@learner), list(minsplit=10)) 
	
  # check warnings
  errorhandler.setup(on.par.without.desc="warn")  
  checkWarning(makeLearner("classif.rpart", foo=1), "Setting par foo without")  
  errorhandler.setup(on.par.without.desc="quiet")
  checkWarning(makeLearner("classif.rpart", foo=1), FALSE)  
  errorhandler.setup()
  
}

