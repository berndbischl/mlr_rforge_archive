test.hyperpars <- function() {
	wl1 = makeLearner("classif.rpart", minsplit=10)
	checkEquals(wl1["par.vals"], list(minsplit=10)) 
	
	m = train(wl1, task=multiclass.task)
  checkTrue(!is(m, "FailureModel"))
	checkEquals(m@learner["par.vals"], list(minsplit=10)) 
	
	f1 = function(data, targetvar, args) {
		data[,2] = args$x * data[,2]
		return(list(data=data, control=list()))
	}
  f2 = function(data, targetvar, args, control) {
    data[,2] = args$x * data[,2]
    return(data)
  }  
  ps = makeParameterSet(
    makeNumericLearnerParameter(id="x"),
    makeNumericLearnerParameter(id="y")
  )
	wl2 = makePreprocWrapper(wl1, train=f1, predict=f2, par.set=ps, par.vals=list(x=1,y=2))
	
	checkTrue(setequal(wl2["par.vals"], list(minsplit=10, x=1, y=2))) 
	checkTrue(setequal(wl2["par.train"], list(minsplit=10, x=1, y=2))) 
  checkTrue(setequal(wl2["par.vals", head=T], list(x=1, y=2))) 
  
	wl3 = setHyperPars(wl2, minsplit=77, x=88)
	checkTrue(setequal(wl3["par.vals"], list(minsplit=77, x=88, y=2))) 
	checkTrue(setequal(wl3["par.train"], list(minsplit=77, x=88, y=2))) 
	checkTrue(setequal(wl3["par.vals", head=T], list(x=88, y=2))) 
	
	m = train(wl2, task=multiclass.task)
	checkTrue(setequal(m@learner["par.vals"], list(minsplit=10, x=1, y=2))) 
  
  # check warnings
  errorhandler.setup(on.par.without.desc="warn")  
  checkWarning(makeLearner("classif.rpart", foo=1), "Setting par foo without")  
  errorhandler.setup(on.par.without.desc="quiet")
  checkWarning(makeLearner("classif.rpart", foo=1), FALSE)  
  errorhandler.setup()
  
}

