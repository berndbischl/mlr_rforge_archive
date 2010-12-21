
test.hyperpars <- function() {
	
	
	wl1 = make.learner("classif.rpart", minsplit=10)
	checkEquals(wl1["par.vals"], list(minsplit=10)) 
	
	m = train(wl1, task=multiclass.task)
	checkEquals(m["fail"], NULL) 
	checkEquals(m["learner"]["par.vals"], list(minsplit=10)) 
	
	fun = function(data, x, y) {
		data[,2] = x*data[,2]
		return(data)
	}
	wl2 = make.preproc.wrapper(wl1, fun=fun, x=1, y=2)
	
	checkTrue(setequal(wl2["par.vals"], list(minsplit=10, x=1, y=2))) 
	checkTrue(setequal(wl2["par.vals", par.when="train"], list(minsplit=10, x=1, y=2))) 
	checkTrue(setequal(wl2["par.vals", par.top.wrapper.only=TRUE], list(x=1, y=2))) 

	wl3 = set.hyper.pars(wl2, minsplit=77, x=88)
	checkTrue(setequal(wl3["par.vals"], list(minsplit=77, x=88, y=2))) 
	checkTrue(setequal(wl3["par.vals", par.when="train"], list(minsplit=77, x=88, y=2))) 
	checkTrue(setequal(wl3["par.vals", par.top.wrapper.only=TRUE], list(x=88, y=2))) 
	
	m = train(wl2, task=multiclass.task)
	checkTrue(setequal(m["learner"]["par.vals"], list(minsplit=10, x=1, y=2))) 
  
  # check warnings
  checkWarning(make.learner("classif.rpart", foo=1), "Setting par foo without")  
  errorhandler.setup(on.par.without.desc="quiet")
  checkWarning(make.learner("classif.rpart", foo=1), FALSE)  
  errorhandler.setup()
  
}

