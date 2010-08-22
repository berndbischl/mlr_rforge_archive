
test.hyperpars <- function() {
	
	
	wl1 = make.learner("classif.rpart", minsplit=10)
	checkEquals(wl1["par.vals"], list(minsplit=10)) 
	checkEquals(wl1["par.vals.name"], c("minsplit")) 
	
	m = train(wl1, task=multiclass.task, par.vals=list(minsplit=5, cp=0.2))
	checkEquals(m["fail"], NULL) 
	checkEquals(m["learner"]["par.vals"], list(minsplit=5, cp=0.2)) 
	
	fun = function(data, x, y) {
		data[,2] = x*data[,2]
		return(data)
	}
	wl2 = make.preproc.wrapper(wl1, fun=fun, x=1, y=2)
	
	checkTrue(setequal(wl2["par.vals"], list(minsplit=10, x=1, y=2))) 
	checkTrue(setequal(wl2["par.vals.name"], c("minsplit", "x", "y"))) 
	checkTrue(setequal(wl2["par.vals", par.when="train"], list(minsplit=10, x=1, y=2))) 
	checkTrue(setequal(wl2["par.vals.name", par.when="train"], c("minsplit", "x", "y"))) 
	checkTrue(setequal(wl2["par.vals", par.top.wrapper.only=TRUE], list(x=1, y=2))) 
	checkTrue(setequal(wl2["par.vals.name", par.top.wrapper.only=TRUE], c("x", "y"))) 

	wl3 = set.hyper.pars(wl2, minsplit=77, x=88)
	checkTrue(setequal(wl3["par.vals"], list(minsplit=77, x=88, y=2))) 
	checkTrue(setequal(wl3["par.vals.name"], c("minsplit", "x", "y"))) 
	checkTrue(setequal(wl3["par.vals", par.when="train"], list(minsplit=77, x=88, y=2))) 
	checkTrue(setequal(wl3["par.vals.name", par.when="train"], c("minsplit", "x", "y"))) 
	checkTrue(setequal(wl3["par.vals", par.top.wrapper.only=TRUE], list(x=88, y=2))) 
	checkTrue(setequal(wl3["par.vals.name", par.top.wrapper.only=TRUE], c("x", "y"))) 
	
	m = train(wl2, task=multiclass.task, par.vals=list(minsplit=5, cp=0.2))
	checkTrue(setequal(m["learner"]["par.vals"], list(cp=0.2, minsplit=5, x=1, y=2))) 
	checkTrue(setequal(m["learner"]["par.vals.name"], c("cp", "minsplit", "x", "y"))) 
}

