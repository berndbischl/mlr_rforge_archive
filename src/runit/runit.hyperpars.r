
test.hyperpars <- function() {
	
	wl1 = make.learner("classif.rpart", minsplit=10, predict.type="prob", predict.threshold=0.3)
	checkEquals(wl1["hyper.pars"], list(minsplit=10, predict.threshold=0.3)) 
	checkEquals(wl1["hyper.names"], c("minsplit", "predict.threshold")) 
	checkEquals(wl1["hyper.types"], c(minsplit="train", predict.threshold="postproc")) 
	
	m = train(wl1, task=multiclass.task, parset=list(cp=0.2))
	checkEquals(m["fail"], NULL) 
	checkEquals(m["hyper.pars"], list(minsplit=10, predict.threshold=0.3, cp=0.2)) 
	checkEquals(m["hyper.names"], c("minsplit", "predict.threshold", "cp")) 
	checkEquals(m["hyper.types"], c(minsplit="train", predict.threshold="postproc", cp="train")) 
	checkEquals(m["hyper.pars", type="train"], list(minsplit=10, cp=0.2)) 
	checkEquals(m["hyper.pars", type="postproc"], list(predict.threshold=0.3)) 
	
	fun = function(data, x, y) {
		data[,2] = x*data[,2]
		return(data)
	}
	wl2 = make.preproc.wrapper(wl1, fun=fun, x=1, y=2)
	
	checkTrue(setequal(wl2["hyper.pars"], list(minsplit=10, predict.threshold=0.3, x=1, y=2))) 
	checkTrue(setequal(wl2["hyper.names"], c("minsplit", "predict.threshold", "x", "y"))) 
	checkTrue(setequal(wl2["hyper.types"], c(minsplit="train", predict.threshold="postproc", x="preproc", y="preproc"))) 
	
	m = train(wl2, task=multiclass.task, parset=list(cp=0.2))
	checkTrue(setequal(m["hyper.pars"], list(cp=0.2, minsplit=10, predict.threshold=0.3, x=1, y=2))) 
	checkTrue(setequal(m["hyper.names"], c("cp", "minsplit", "predict.threshold", "x", "y"))) 
	checkTrue(setequal(m["hyper.types"], c(cp="train", minsplit="train", predict.threshold="postproc", x="preproc", y="preproc"))) 
	checkTrue(setequal(m["hyper.pars", type="train"], list(minsplit=10, cp=0.2))) 
	checkTrue(setequal(m["hyper.pars", type="postproc"], list(predict.threshold=0.3))) 
	checkTrue(setequal(m["hyper.pars", type="preproc"], list(x=1, y=2))) 
}

