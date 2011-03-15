test.tune <- function() {
  cp = c(0.05, 0.9)
  minsplit = 1:3 
  ps1 = makeParameterSet(
    makeDiscreteParameter("cp", vals=cp), 
    makeDiscreteParameter("minsplit", vals=minsplit)
  )
	ctrl = makeTuneGridControl()
	folds = 3
	
	tr <- tune.rpart(formula=multiclass.formula, data=multiclass.df, cp=cp, minsplit=minsplit,
			tunecontrol = tune.control(sampling = "cross", cross = folds))  
	
	cv.instance <- e1071.cv.to.mlr.cv(tr)
	
	tr2 <- tune("classif.rpart", multiclass.task, cv.instance, par.set=ps1, control=ctrl)
  pp = as.data.frame(tr2@path)  
	# todo test scale with tune.e1071 and scaled grid!	
	for(i in 1:nrow(tr$performances)) {
		cp <- tr$performances[i,"cp"]
		ms <- tr$performances[i,"minsplit"]
		j <- which(pp$cp == cp & pp$minsplit == ms )
		checkEqualsNumeric(tr$performances[i,"error"], pp[j,"mmce.test.mean"])    
		checkEqualsNumeric(tr$performances[i,"dispersion"], pp[j,"mmce.test.sd"])    
	}
  # test printing
  print(tr2)
  
	# check multiple measures
	ms = c("acc", "mmce", "timefit") 
	tr2 = tune("classif.rpart", multiclass.task, cv.instance, par.set=ps1, control=ctrl)
  
	# tune wrapper
	res = makeResampleDesc("CV", iters=2)
  ps2 = makeParameterSet(
    makeDiscreteParameter("minsplit", vals=seq(3,10,2))
  )  
	wl = makeTuneWrapper("classif.rpart", resampling=res, par.set=ps2, control=ctrl)
	m = train(wl,  multiclass.task)
	# todo check opt. parameter is same as with tune
	
	#tune chain
	wl = makeLearner("classif.rpart", minsplit=10, cp=0.01, predict.type="prob")
	
  f1 = function(data, targetvar, args) {
    set.seed(1)
    v = sample(setdiff(colnames(data), targetvar), args$n)
    list(data=data[, c(v, targetvar), drop=FALSE], control=list(vars=v))
  }
  f2 = function(data, targetvar, args, control) {
    data[, control$vars, drop=FALSE]
	}
  ps3 = makeParameterSet(
    makeIntegerLearnerParameter("n", lower=1, upper=60)
  ) 
	wl = makePreprocWrapper(wl, train=f1, predict=f2, par.set=ps3, par.vals=list(n=3))
  
  ps4 = makeParameterSet(
    makeDiscreteParameter("minsplit", vals=c(3L,30L)),
    makeDiscreteParameter("n", vals=c(1L,60L))
  ) 
	tr = tune(wl, binaryclass.task, res, par.set=ps4, control=ctrl)
  checkTrue(!any(is.na(tr["y"])))
  checkEquals(tr@x$n, 60)
  
  checkException(tune("classif.rpart", multiclass.task, cv.instance, par.set=makeParameterSet(), control=ctrl))
}


test.tune.optim = function() {
  res = makeResampleDesc("CV", iters=2)
  ps1 = makeParameterSet(
    makeNumericParameter("C", trafo=function(x) 2^x), 
    makeNumericParameter("sigma", trafo=function(x) 2^x)
  )
  ps2 = makeParameterSet(
    makeNumericParameter("cp", lower=0.001, upper=1), 
    makeIntegerParameter("minsplit", lower=1)
  )
  ps3 = makeParameterSet(
    makeNumericParameter("cp", lower=0.001, upper=1), 
    makeDiscreteParameter("minsplit", vals=c(1,2))
  )
  
  # nelder mead with optim
  ctrl = makeTuneOptimControl(method="Nelder-Mead", start=c(1, 1), maxit=10)
  tr = tune("classif.ksvm", binaryclass.task, res, par.set=ps1, control=ctrl)
  ctrl = makeTuneOptimControl(method="Nelder-Mead", start=c(0.05, 5), maxit=10)
  checkException(tune("classif.rpart", binaryclass.task, res, par.set=ps2, control=ctrl))
  
  ctrl = makeTuneOptimControl(method="SANN", start=c(1, 1), maxit=10)
  tr = tune("classif.ksvm", binaryclass.task, res, par.set=ps1, control=ctrl)
  ctrl = makeTuneOptimControl(method="SANN", start=c(0.05, 5), maxit=10)
  checkException(tune("classif.rpart", binaryclass.task, res, par.set=ps2, control=ctrl))
  
  ctrl = makeTuneOptimControl(method="L-BFGS-B", start=c(1, 1), maxit=10)
  tr = tune("classif.ksvm", binaryclass.task, res, par.set=ps1, control=ctrl)
  ctrl = makeTuneOptimControl(method="L-BFGS-B", start=c(0.05, 5), maxit=10)
  tr = tune("classif.rpart", binaryclass.task, res, par.set=ps2, control=ctrl)
  
  checkException(tune("classif.rpart", multiclass.task, res, par.set=ps3, control=ctrl))
} 


test.tune.cmaes = function() {
  res = makeResampleDesc("CV", iters=2)
  ps1 = makeParameterSet(
    makeNumericParameter("cp", lower=0.001, upper=1), 
    makeIntegerParameter("minsplit", lower=1, upper=10)
  )
  ctrl1 = makeTuneCMAESControl(start=c(0.05, 5L), maxit=5)
  tr1 = tune("classif.rpart", multiclass.task, res, par.set=ps1, control=ctrl1)
  
  ps2 = makeParameterSet(
    makeNumericVectorParameter("cutoff", lower=0.0001, upper=1, dim=3, trafo=function(x) x / (1.1*sum(x))), 
    makeIntegerParameter("ntree", lower=100, upper=500) 
  )
  
  ctrl2 = makeTuneCMAESControl(start=c(1/3, 1/3, 1/3, 200L), maxit=5, sigma=2)
  tr2 = tune("classif.randomForest", multiclass.task, res, par.set=ps2, control=ctrl2)
  checkEquals(ncol(as.data.frame(tr2@path)), 4+2+2)
  checkTrue(is.numeric(tr2@y)) 
  checkEquals(length(tr2@y), 2) 
  checkTrue(is.list(tr2@x)) 
  checkEquals(length(tr2@x), 2) 
  
  ps3 = makeParameterSet(
    makeNumericParameter("cp", lower=0.001, upper=1), 
    makeDiscreteParameter("minsplit", vals=c(1,2))
  )
  checkException(tune("classif.rpart", multiclass.task, res, par.set=ps3, control=ctrl1))
} 



test.tune.spo = function() {
  res = makeResampleDesc("CV", iters=2)
  ps1 = makeParameterSet(
    makeNumericParameter("cp", lower=0.001, upper=1), 
    makeIntegerParameter("minsplit", lower=1, upper=10)
  )
  
  spo.ctrl = makeSPOControl(init.design.points=3, seq.loops=2)
  ctrl = makeTuneSPOControl(learner="regr.randomForest", spo.control=spo.ctrl)
  tr1 = tune("classif.rpart", multiclass.task, res, par.set=ps1, control=ctrl)
  checkEquals(length(as.list(tr1@path)), 25)
  checkEquals(dim(as.data.frame(tr1@path)), c(25, 2+2+2))
  
  ps2 = makeParameterSet(
    makeIntegerParameter("ntree", lower=100, upper=500),
    makeNumericVectorParameter("cutoff", dim=3, lower=0.001, upper=1, trafo=function(x) 0.9*x/sum(x)) 
  )
  tr2 = tune("classif.randomForest", multiclass.task, res, par.set=ps2, control=ctrl)
  checkEquals(length(as.list(tr2@path)), 25)
} 
  

