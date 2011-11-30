# todo: test tuning of chain, maybe in mlrChains
test.tune = function() {
  cp = c(0.05, 0.9)
  minsplit = 1:3 
  ps1 = makeParamSet(
    makeDiscreteParam("cp", values=cp), 
    makeDiscreteParam("minsplit", values=minsplit)
  )
	ctrl = makeTuneControlGrid()
	folds = 3
	
	tr = tune.rpart(formula=multiclass.formula, data=multiclass.df, cp=cp, minsplit=minsplit,
			tunecontrol = tune.control(sampling = "cross", cross = folds))  
	
	cv.instance = e1071.cv.to.mlr.cv(tr)
	m1 = setAggregation(mmce, test.mean)
  m2 = setAggregation(mmce, test.sd)
	tr2 = tune("classif.rpart", multiclass.task, cv.instance, par.set=ps1, control=ctrl, measures=list(m1, m2))
  pp = as.data.frame(tr2@path)  
	# todo test scale with tune.e1071 and scaled grid!	
	for(i in 1:nrow(tr$performances)) {
		cp = tr$performances[i,"cp"]
		ms = tr$performances[i,"minsplit"]
		j = which(pp$cp == cp & pp$minsplit == ms )
		checkEqualsNumeric(tr$performances[i,"error"], pp[j,"mmce.test.mean"])    
		checkEqualsNumeric(tr$performances[i,"dispersion"], pp[j,"mmce.test.sd"])    
	}
  # test printing
  print(tr2)
  
	# check multiple measures
	ms = c("acc", "mmce", "timefit") 
	tr2 = tune("classif.rpart", multiclass.task, cv.instance, par.set=ps1, control=ctrl)
  
  checkException(tune("classif.rpart", multiclass.task, cv.instance, par.set=makeParamSet(), control=ctrl))
}


test.tune.optim = function() {
  res = makeResampleDesc("CV", iters=2)
  ps1 = makeParamSet(
    makeNumericParam("C", trafo=function(x) 2^x), 
    makeNumericParam("sigma", trafo=function(x) 2^x)
  )
  ps2 = makeParamSet(
    makeNumericParam("cp", lower=0.001, upper=1), 
    makeIntegerParam("minsplit", lower=1)
  )
  ps3 = makeParamSet(
    makeNumericParam("cp", lower=0.001, upper=1), 
    makeDiscreteParam("minsplit", values=c(1,2))
  )
  
  # nelder mead with optim
  ctrl = makeTuneControlOptim(method="Nelder-Mead", start=c(0, 0), maxit=10)
  tr = tune("classif.ksvm", binaryclass.task, res, par.set=ps1, control=ctrl)
  ctrl = makeTuneControlOptim(method="Nelder-Mead", start=c(0.05, 5), maxit=10)
  checkException(tune("classif.rpart", binaryclass.task, res, par.set=ps2, control=ctrl))
  
  ctrl = makeTuneControlOptim(method="SANN", start=c(0, 0), maxit=10)
  tr = tune("classif.ksvm", binaryclass.task, res, par.set=ps1, control=ctrl)
  ctrl = makeTuneControlOptim(method="SANN", start=c(0.05, 5), maxit=10)
  checkException(tune("classif.rpart", binaryclass.task, res, par.set=ps2, control=ctrl))
  
  ctrl = makeTuneControlOptim(method="L-BFGS-B", start=c(0, 0), maxit=10)
  tr = tune("classif.ksvm", binaryclass.task, res, par.set=ps1, control=ctrl)
  ctrl = makeTuneControlOptim(method="L-BFGS-B", start=c(0.05, 5), maxit=10)
  tr = tune("classif.rpart", binaryclass.task, res, par.set=ps2, control=ctrl)
  
  checkException(tune("classif.rpart", multiclass.task, res, par.set=ps3, control=ctrl))
} 


test.tune.cmaes = function() {
  res = makeResampleDesc("CV", iters=2)
  ps1 = makeParamSet(
    makeNumericParam("cp", lower=0.001, upper=1), 
    makeIntegerParam("minsplit", lower=1, upper=10)
  )
  ctrl1 = makeTuneControlCMAES(start=c(0.05, 5L), maxit=5)
  tr1 = tune("classif.rpart", multiclass.task, res, par.set=ps1, control=ctrl1)
  
  ps2 = makeParamSet(
    makeNumericVectorParam("cutoff", lower=0.0001, upper=1, length=3, trafo=function(x) x / (1.1*sum(x))), 
    makeIntegerParam("ntree", lower=100, upper=500) 
  )
  
  ctrl2 = makeTuneControlCMAES(start=c(1/3, 1/3, 1/3, 200L), maxit=5, sigma=2)
  tr2 = tune("classif.randomForest", multiclass.task, res, par.set=ps2, control=ctrl2)
  checkEquals(ncol(as.data.frame(tr2@path)), 4+1+2)
  checkTrue(is.numeric(tr2@y)) 
  checkEquals(length(tr2@y), 1) 
  checkTrue(is.list(tr2@x)) 
  checkEquals(length(tr2@x), 2) 
  
  ps3 = makeParamSet(
    makeNumericParam("cp", lower=0.001, upper=1), 
    makeDiscreteParam("minsplit", values=c(1,2))
  )
  checkException(tune("classif.rpart", multiclass.task, res, par.set=ps3, control=ctrl1))
} 



test.tune.mbo = function() {
  res = makeResampleDesc("CV", iters=2)
  ps1 = makeParamSet(
    makeNumericParam("cp", lower=0.001, upper=1), 
    makeIntegerParam("minsplit", lower=1, upper=10)
  )
  
  mbo.ctrl = makeMboControl(init.design.points=3, seq.loops=2)
  ctrl = makeTuneControlMbo(learner="regr.randomForest", spo.control=spo.ctrl)
  tr1 = tune("classif.rpart", multiclass.task, res, par.set=ps1, control=ctrl)
  checkEquals(getOptPathLength(tr1@path), 5)
  checkEquals(dim(as.data.frame(tr1@path)), c(5, 2+1+2))
  
  ps2 = makeParamSet(
    makeIntegerParam("ntree", lower=100, upper=500),
    makeNumericVectorParam("cutoff", length=3, lower=0.001, upper=1, trafo=function(x) 0.9*x/sum(x)) 
  )
  tr2 = tune("classif.randomForest", multiclass.task, res, par.set=ps2, control=ctrl)
  checkEquals(getOptPathLength(tr2@path), 5)
} 
  

