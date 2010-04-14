test.tune <- function() {
	
	
	cp <- c(0.05, 0.9)
	minsplit <- c(1:3)
	ranges <- list(cp = cp, minsplit=minsplit)
	folds = 3
	
	tr <- tune.rpart(formula=multiclass.formula, data=multiclass.df, cp=cp, minsplit=minsplit,
			tunecontrol = tune.control(sampling = "cross", cross = folds))  
	
	cv.instance <- e1071.cv.to.mlr.cv(tr)
	
	tr2 <- tune("classif.rpart", multiclass.task, cv.instance, method="grid", control=grid.control(ranges=ranges))
	
	for(i in 1:nrow(tr$performances)) {
		cp <- tr$performances[i,"cp"]
		ms <- tr$performances[i,"minsplit"]
		j <- which(tr2$path$cp == cp & tr2$path$minsplit == ms )
		checkEqualsNumeric(tr$performances[i,"error"], tr2$path[j,"mean.mmce"])    
		checkEqualsNumeric(tr$performances[i,"dispersion"], tr2$path[j,"sd.mmce"])    
	}
	
	

	# check grid and scale
	control = grid.control(ranges=list(C=-1:1, sigma=-1:1))
	tune("classif.ksvm", multiclass.task, cv.instance, method="grid", control=control, scale=function(x)10^x)
	
	# check pattern search
	control = ps.control(start=list(C=0, sigma=0))
	tr3 <- tune("classif.ksvm", multiclass.task, cv.instance, method="pattern", control=control, scale=function(x)10^x)

	#complex test for tuning
	
	###########!!!!! check with tune.e1071
	
	r1 <- list(kernel="polydot", C=1:2)
	r2 <- list(kernel="rbfdot", sigma=1:2)
	r <- combine.ranges(r1, r2)
	control = grid.control(ranges=r)
	res = make.res.desc("cv", iters=2)
	inner = make.res.desc("cv", iters=2)
	
	wl = make.learner("classif.ksvm", type="spoc-svc")
	tr = tune(wl, multiclass.task, res, control=control)
	
	svm.tuner <- make.tune.wrapper(wl, resampling=inner, control=grid.control(ranges=r))
	bench.exp(svm.tuner, multiclass.task, resampling=res)

}

