test.tune <- function() {
	
	
	cp <- c(0.05, 0.9)
	minsplit <- c(1:3)
	ranges <- list(cp = cp, minsplit=minsplit)
	folds = 3
	
	tr <- tune.rpart(formula=multiclass.formula, data=multiclass.df, cp=cp, minsplit=minsplit,
			tunecontrol = tune.control(sampling = "cross", cross = folds))  
	
	cv.instance <- e1071.cv.to.mlr.cv(tr)
	
	tr2 <- tune("rpart.classif", multiclass.task, cv.instance, method="grid", control=grid.control(ranges=ranges))
	
	for(i in 1:nrow(tr$performances)) {
		cp <- tr$performances[i,"cp"]
		ms <- tr$performances[i,"minsplit"]
		j <- which(tr2$all.perfs$cp == cp & tr2$all.perfs$minsplit == ms )
		checkEqualsNumeric(tr$performances[i,"error"], tr2$all.perfs[j,"mean"])    
		checkEqualsNumeric(tr$performances[i,"dispersion"], tr2$all.perfs[j,"sd"])    
	}
	
	

	# check grid and scale
	control = grid.control(ranges=list(C=-1:1, sigma=-1:1))
	tune("kernlab.svm.classif", multiclass.task, cv.instance, method="grid", control=control, scale=function(x)10^x)
	
	# check pattern search
	control = ps.control(start=list(C=0, sigma=0))
	tr3 <- tune("kernlab.svm.classif", multiclass.task, cv.instance, method="pattern", control=control, scale=function(x)10^x)

	#complex test for tuning
	
	###########!!!!! check with tune.e1071
	
	r1 <- list(kernel="polydot", C=1:2)
	r2 <- list(kernel="rbfdot", sigma=1:2)
	r <- combine.ranges(r1, r2)
	control = grid.control(ranges=r)
	res = make.res.desc("cv", iters=2)
	inner = make.res.desc("cv", iters=2)
	
	tr = tune("kernlab.svm.classif", multiclass.task, res, control=control, fixed=list(type="spoc-svc"))
	
	wl = make.learner("kernlab.svm.classif")			
	set.train.par(wl, type="spoc-svc")
	tr = tune(wl, multiclass.task, res, control=control)
	
	svm.tuner <- make.tune.wrapper("kernlab.svm.classif", resampling=inner, control=grid.control(ranges=r), fixed=list(type="spoc-svc"))
	bench.exp(svm.tuner, multiclass.task, resampling=res)
	
	svm.tuner <- make.tune.wrapper(wl, resampling=inner, control=grid.control(ranges=r))
	bench.exp(svm.tuner, multiclass.task, resampling=res)
	

}

