test.tune <- function() {
	
	
	cp <- c(0.05, 0.9)
	minsplit <- c(1:3)
	ranges = list(cp = cp, minsplit=minsplit)
	ctrl = grid.control(ranges=ranges)
	folds = 3
	
	tr <- tune.rpart(formula=multiclass.formula, data=multiclass.df, cp=cp, minsplit=minsplit,
			tunecontrol = tune.control(sampling = "cross", cross = folds))  
	
	cv.instance <- e1071.cv.to.mlr.cv(tr)
	
	tr2 <- tune("classif.rpart", multiclass.task, cv.instance, method="grid", control=ctrl, model=T)
	
	# todo test scale with tune.e1071 and scaled grid!
	
	for(i in 1:nrow(tr$performances)) {
		cp <- tr$performances[i,"cp"]
		ms <- tr$performances[i,"minsplit"]
		pp = tr2["path", as.data.frame=T]
		j <- which(pp$cp == cp & pp$minsplit == ms )
		checkEqualsNumeric(tr$performances[i,"error"], pp[j,"mean.mmce"])    
		checkEqualsNumeric(tr$performances[i,"dispersion"], pp[j,"sd.mmce"])    
	}
	
	# check multiple measures
	ms = c("acc", "mmce", "time.train") 
	tr2 = tune("classif.rpart", multiclass.task, cv.instance, control=ctrl)
	

	# check grid and scale
	control = grid.control(ranges=list(C=-1:1, sigma=-1:1), scale=function(x)10^x)
	tune("classif.ksvm", multiclass.task, cv.instance, method="grid", control=control)
	
	# tune wrapper
	res = make.res.desc("cv", iters=2)
	ranges = list(minsplit=seq(3,10,2))
	wl = make.tune.wrapper("classif.rpart", resampling=res, control=grid.control(ranges=ranges))
	m = train(wl,  multiclass.task)
	# todo check opt. parameter is same as with tune
	
	
#	# check pattern search
#	control = ps.control(start=list(C=0, sigma=0), scale=function(x)10^x)
#	tr3 <- tune("classif.ksvm", multiclass.task, resampling=cv.instance, method="pattern", control=control)


}

