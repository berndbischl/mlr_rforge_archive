
simple.test <- function(t.name, df, formula, train.inds, old.predicts, parset=list()) {
	
	inds <- train.inds
	train <- df[inds,]
	test <- df[-inds,]
	
	wl = do.call("make.learner", c(t.name, parset))
	if (is(wl, "wrapped.learner.classif")) {
		ct <- make.task(data=df, formula=formula)
	} else {
		ct <- make.task(data=df, formula=formula)
	}
	cm <- try(train(wl, ct, subset=inds))
	if(class(cm)[1] == "learner.failure"){
		checkTrue(class(old.predicts)=="try-error")
	}else{
		cp <- predict(cm, newdata=test)
		# to avoid issues with dropped levels in the class factor we only check the elemenst as charcters
		checkEquals(as.character(cp["response"]), as.character(old.predicts))
	}
}

simple.test.parsets <- function(t.name, df, formula, train.inds, old.predicts.list, parset.list) {
	
	inds <- train.inds
	train <- df[inds,]
	test <- df[-inds,]
	
	for (i in 1:length(parset.list)) {
		parset <- parset.list[[i]]
		old.predicts <- old.predicts.list[[i]]
		simple.test(t.name, df, formula, train.inds, old.predicts, parset)
	}
}


prob.test <- function(t.name, df, formula, train.inds, old.probs, parset=list()) {
	
	inds <- train.inds
	train <- df[inds,]
	test <- df[-inds,]
	
	ct <- make.task(data=df, formula=formula)
	
	wl = do.call("make.learner", c(t.name, parset))
	cm <- try(train(wl, ct, subset=inds, type="prob"))
	
	if(class(cm@learner.model)[1] == "learner.failure"){
		checkTrue(class(old.predicts)=="try-error")
	}else{
		cp <- predict(cm, newdata=test, type="prob")
		# dont need names for num vector, 2 classes
		if (is.numeric(old.probs))
			names(old.probs) = NULL
		else 
			old.probs = as.matrix(old.probs)
		
		p = cp["prob"]
		if (is.data.frame(p))
			p = as.matrix(p)
		# we change names a bit so dont check them
		colnames(p) = NULL
		colnames(old.probs) = NULL
		class(old.probs) = NULL
		checkEquals(p, old.probs)
	}
}

prob.test.parsets <- function(t.name, df, formula, train.inds, old.probs.list, parset.list) {
	
	inds <- train.inds
	train <- df[inds,]
	test <- df[-inds,]
	
	for (i in 1:length(parset.list)) {
		parset <- parset.list[[i]]
		old.probs <- old.probs.list[[i]]
		prob.test(t.name, df, formula, train.inds, old.probs, parset)
	}
}


cv.test <- function(t.name, df, formula, folds=2, parset=list(), tune.train, tune.predict = predict) {
	
	data = df
	
	tt <- function(formula, data, subset=1:nrow(data), ...) {
		pars <- list(formula=formula, data=data[subset, ])
		pars <- c(pars, parset)
		logger.debug("normal tune train call:", t.name, capture.output(formula), "with pars:")
		logger.debug(parset)
		logger.debug("on", length(subset), "examples:")
		logger.debug(subset)
		set.seed(debug.seed)
		capture.output(
			m <- do.call(tune.train, pars)
		)
		return(m)
	}
	
	tp <- function(model, newdata) {
		logger.debug("Normal tune predict:", t.name, "with pars:")
		# todo insert precit.fct.pars
		#logger.debug(tmp@predict.fct.pars)
		logger.debug("on", nrow(newdata), "examples:")
		logger.debug(rownames(newdata))
		
		set.seed(debug.seed)
		p <- tune.predict(model, newdata)
		
		logger.debug("Prediction:")
		logger.debug(p)
		return(p)
	}
	
	tr <- e1071::tune(method=tt, predict.func=tp, train.x=formula, data=data, tunecontrol = tune.control(cross = folds, best.model=FALSE))
	
	# todo bad code!!!!!
	if(class(tr)=="try-error"){
		warning("tune produced error!")
	} else {
		logger.debug("normal tune result:")
		logger.debug(tr$performances)
		cv.instance <- e1071.cv.to.mlr.cv(tr)
		wl = do.call("make.learner", c(t.name, parset))
		if (is(wl, "wrapped.learner.classif")) {
			lt <- make.task(data=df, formula=formula)
		} else {
			lt <- make.task(data=df, formula=formula)
		}
		cvr <- resample.fit(wl, lt, cv.instance)
		cva <- performance(cvr)
		if (is(lt, "classif.task")) { 
			checkEqualsNumeric(cva$agg["mean", "mmce"], tr$performances[1,2])
			checkEqualsNumeric(cva$agg["sd",   "mmce"], tr$performances[1,3])
		} else {
			checkEqualsNumeric(cva$agg["mean", "mse"], tr$performances[1,2])
			checkEqualsNumeric(cva$agg["sd",   "mse"], tr$performances[1,3])
		}
	}
}

cv.test.parsets <- function(t.name, df, formula, folds=3, tune.train, tune.predict=predict, parset.list) {
	
	for (i in 1:length(parset.list)) {
		parset <- parset.list[[i]]
		cv.test(t.name, df, formula, folds, parset, tune.train, tune.predict)
	}
}



bs.test <- function(t.name, df, formula, iters=3, parset=list(), tune.train, tune.predict = predict) {
	
	data = df
	
	tr <- e1071::tune(method=tune.train, predict.func=tune.predict, train.x=formula, data=data, 
			tunecontrol = tune.control(sampling = "bootstrap", nboot = iters, boot.size=1))
	
	bs.instance <- e1071.bs.to.mlr.bs(tr)
	
	ct <- make.task(data=df, formula=formula)
	
	bsr <- resample.fit(t.name, ct, bs.instance)
	
	bsp <- performance(bsr)
	
	if (is(ct, "classif.task")) { 
		checkEqualsNumeric(bsp$agg["mean", "mmce"], tr$performances[1,2])
		checkEqualsNumeric(bsp$agg["sd",   "mmce"], tr$performances[1,3])
	} else {
		checkEqualsNumeric(bsp$agg["mean", "mse"], tr$performances[1,2])
		checkEqualsNumeric(bsp$agg["sd",   "mse"], tr$performances[1,3])
	}
}







