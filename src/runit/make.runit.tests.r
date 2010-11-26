
simple.test <- function(t.name, df, target, train.inds, old.predicts, parset=list()) {
	
	inds <- train.inds
	train <- df[inds,]
	test <- df[-inds,]
	
	wl = do.call("make.learner", c(t.name, parset))
	ct = make.task(data=df, target=target)
	cm = try(train(wl, ct, subset=inds))
	if(class(cm)[1] == "learner.failure"){
		checkTrue(class(old.predicts)=="try-error")
	}else{
		cp <- predict(cm, newdata=test)
		# to avoid issues with dropped levels in the class factor we only check the elemenst as charcters
		checkEquals(as.character(cp["response"]), as.character(old.predicts))
	}
}

simple.test.parsets <- function(t.name, df, target, train.inds, old.predicts.list, parset.list) {
	
	inds <- train.inds
	train <- df[inds,]
	test <- df[-inds,]
	
	for (i in 1:length(parset.list)) {
		parset <- parset.list[[i]]
		old.predicts <- old.predicts.list[[i]]
		simple.test(t.name, df, target, train.inds, old.predicts, parset)
	}
}


prob.test <- function(t.name, df, target, train.inds, old.probs, parset=list()) {
	
	inds <- train.inds
	train <- df[inds,]
	test <- df[-inds,]
	
	ct <- make.task(data=df, target=target)
	
	wl = do.call("make.learner", c(t.name, parset, predict.type="prob"))
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

prob.test.parsets <- function(t.name, df, target, train.inds, old.probs.list, parset.list) {
	
	inds <- train.inds
	train <- df[inds,]
	test <- df[-inds,]
	
	for (i in 1:length(parset.list)) {
		parset <- parset.list[[i]]
		old.probs <- old.probs.list[[i]]
		prob.test(t.name, df, target, train.inds, old.probs, parset)
	}
}


cv.test <- function(t.name, df, target, folds=2, parset=list(), tune.train, tune.predict = predict) {
	
	data = df
	formula = formula(paste(target, "~."))	
	
	tt <- function(formula, data, subset=1:nrow(data), ...) {
		pars <- list(formula=formula, data=data[subset, ])
		pars <- c(pars, parset)
		set.seed(debug.seed)
		capture.output(
			m <- do.call(tune.train, pars)
		)
		return(m)
	}
	
	tp <- function(model, newdata) {
		# todo insert precit.fct.pars
		
		set.seed(debug.seed)
		p <- tune.predict(model, newdata)
		
		return(p)
	}
	
	tr <- e1071::tune(method=tt, predict.func=tp, train.x=formula, data=data, tunecontrol = tune.control(cross = folds, best.model=FALSE))
	
	# todo bad code!!!!!
	if(class(tr)=="try-error"){
		warning("tune produced error!")
	} else {
		cv.instance <- e1071.cv.to.mlr.cv(tr)
		wl = do.call("make.learner", c(t.name, parset))
		lt = make.task(data=df, target=target)
		ms = resample(wl, lt, cv.instance)$measures
    if (is(lt, "classif.task")) { 
      checkEqualsNumeric(mean(ms["mmce"]), tr$performances[1,2])
      checkEqualsNumeric(sd  (ms["mmce"]), tr$performances[1,3])
    } else {
      checkEqualsNumeric(mean(ms["mse"]), tr$performances[1,2])
      checkEqualsNumeric(sd  (ms["mse"]), tr$performances[1,3])
    }
  }
}

cv.test.parsets <- function(t.name, df, target, folds=3, tune.train, tune.predict=predict, parset.list) {
	
	for (i in 1:length(parset.list)) {
		parset <- parset.list[[i]]
		cv.test(t.name, df, target, folds, parset, tune.train, tune.predict)
	}
}



bs.test <- function(t.name, df, target, iters=3, parset=list(), tune.train, tune.predict = predict) {
	
	data = df
	formula = formula(paste(target, "~."))	
	tr <- e1071::tune(method=tune.train, predict.func=tune.predict, train.x=formula, data=data, 
			tunecontrol = tune.control(sampling = "bootstrap", nboot = iters, boot.size=1))
	
	bs.instance <- e1071.bs.to.mlr.bs(tr)
	
	ct = make.task(data=df, target=target)
	ms = resample(t.name, ct, bs.instance)$measures
  
	if (is(ct, "classif.task")) { 
		checkEqualsNumeric(mean(ms["mmce"]), tr$performances[1,2])
		checkEqualsNumeric(sd  (ms["mmce"]), tr$performances[1,3])
	} else {
    checkEqualsNumeric(mean(ms["mse"]), tr$performances[1,2])
    checkEqualsNumeric(sd  (ms["mse"]), tr$performances[1,3])
  }
}







