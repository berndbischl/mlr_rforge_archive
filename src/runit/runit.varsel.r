test.varsel <- function() {
	inner = make.res.desc("cv", iter=2)

	# check all methods
	
	ctrl = sequential.control(method="sfs", alpha=0.01, path=TRUE)
	vr = varsel("classif.lda", task=multiclass.task, resampling=inner, control=ctrl)
	checkTrue(length(as.list(vr@path)) > 1) 
	
	ctrl = sequential.control(method="sbs", beta=0.01, path=TRUE)
	vr = varsel("classif.lda", task=multiclass.task, resampling=inner, control=ctrl)
	checkTrue(length(as.list(vr@path)) > 1) 

	ctrl = sequential.control(method="sffs", alpha=0.01, path=TRUE)
	vr = varsel("classif.lda", task=multiclass.task, resampling=inner, control=ctrl)
	checkTrue(length(as.list(vr@path)) > 1) 
	
	ctrl = sequential.control(method="sfbs", beta=0.01, path=TRUE)
	vr = varsel("classif.lda", task=multiclass.task, resampling=inner, control=ctrl)
	checkTrue(length(as.list(vr@path)) > 1) 
  
  ctrl = exhaustive.control(max.vars=2)
  vr = varsel("classif.lda", task=multiclass.task, resampling=inner, control=ctrl)
  checkEquals(length(as.list(vr@path)), 11) 
  checkEquals(vr@x, 2) 
  
	# check maxit
	ctrl = randomvarsel.control(maxit=4, path=TRUE)
	vr = varsel("classif.lda", task=multiclass.task, resampling=inner, control=ctrl)
	checkEquals(length(as.list(vr@path)), 4) 
	
	# check max.vars
	ctrl = sequential.control(alpha=0, max.vars=1, method="sfs", path=TRUE)
	vr = varsel("classif.lda", task=binaryclass.task, resampling=inner, control=ctrl)
	checkEquals(length(vr@x), 1) 

	ctrl = sequential.control(beta=1, max.vars=58, method="sbs", path=TRUE)
	vr = varsel("classif.lda", task=binaryclass.task, resampling=inner, control=ctrl)
	checkEquals(length(vr@x), 58) 
	
	# check empty model
	ctrl = sequential.control(method="sfs", alpha=10)
	vr = varsel("classif.lda", task=multiclass.task, resampling=inner, control=ctrl, model=TRUE)
	checkEquals(vr@x, character(0)) 
	checkTrue(is(vr["model"], "wrapped.model")) 
	checkTrue(is(vr["model"]["learner.model"], "novars")) 
	
	wl = makeVarselWrapper("classif.lda", resampling=inner, control=ctrl)
	outer = make.res.desc("cv", iter=2)
	be = bench.exp(wl, task=multiclass.task, resampling=outer)
}

