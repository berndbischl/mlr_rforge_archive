test.varsel <- function() {
	inner = make.res.desc("cv", iter=2)

	# check all methods
	
	ctrl = sequential.control(method="sfs", alpha=0.01)
	vr = varsel("classif.lda", task=multiclass.task, resampling=inner, control=ctrl, path=T)
	checkTrue(length(vr["path"]) > 1) 
	
	ctrl = sequential.control(method="sbs", beta=0.01)
	vr = varsel("classif.lda", task=multiclass.task, resampling=inner, control=ctrl, path=T)
	checkTrue(length(vr["path"]) > 1) 

	ctrl = sequential.control(method="sffs", alpha=0.01)
	vr = varsel("classif.lda", task=multiclass.task, resampling=inner, control=ctrl, path=T)
	checkTrue(length(vr["path"]) > 1) 
	
	ctrl = sequential.control(method="sfbs", beta=0.01)
	vr = varsel("classif.lda", task=multiclass.task, resampling=inner, control=ctrl, path=T)
	checkTrue(length(vr["path"]) > 1) 
	
	ctrl = randomvarsel.control(maxit=4)
	vr = varsel("classif.lda", task=multiclass.task, resampling=inner, control=ctrl, path=T)
	checkEquals(length(vr["path"]), 4) 
	
	
	# check empty model
	ctrl = sequential.control(method="sfs", alpha=10)
	vr = varsel("classif.lda", task=multiclass.task, resampling=inner, control=ctrl, model=T)
	checkEquals(vr["par"], character(0)) 
	checkTrue(is(vr["model"], "wrapped.model")) 
	checkTrue(is(vr["model"]["learner.model"], "novars")) 
	
	wl = make.varsel.wrapper("classif.lda", resampling=inner, control=ctrl)
	outer = make.res.desc("cv", iter=2)
	be = bench.exp(wl, task=multiclass.task, resampling=outer)
	
}

