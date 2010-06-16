test.varsel <- function() {
	inner = make.res.desc("cv", iter=2)

	ctrl = seq.control(method="sfs", alpha=0.01)
	vr = varsel("classif.lda", task=multiclass.task, resampling=inner, control=ctrl, path=T)
	checkTrue(length(vr["path"]) > 1) 
	
	ctrl = seq.control(method="sbs", beta=0.01)
	vr = varsel("classif.lda", task=multiclass.task, resampling=inner, control=ctrl, path=T)
	checkTrue(length(vr["path"]) > 1) 
	
	# check empty model
	ctrl = seq.control(method="sfs", alpha=10)
	vr = varsel("classif.lda", task=multiclass.task, resampling=inner, control=ctrl, model=T)
	checkEquals(vr["par"], character(0)) 
	checkTrue(is(vr["model"], "wrapped.model")) 
	checkTrue(is(vr["model"]["learner.model"], "novars")) 
	
	wl = make.varsel.wrapper("classif.lda", resampling=inner, control=ctrl)
	outer = make.res.desc("cv", iter=2)
	be = bench.exp(wl, task=multiclass.task, resampling=outer)
}

