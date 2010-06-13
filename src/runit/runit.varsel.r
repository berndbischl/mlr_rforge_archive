test.varsel <- function() {
	inner = make.res.desc("cv", iter=2)
	ctrl = varsel.control(alpha=0.01)
	vr = varsel("classif.lda", task=multiclass.task, resampling=inner, method="sfs", control=ctrl)

	vr = varsel("classif.lda", task=multiclass.task, resampling=inner, method="sbs", control=ctrl)
	checkTrue(length(vr["path"]) > 1) 
	
	# check empty model
	ctrl = varsel.control(alpha=10)
	vr = varsel("classif.lda", task=multiclass.task, resampling=inner, control=ctrl, model=T)
	checkEquals(vr["par"], character(0)) 
	checkTrue(is(vr["model"], "wrapped.model")) 
	checkTrue(is(vr["model"]["learner.model"], "novars")) 
	
	wl = make.varsel.wrapper("classif.lda", resampling=inner, method="sfs", control=ctrl)
	outer = make.res.desc("cv", iter=2)
	be = bench.exp(wl, task=multiclass.task, resampling=outer)
}

