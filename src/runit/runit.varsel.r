test.varsel <- function() {
	inner = make.res.desc("cv", iter=2)
	ctrl = varsel.control(alpha=0.01)
	vr = varsel("classif.lda", task=multiclass.task, resampling=inner, method="sfs", control=ctrl)

	vr = varsel("classif.lda", task=multiclass.task, resampling=inner, method="sbs", control=ctrl)
	checkTrue(length(vr["path"]) > 1) 
	
	wl = make.varsel.wrapper("classif.lda", resampling=inner, method="sfs", control=ctrl)
	outer = make.res.desc("cv", iter=2)
	be = bench.exp(wl, task=multiclass.task, resampling=outer)
}

