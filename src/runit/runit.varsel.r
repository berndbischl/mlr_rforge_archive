test.varsel <- function() {
	inner = make.res.desc("cv", iter=2)
	ctrl = varsel.control(alpha=0.003)
	vr = varsel("classif.lda", task=multiclass.task, resampling=inner, method="sfs", control=ctrl)
	
	wl = make.varsel.wrapper("classif.lda", resampling=inner, method="sfs", control=ctrl)
	outer = make.res.desc("cv", iter=2)
	be = bench.exp(wl, task=ct, resampling=outer)
}

