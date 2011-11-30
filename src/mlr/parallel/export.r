



#export.resample.fit = function(learner, task, ResampleInstance, parset, vars, type, extract) {
##	if (!is.null(parent.frame()$caller) && !parent.frame()$caller == "tune") {
#		
#	export(".mlr.learner", learner)
#	export(".mlr.task", task)
#	export(".mlr.rin", ResampleInstance)
#	export(".mlr.parset", parset)
#	export(".mlr.type", type)
#	export(".mlr.extract", extract)
#}


export.tune <- function(learner, task, loss, scale) {
	export(".mlr.learner", learner)
	export(".mlr.task", task)
	export(".mlr.loss", loss)
	export(".mlr.scale", scale)
}


export <- function(name, obj) {
	doit = TRUE
	# multicore does not require to export because mem is duplicated after fork (still copy-on-write)
	if (.mlr.conf$parallel.setup$mode != "local" && .mlr.conf$parallel.setup$mode != "multicore") {
		if (is(obj, "LearnTask")) {
			hash = digest(list(name, obj))
			if (exists(hash, envir=.mlr.export)) 
				doit = FALSE
			else {
				assign(hash, TRUE, env=.mlr.export)
			}
		}
		if (doit) {
			sfClusterCall(assign, name, obj, env=globalenv())
		}
	}
}