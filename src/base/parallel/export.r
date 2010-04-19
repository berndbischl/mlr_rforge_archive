



#export.resample.fit = function(learner, task, resample.instance, parset, vars, type, extract) {
##	if (!is.null(parent.frame()$caller) && !parent.frame()$caller == "tune") {
#		
#	export(".mlr.learner", learner)
#	export(".mlr.task", task)
#	export(".mlr.rin", resample.instance)
#	export(".mlr.parset", parset)
#	export(".mlr.vars", vars)
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
	assign(name, obj, envir = .GlobalEnv)
	if (.mlr.local$parallel.setup$mode != "local") {
		if (is(obj, "wrapped.learner")) {
			sfClusterEval(require(obj@pack ,character.only=TRUE))
		}
		sfExport(name)
	}
}