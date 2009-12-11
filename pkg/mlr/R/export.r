
export.tune <- function(learner, task, fixed, loss, scale) {
	export(".mlr.learner", learner)
	export(".mlr.task", task)
	export(".mlr.fixed", fixed)
	export(".mlr.loss", loss)
	export(".mlr.scale", scale)
}


export <- function(name, obj) {
	assign(name, obj, envir = .GlobalEnv)
	if (.mlr.local$parallel.setup$mode != "local") {
		if (is(obj, "wrapped.learner")) {
			sfClusterEval(require(obj@learner.pack ,character.only=TRUE))
		}
		sfExport(name)
	}
}