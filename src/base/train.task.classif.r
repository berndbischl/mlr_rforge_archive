#' @include train.task.r
#' @include wrapped.model.r
roxygen()

#' @export
setMethod(
		f = "train",
		signature = signature(learn.task="classif.task", subset="numeric", parset="list", vars="character"),
		def = function(learn.task, subset, parset, vars) {
			wl <- learn.task@wrapped.learner
			if (learn.task@type == "prob") {
				wl <- do.call(set.train.par, c(list(wl), wl@train.par.for.probs))
			} else { 
				wl <- do.call(set.train.par, c(list(wl), wl@train.par.for.classes))
			}
			m <- train.generic(learn.task, wl, subset, parset, vars)
			class(m) <- "wrapped.classif.model"
			return(m)
		}
)
