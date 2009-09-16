#' @include train.task.r
#' @include wrapped.model.r
roxygen()


#' @export
setMethod(
		f = "train",
		signature = signature(learn.task="regr.task", subset="numeric", parset="list", vars="character"),
		def = function(learn.task, subset, parset, vars) {
			m <- train.generic(learn.task, learn.task@wrapped.learner, subset, parset, vars)
			class(m) <- "wrapped.regr.model"
			return(m)
		}
)
