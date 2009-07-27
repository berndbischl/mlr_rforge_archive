#' @include task.classif.r
#' @include train.task.r
#' @include wrapped.model.r
roxygen()

#' @export
setMethod(
		f = "train",
		signature = c(learn.task="classif.task", subset="integer", parset="list"),
		def = function(learn.task, subset, parset) {
			wl <- learn.task@wrapped.learner
			ps <- switch(learn.task@type,
					"class" = wl@train.par.for.classes,
					"prob"  = wl@train.par.for.probs
			)
			parset <- c(parset, ps)
			m <- callNextMethod(learn.task, subset, parset)
			class(m) <- "wrapped.classif.model"
			return(m)
		}
)
