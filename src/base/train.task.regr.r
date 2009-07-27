#' @include task.regr.r
#' @include train.task.r
#' @include wrapped.model.r
roxygen()


#' @export
setMethod(
		f = "train",
		signature = c(learn.task="regr.task", subset="integer", parset="list"),
		def = function(learn.task, subset, parset) {
			m <- callNextMethod(learn.task, subset, parset)
			class(m) <- "wrapped.regr.model"
			return(m)
		}
)
