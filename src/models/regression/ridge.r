#' @include learnerR.r
roxygen()
#' @include task.regr.r
roxygen()


setClass(
		"regr.ridge", 
		contains = c("rlearner.regr")
)


setMethod(
		f = "initialize",
		signature = signature("regr.ridge"),
		def = function(.Object) {

			desc = c(
					missings = TRUE,
					doubles = TRUE,
					factors = TRUE,
					weights = FALSE
			)
      par.set = list(
        numeric.learner.parameter(id="lambda2", default=0, lower=0)
      )
			callNextMethod(.Object, pack="penalized", desc=desc, par.set=par.set)
		}
)



#' @rdname train.learner


setMethod(
		f = "train.learner",
		signature = signature(
				.learner="regr.ridge", 
				.task="regr.task", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset, ...) {
      f = .task["formula"]
      penalized(f, data=get.data(.task, .subset), ...)
    }
)


#' @rdname pred.learner

setMethod(
		f = "pred.learner",
		signature = signature(
				.learner = "regr.ridge", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "missing" 
		),
		
		def = function(.learner, .model, .newdata, ...) {
			m <- .model["learner.model"]
			.newdata[, .model["target"]] <- 0
			predict(m, data=.newdata,  ...)[,"mu"]
		}
)	

