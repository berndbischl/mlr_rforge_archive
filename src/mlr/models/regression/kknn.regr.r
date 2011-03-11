#' @include learnerR.r
roxygen()
#' @include RegrTask.R
roxygen()

setClass(
		"regr.kknn", 
		contains = c("rlearner.regr")
)


setMethod(
		f = "initialize",
		signature = signature("regr.kknn"),
		def = function(.Object) {
			
			desc = c(
					missings = FALSE,
					numerics = TRUE,
					factors = TRUE,
					weights = FALSE
			)
      par.set = makeParameterSet(
        makeIntegerLearnerParameter(id="k", default=7L, lower=1L),
        makeNumericLearnerParameter(id="distance", default=2, lower=0),
        makeDiscreteLearnerParameter(id="kernel", default="triangular", 
          vals=list("rectangular", "triangular", "epanechnikov", "biweight", "triweight", "cos", "inv", "gaussian"))
      )
			callNextMethod(.Object, pack="kknn", desc=desc, par.set=par.set)
		}
)

#' @rdname trainLearner

setMethod(
		f = "trainLearner",
		signature = signature(
				.learner="regr.kknn", 
				.task="RegrTask", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset, ...) {
			list(td=.task@desc, data=get.data(.task, .subset), parset=list(...))
		}
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "regr.kknn", 
				.model = "WrappedModel", 
				.newdata = "data.frame", 
				.type = "missing" 
		),
		
		def = function(.learner, .model, .newdata, ...) {
			m <- .model["learner.model"]
			f = m$td["formula"]
			# this is stupid but kknn forces it....
			.newdata[, m$td["target"]] <- 0
			pars <- list(formula=f, train=m$data, test=.newdata)  
			pars <- c(pars, m$parset, list(...))
			m <- do.call(kknn, pars)
			return(m$fitted.values)
		}
)	



