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
      par.set = makeParamSet(
        makeIntegerLearnerParam(id="k", default=7L, lower=1L),
        makeNumericLearnerParam(id="distance", default=2, lower=0),
        makeDiscreteLearnerParam(id="kernel", default="triangular", 
          values=list("rectangular", "triangular", "epanechnikov", "biweight", "triweight", "cos", "inv", "gaussian"))
      )
			.Object = callNextMethod(.Object, pack="kknn", par.set=par.set)
      
      setProperties(.Object,
        missings = FALSE,
        numerics = TRUE,
        factors = TRUE,
        se.fit = FALSE,
        weights = FALSE
      )
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
			list(td=.task@desc, data=getData(.task, .subset), parset=list(...))
		}
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "regr.kknn", 
				.model = "WrappedModel", 
				.newdata = "data.frame" 
		),
		
		def = function(.learner, .model, .newdata, ...) {
			m <- .model@learner.model
      f = getFormula(.model@task.desc)
      # this is stupid but kknn forces it....
			.newdata[, m$td@target] <- 0
			pars <- list(formula=f, train=m$data, test=.newdata)  
			pars <- c(pars, m$parset, list(...))
			m <- do.call(kknn, pars)
			return(m$fitted.values)
		}
)	



