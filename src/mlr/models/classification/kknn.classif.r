#' @include learnerR.r
roxygen()
#' @include WrappedModel.R
roxygen()
#' @include trainLearner.R
roxygen()
#' @include predictLearner.R
roxygen()
#' @include ClassifTask.R
roxygen()

setClass(
		"classif.kknn", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.kknn"),
		def = function(.Object) {
			#todo: find out what ykernel and contrasts really do 
			par.set = makeParamSet(
        makeIntegerLearnerParam(id="k", default=7L, lower=1L),
				makeNumericLearnerParam(id="distance", default=2, lower=0),
				makeDiscreteLearnerParam(id="kernel", default="triangular", 
						vals=list("rectangular", "triangular", "epanechnikov", "biweight", "triweight", "cos", "inv", "gaussian"))
			)
			
      .Object = callNextMethod(.Object, pack="kknn", par.set=par.set)
    
      setProperties(.Object, 
        oneclass = FALSE,
        twoclass = TRUE,
        multiclass = TRUE,
        missings = FALSE,
        numerics = TRUE,
        factors = TRUE,
        prob = TRUE,
        weights = FALSE
      )
    }
)

#' @rdname trainLearner

setMethod(
		f = "trainLearner",
		signature = signature(
				.learner="classif.kknn", 
				.task="ClassifTask", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {
			list(td=.task@desc, data=getData(.task, .subset), parset=list(...))
		}
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "classif.kknn", 
				.model = "WrappedModel", 
				.newdata = "data.frame" 
		),
		
		def = function(.learner, .model, .newdata, ...) {
			m = .model@learner.model
			f = getFormula(.model@task.desc)
			# this is stupid but kknn forces it....
			.newdata[, m$td@target] <- 0
			pars <- list(formula=f, train=m$data, test=.newdata)  
			pars <- c(pars, m$parset, list(...))
			m <- do.call(kknn, pars)
			if (.learner@predict.type == "response")
				return(m$fitted.values)
			else 
				return(m$prob)
		}
)	




