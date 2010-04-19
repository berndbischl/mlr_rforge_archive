#' @include wrapped.learner.regr.r
roxygen()

setClass(
		"regr.kknn", 
		contains = c("wrapped.learner.regr")
)



predict.kknn.model2 <- function(model, newdata, ...) {
}


setMethod(
		f = "initialize",
		signature = signature("regr.kknn"),
		def = function(.Object, parset) {
			
			desc = new("regr.props",
					supports.missing = FALSE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.weights = FALSE
			)
			
			callNextMethod(.Object, label="KKNN", pack="kknn", props=desc, parset=parset)
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.wrapped.learner="regr.kknn", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="missing", 
				.type = "missing" 
		),
		
		def = function(.wrapped.learner, .targetvar, .data, .weights, ...) {
			list(target=.targetvar, data=.data, parset=list(...))
		}
)

#' @rdname predict.learner

setMethod(
		f = "predict.learner",
		signature = signature(
				.wrapped.learner = "regr.kknn", 
				.wrapped.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "missing" 
		),
		
		def = function(.wrapped.learner, .wrapped.model, .newdata, ...) {
			m <- .wrapped.model["learner.model"]
			f <- as.formula(paste(m$target, "~."))
			# this is stupid but kknn forces it....
			.newdata[, m$target] <- 0
			pars <- list(formula=f, train=m$data, test=.newdata)  
			pars <- c(pars, m$parset, list(...))
			m <- do.call(kknn, pars)
			return(m$fitted.values)
		}
)	



