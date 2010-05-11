#' @include wrapped.learner.regr.r
roxygen()


setClass(
		"regr.lasso", 
		contains = c("wrapped.learner.regr")
)


setMethod(
		f = "initialize",
		signature = signature("regr.lasso"),
		def = function(.Object, parset) {
			
			desc = new("regr.props",
					supports.missing = TRUE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.weights = FALSE
			)
			
			callNextMethod(.Object, label="Lasso regression", pack="penalized", props=desc, parset=parset)
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.wrapped.learner="regr.lasso", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="missing" 
		),
		
		def = function(.wrapped.learner, .targetvar, .data, .weights, ...) {
			f = as.formula(paste(.targetvar, "~."))
			args = list(...)
			i = which(names(args) == "lambda") 
			if (length(i) > 0) {
				names(args)[i] = "lambda1"
			}
			pars <- list(f, data=.data)
			pars <- c(pars, args)
			do.call(penalized, pars)
		}
)

#' @rdname predict.learner

setMethod(
		f = "predict.learner",
		signature = signature(
				.wrapped.learner = "regr.lasso", 
				.wrapped.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "missing" 
		),
		
		def = function(.wrapped.learner, .wrapped.model, .newdata, ...) {
			m <- .wrapped.model["learner.model"]
			.newdata[, .wrapped.model["target"]] <- 0
			predict(m, data=.newdata,  ...)[,"mu"]
		}
)	




