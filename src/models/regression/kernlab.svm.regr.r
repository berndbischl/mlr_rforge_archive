#' @include rlearner.r
roxygen()

  
setClass(
		"regr.ksvm", 
		contains = c("rlearner.regr")
)



setMethod(
		f = "initialize",
		signature = signature("regr.ksvm"),
		def = function(.Object) {
			
			desc = new("regr.props",
					supports.missing = FALSE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.weights = FALSE
			)
			
			callNextMethod(.Object, label="SVM", pack="kernlab", props=desc)
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="regr.ksvm", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="missing" 
		),
		
		def = function(.learner, .targetvar, .data, .weights, ...) {
			f = as.formula(paste(.targetvar, "~."))
			
			kpar = list()
			args = list(...)
			args.names <- names(args)
			
			kernel.par.names = c("degree", "offset", "scale", "sigma", "order", "length", "lambda")
			
			kpar = list()
			for (k in kernel.par.names) {
				x = args[[k]]
				if (!is.null(x)) {
					kpar[[k]] = x
					args[[k]] = NULL
				}
			}
			
			kargs = list(f, data=.data)
			if (length(kpar) > 0)
	 			kargs$kpar = kpar
			kargs = c(kargs, args)
			do.call(ksvm, kargs)
		}
)

#' @rdname predict.learner

setMethod(
		f = "predict.learner",
		signature = signature(
				.learner = "regr.ksvm", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "missing" 
		),
		
		def = function(.learner, .model, .newdata, ...) {
			predict(.model["learner.model"], newdata=.newdata, ...)[,1]
		}
)	



