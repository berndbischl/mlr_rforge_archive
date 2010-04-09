#' @include wrapped.learner.regr.r
roxygen()

  
setClass(
		"kernlab.svm.regr", 
		contains = c("wrapped.learner.regr")
)



setMethod(
		f = "initialize",
		signature = signature("kernlab.svm.regr"),
		def = function(.Object, parset) {
			
			desc = new("regr.props",
					supports.missing = FALSE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.weights = FALSE
			)
			
			callNextMethod(.Object, learner.name="SVM", learner.pack="kernlab", learner.props=desc, parset=parset)
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.wrapped.learner="kernlab.svm.regr", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="missing", 
				.type = "missing" 
		),
		
		def = function(.wrapped.learner, .targetvar, .data, .weights, ...) {
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
				.wrapped.learner = "kernlab.svm.regr", 
				.wrapped.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "missing" 
		),
		
		def = function(.wrapped.learner, .wrapped.model, .newdata, ...) {
			predict(.wrapped.model["learner.model"], newdata=.newdata, ...)[,1]
		}
)	



