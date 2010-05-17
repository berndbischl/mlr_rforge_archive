#' @include learnerR.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include predict.learner.r
roxygen()


setClass(
		"classif.ksvm", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.ksvm"),
		def = function(.Object) {
			
			desc = new("classif.props",
					supports.multiclass = TRUE,
					supports.missing = FALSE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = TRUE,
					supports.probs = TRUE,
					supports.decision = TRUE,
					supports.weights = FALSE,	
					supports.costs = FALSE 
			)
			
			callNextMethod(.Object, label="SVM", pack="kernlab", props=desc)
		}
)


#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="classif.ksvm", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="matrix" 
		),
		
		# todo custom kernel. freezes? check mailing list
		# todo unify cla + regr, test all sigma stuff
		def = function(.learner, .targetvar, .data, .weights, .costs,  ...) {
			f = as.formula(paste(.targetvar, "~."))
			
			kpar = list()
			args = list(...)
			args.names <- names(args)
			
			kernel.par.names = c("degree", "offset", "scale", "sigma", "order", "length", "lambda")
#			kernel.par.names = c(kernel.par.names, paste("kpar", 0:9, sep=""))
			
			kpar = list()
			for (k in kernel.par.names) {
				x = args[[k]]
				if (!is.null(x)) {
					kpar[[k]] = x
					args[[k]] = NULL
				}
			}
			

			kargs = list(f, data=.data, fit=FALSE) 
			if (length(kpar) > 0)
				kargs$kpar = kpar
			
#			# there's a strange behaviour in r semantics here wgich forces this, see do.call and the comment about substitute
#			if (!is.null(args$kernel) && is.function(args$kernel) && !is(args$kernel,"kernel")) {
#				args$kernel = do.call(args$kernel, kpar)	
#			} 
			
			kargs = c(kargs, args)
			do.call(ksvm, kargs)
		}
)

#' @rdname predict.learner

setMethod(
		f = "predict.learner",
		signature = signature(
				.learner = "classif.ksvm", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			.type <- switch(.type, prob="probabilities", decision="decision", "response")
			predict(.model["learner.model"], newdata=.newdata, type=.type, ...)
		}
)	

