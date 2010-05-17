#' @include learner.r
#' @include train.learner.r
#' @include predict.learner.r

setClass(
		"base.wrapper",
		contains = c("learner"),
		representation = representation(
			learner = "learner"
		)
)


#' Getter.
#' @rdname base.wrapper-class

setMethod(
		f = "[",
		signature = signature("base.wrapper"),
		def = function(x,i,j,...,drop) {
			if (i == "learner")
				return(x@learner)
			if (i %in% c("hyper.pars", "hyper.types", "hyper.names"))
				return(callNextMethod())
			else
				return(x@learner[i])
		}
)



setMethod(
		f = "initialize",
		signature = signature("base.wrapper"),
		def = function(.Object, learner) {
			if (missing(learner))
				return(.Object)
			.Object@learner = learner
			.Object@hyper.pars = insert(.Object@hyper.pars, learner["hyper.pars"])
			.Object@hyper.types = insert(.Object@hyper.types, learner["hyper.types"])
			return(.Object)
		}
)


#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="base.wrapper", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="matrix" 
		),
		
		def = function(.learner, .targetvar, .data, .weights, .costs,  ...) {
			args = list(...)
			args.ns = names(args)
			hps.types = .learner["hyper.types"]
			exclude = names(hps.types)[hps.types != "train"]
			include = args.ns[!(args.ns %in% exclude)]
			ps = wl["hyper.pars", type="train"]
			ps = insert(ps, args, el.names=include)
			f.args = list(.learner@learner, .targetvar, .data, .weights, .costs)
			f.args = c(f.args, ps)
			do.call(train.learner, f.args)
		}
)

#' @rdname predict.learner

setMethod(
		f = "predict.learner",
		signature = signature(
				.learner = "base.wrapper", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			args = list(.learner@learner, .model, .newdata, .type)
			#args = c(args, .learner["hyper.pars", type="predict"])
			args = c(args, list(...))
			do.call(predict.learner, args)
		}
)	



