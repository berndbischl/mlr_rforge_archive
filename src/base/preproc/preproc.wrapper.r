#' @include base.wrapper.r

setClass(
		"preproc.wrapper",
		contains = c("base.wrapper"),
		representation = representation(
				fun = "function",
				defaults = "list"
		)
)


setMethod(
		f = "initialize",
		signature = signature("preproc.wrapper"),
		def = function(.Object, learner, fun, ...) {
			.Object@fun = fun
			.Object = set.hyper.pars(.Object, list(...), type="preproc")
			callNextMethod(.Object, learner)
		}
)


#' Fuses a base learner with a preprocessing method. Creates a learner object, which can be
#' used like any other learner object, but which internally preprocesses the data as requested. 
#' If the train or predict function is called on it, the preprocessing is always invoked before.
#'
#' @param learner [\code{\linkS4class{learner}} or string]\cr 
#'        Learning algorithm. See \code{\link{learners}}.  
#' @param fun [function] \cr
#'        Function to preprocess a data.frame. First argument must be called 'data', which will be preprocessed and subsequently returned.
#' @param ... [any] \cr
#'        Optional parameters to control the preprocessing. Passed to fun.   
#' 
#' @return \code{\linkS4class{learner}}.
#' 
#' @exportClass  preproc.wrapper
#'   
#' @title Fuse learner with preprocessing.
#' @export

make.preproc.wrapper = function(learner, fun, ...) {
	if (is.character(learner))
		learner = make.learner(learner)
	new("preproc.wrapper", learner=learner, fun=fun, ...)
}


#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="preproc.wrapper", 
				.targetvar="character", 
				.data="data.frame", 
				.data.desc="data.desc", 
				.task.desc="task.desc", 
				.weights="numeric", 
				.costs="matrix" 
		),
		
		def = function(.learner, .targetvar, .data, .data.desc, .task.desc, .weights, .costs,  ...) {			
			fun.args = .learner["hyper.pars", type="preproc"]
			fun.args = insert.matching(fun.args, list(...))		
			fun.args$data = .data
			.data = do.call(.learner@fun, fun.args)
			callNextMethod(.learner, .targetvar, .data, .data.desc, .task.desc, .weights, .costs,  ...)
		}
)

#' @rdname pred.learner

setMethod(
		f = "pred.learner",
		signature = signature(
				.learner = "preproc.wrapper", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			fun.args = .model["hyper.pars", type="preproc"]
			fun.args$data = .newdata
			.newdata = do.call(.learner@fun, fun.args)  
			callNextMethod(.learner, .model, .newdata, .type, ...)
		}
)	





