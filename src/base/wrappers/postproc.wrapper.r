#' @include base.wrapper.r

setClass(
		"postproc.wrapper",
		contains = c("base.wrapper"),
		representation = representation(
				fun = "function",
				defaults = "list"
		)
)

#' Constructor.

setMethod(
		f = "initialize",
		signature = signature("postproc.wrapper"),
		def = function(.Object, learner, id, fun, ...) {
			.Object = set.hyper.pars(.Object, parset=list(...))
			.Object@fun = fun
			callNextMethod(.Object, learner, id=id)
		}
)


#' Fuses a base learner with a postprocessing method. Creates a learner object, which can be
#' used like any other learner object, but which internally postprocesses the prediction as requested. 
#'
#' @param learner [\code{\linkS4class{learner}} or string]\cr 
#'        Learning algorithm. See \code{\link{learners}}.  
#' @param id [string] \cr
#'        Id for resulting learner object. If missing, id of "learner" argument is used.
#' @param fun [function] \cr
#'        Function to postprocess a prediction object. First argument must be called 'pred', which will be postprocessed and subsequently returned.
#' @param ... [any] \cr
#'        Optional parameters to control the postprocessing. Passed to fun.   
#' 
#' @return \code{\linkS4class{learner}}.
#' 
#' @title Fuse learner with postprocessing.
#' @export

make.postproc.wrapper = function(learner, id=as.character(NA), fun, ...) {
	if (is.character(learner))
		learner = make.learner(learner)
	new("postproc.wrapper", learner=learner, id=id, fun=fun, ...)
}


#' @rdname pred.learner

setMethod(
		f = "pred.learner",
		signature = signature(
				.learner = "postproc.wrapper", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			p = callNextMethod(.learner, .model, .newdata, .type, ...)
			fun.args = .learner["hyper.pars"]
			fun.args$pred = p
			p = do.call(.learner@fun, fun.args)  		
		}
)	





