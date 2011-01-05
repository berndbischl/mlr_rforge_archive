#' @include base.wrapper.r

setClass(
		"postproc.wrapper",
		contains = c("base.wrapper"),
		representation = representation(
				fun = "function"
		)
)

#' Constructor.

setMethod(
		f = "initialize",
		signature = signature("postproc.wrapper"),
		def = function(.Object, learner, fun, args) {
      .Object@fun = fun
      pds = list()
      for (i in seq(length=length(args))) {
        n = names(args)[i]
        pds[[i]] = new("par.desc.unknown", par.name=n, when="predict")
      }
      callNextMethod(.Object, learner=learner, par.descs=pds, par.vals=args) 
		}
)


#' Fuses a base learner with a postprocessing method. Creates a learner object, which can be
#' used like any other learner object, but which internally postprocesses the prediction as requested. 
#'
#' @param learner [\code{\linkS4class{learner}} or string]\cr 
#'        Learning algorithm. See \code{\link{learners}}.  
#' @param fun [function] \cr
#'        Function to postprocess a prediction object. First argument must be called 'pred', which will be postprocessed and subsequently returned.
#' @param ... [any] \cr
#'        Optional parameters to control the postprocessing. Passed to fun.   
#' 
#' @return \code{\linkS4class{learner}}.
#' 
#' @title Fuse learner with postprocessing.
#' @export

make.postproc.wrapper = function(learner, fun, args, ...) {
  if (is.character(learner))
    learner = make.learner(learner)
  if (missing(args))
    args=list()
  if (any(names(formals(fun)) != c("pred", "args")))
    stop("Arguments in postproc function have to be: pred, args")   
	new("postproc.wrapper", learner=learner, fun=fun, args=args)
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
      myargs = .learner["par.vals", par.top.wrapper.only=TRUE]
      p = .learner@fun(p, myargs)
		}
)	


