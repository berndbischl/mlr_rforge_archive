#' @include base.wrapper.r

setClass(
		"preproc.wrapper",
		contains = c("base.wrapper"),
		representation = representation(
        fun = "function",
        control.fun = "function",
        control = "list"
    )
)

#' Constructor.

setMethod(
		f = "initialize",
		signature = signature("preproc.wrapper"),
		def = function(.Object, learner, id, fun, control, par.descs, par.vals) {
			.Object@fun = fun
      .Object@control.fun = control
      callNextMethod(.Object, learner=learner, id=id, par.descs=par.descs, par.vals=par.vals)
		}
)


#' Fuses a base learner with a preprocessing method. Creates a learner object, which can be
#' used like any other learner object, but which internally preprocesses the data as requested. 
#' If the train or predict function is called on it, the preprocessing is always invoked before.
#'
#' @param learner [\code{\linkS4class{learner}} or string]\cr 
#'        Learning algorithm. See \code{\link{learners}}.  
#' @param id [string] \cr
#'        Id for resulting learner object. If missing, id of "learner" argument is used.
#' @param fun [function] \cr
#'        Function to preprocess a data.frame. First argument must be called 'data', which will be preprocessed and subsequently returned.
#' @param ... [any] \cr
#'        Optional parameters to control the preprocessing. Passed to fun.   
#' 
#' @return \code{\linkS4class{learner}}.
#' 
#' @title Fuse learner with preprocessing.
#' @export

make.preproc.wrapper = function(learner, id=as.character(NA), fun, control, args) {
	if (is.character(learner))
		learner = make.learner(learner)
  if (missing(control))
    control=function(data, targetvar, args) NULL
  if (missing(args))
    args=list()
  if (any(names(formals(fun)) != c("data", "targetvar", "args", "control")))
		stop("Arguments in preproc function have to be: data, targetvar, args, control")		
  if (any(names(formals(control)) != c("data", "targetvar", "args")))
    stop("Arguments in control function have to be: data, targetvar, args")    
	pds = list()
	pvs = list()
	for (i in seq(length=length(args))) {
		n = names(args)[i]
		p = args[[i]]
		pds[[i]] = new("par.desc.unknown", par.name=n, when="both", default=p)
		pvs[[n]] = p
	}
	new("preproc.wrapper", learner=learner, id=id, fun=fun, control=control, par.descs=pds, par.vals=pvs)
}


#' @rdname train.learner

setMethod(
		f = "train.learner",
    signature = signature(
      .learner="preproc.wrapper", 
      .task="learn.task", .subset="integer"
    ),
      
		def = function(.learner, .task, .subset,  ...) {
      d = get.data(.task, .subset)
      fargs = .learner["par.vals", par.top.wrapper.only=TRUE]
      tn = .task["target"]
      ctrl = .learner@control.fun(data=d, targetvar=tn, args=fargs)
      xx <<- ctrl
      d = .learner@fun(data=d, targetvar=tn, args=fargs, control = ctrl)
      if (!is.data.frame(d))
        stop("Preprocessing must result in a data.frame!")
      if (nrow(d) != .task["size"])
        stop("Preprocessing may not change number of cases!")
      .task = change.data(.task, d)
			m = callNextMethod(.learner, .task, .subset, .vars, ...)
      attr(m, "control") = ctrl
      return(m)
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
      fargs = .model@learner["par.vals", par.top.wrapper.only=TRUE]
      tn = .model["task.desc"]["target"]
      m = nrow(.newdata)
      .newdata = .learner@fun(data=.newdata, targetvar=tn, args=fargs, control=.learner@control)
      if (!is.data.frame( .newdata))
        stop("Preprocessing must result in a data.frame!")
      if (nrow(.newdata) != m)
        stop("Preprocessing may not change number of cases!")
			callNextMethod(.learner, .model, .newdata, .type, ...)
		}
)	





