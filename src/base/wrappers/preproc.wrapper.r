#' @include base.wrapper.r

setClass(
		"preproc.wrapper",
		contains = c("base.wrapper"),
		representation = representation(
        train = "function",
        predict = "function"
    )
)

#' Constructor.

setMethod(
		f = "initialize",
		signature = signature("preproc.wrapper"),
		def = function(.Object, learner, train, predict, par.descs, par.vals) {
			.Object@train = train
      .Object@predict = predict
      callNextMethod(.Object, learner=learner, par.descs=par.descs, par.vals=par.vals)
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
#' @title Fuse learner with preprocessing.
#' @export

make.preproc.wrapper = function(learner, train, predict, args) {
	if (is.character(learner))
		learner = make.learner(learner)
  if (missing(args))
    args=list()
  if (any(names(formals(train)) != c("data", "targetvar", "args")))
		stop("Arguments in preproc train function have to be: data, targetvar, args")		
  if (any(names(formals(predict)) != c("data", "targetvar", "args", "control")))
    stop("Arguments in preproc predict function have to be: data, targetvar, args, control")    
	pds = list()
	pvs = list()
	for (i in seq(length=length(args))) {
		n = names(args)[i]
		p = args[[i]]
		pds[[i]] = new("par.desc.unknown", par.name=n, when="both", default=p)
		pvs[[n]] = p
	}
	new("preproc.wrapper", learner=learner, train=train, predict=predict, par.descs=pds, par.vals=pvs)
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
      p = .learner@train(data=d, targetvar=tn, args=fargs)
      if (!(is.list(p) && length(p)==2 && all(names(p) == c("data", "control")) 
          && is.data.frame(p$data) && is.list(p$control)))
        stop("Preprocessing train must result in list wil elements data[data.frame] and control[list]!")
      if (nrow(p$data) != length(.subset))
        stop("Preprocessing train may not change number of cases!")
      .task = change.data(.task, p$data)
			m = callNextMethod(.learner, .task, .subset, ...)
      attr(m, "control") = p$control
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
      .newdata = .learner@predict(data=.newdata, targetvar=tn, args=fargs, control=.model@control)
      if (!is.data.frame( .newdata))
        stop("Preprocessing must result in a data.frame!")
      if (nrow(.newdata) != m)
        stop("Preprocessing predict may not change number of cases!")
			callNextMethod(.learner, .model, .newdata, .type, ...)
		}
)	

setClass(
  "preproc.model",
  contains = c("wrapped.model"),
  representation = representation(
    control = "list"
  )
)

setMethod(
  f = "initialize",
  signature = signature("preproc.model"),
  def = function(.Object, learner, model, task.desc, prep.control, subset, vars, time, control) {
    .Object@control = control
    callNextMethod(.Object, learner, model, task.desc, prep.control, subset, vars, time)
  }
)




