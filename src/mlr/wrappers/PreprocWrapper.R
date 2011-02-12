#' @include BaseWrapper.R

setClass(
		"PreprocWrapper",
		contains = c("BaseWrapper"),
		representation = representation(
        train = "function",
        predict = "function"
    )
)

#' Constructor.

setMethod(
		f = "initialize",
		signature = signature("PreprocWrapper"),
		def = function(.Object, learner, train, predict, par.set, par.vals) {
			.Object@train = train
      .Object@predict = predict
      callNextMethod(.Object, learner=learner, par.set=par.set, par.vals=par.vals)
		}
)


#' Fuses a base learner with a preprocessing method. Creates a learner object, which can be
#' used like any other learner object, but which internally preprocesses the data as requested. 
#' If the train or predict function is called on it, the preprocessing is always invoked before.
#'
#' @param learner [\code{\linkS4class{learner}} or string]\cr 
#'   Learning algorithm. See \code{\link{learners}}.  
#' @param fun [function] \cr
#'   Function to preprocess a data.frame. First argument must be called 'data', which will be preprocessed and subsequently returned.
#' @param ... [any] \cr
#'   Optional parameters to control the preprocessing. Passed to fun.   
#' 
#' @return \code{\linkS4class{learner}}.
#' 
#' @title Fuse learner with preprocessing.
#' @export

makePreprocWrapper = function(learner, train, predict, par.set=makeParameterSet(), par.vals=list()) {
	if (is.character(learner))
		learner = makeLearner(learner)
  if (missing(par.set))
    par.set=makeParameterSet()
  if (any(names(formals(train)) != c("data", "targetvar", "args")))
		stop("Arguments in preproc train function have to be: data, targetvar, args")		
  if (any(names(formals(predict)) != c("data", "targetvar", "args", "control")))
    stop("Arguments in preproc predict function have to be: data, targetvar, args, control")    
	new("PreprocWrapper", learner=learner, train=train, predict=predict, par.set=par.set, par.vals=par.vals)
}


#' @rdname trainLearner

setMethod(
		f = "trainLearner",
    signature = signature(
      .learner="PreprocWrapper", 
      .task="LearnTask", .subset="integer"
    ),
      
		def = function(.learner, .task, .subset,  ...) {
      pvs = .learner["par.vals", head=TRUE]
      
      d = get.data(.task, .subset)
      tn = .task["target"]
      p = .learner@train(data=d, targetvar=tn, args=pvs)
      if (!(is.list(p) && length(p)==2 && all(names(p) == c("data", "control")) 
          && is.data.frame(p$data) && is.list(p$control)))
        stop("Preprocessing train must result in list wil elements data[data.frame] and control[list]!")
      if (nrow(p$data) != length(.subset))
        stop("Preprocessing train may not change number of cases!")
      .task = change.data(.task, p$data)
      # we have already subsetted!
			m = trainLearner(.learner@learner, .task, 1:.task["size"], ...)
      attr(m, "control") = p$control
      return(m)
		}
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "PreprocWrapper", 
				.model = "WrappedModel", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
      pvs = .model@learner@par.vals
      m = nrow(.newdata)
      .newdata = .learner@predict(.newdata, .model["desc"]["target"], pvs, .model@control)
      if (!is.data.frame( .newdata))
        stop("Preprocessing must result in a data.frame!")
      if (nrow(.newdata) != m)
        stop("Preprocessing predict may not change number of cases!")
			predictLearner(.learner@learner, .model, .newdata, .type, ...)
		}
)	

setClass(
  "preproc.model",
  contains = c("WrappedModel"),
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




