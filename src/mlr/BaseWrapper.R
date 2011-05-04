#' @include Learner.R
roxygen()
#' @include setId.R
roxygen()
#' @include setHyperPars.R
roxygen()
#' @include setPredictType.R
roxygen()
#' @include trainLearner.R
roxygen()
#' @include predictLearner.R
roxygen()


#' Abstract base class to wrap something around a learner.
#' @exportClass BaseWrapper

setClass(
		"BaseWrapper",
		contains = c("Learner"),
		representation = representation(
			learner = "Learner"
		)
)


#' Constructor.

setMethod(
		f = "initialize",
		signature = signature("BaseWrapper"),
		def = function(.Object, learner, pack=character(0), par.set, par.vals=list()) {
			if (missing(learner))
				return(make.empty(.Object))
      if (is(learner, "OptWrapper")) 
        stop("Cannot wrap an optimization wrapper with something else!")
      ns = intersect(names(par.set@pars), names(learner@par.set@pars))
      if (length(ns) > 0)
        stop("Hyperparameter names in wrapper clash with base learner names: ", paste(ns, collapse=","))
			.Object@learner = learner
      .Object@pack = c(pack, learner@pack) 
      .Object = callNextMethod(.Object, par.set=par.set, par.vals=par.vals, pack=pack)
      .Object@properties = learner@properties 
      return(.Object)
		}
)



#' @rdname BaseWrapper-class

setMethod(
		f = "[",
		signature = signature("BaseWrapper"),
		def = function(x,i,j,...,drop) {
      
      # these belong to BaseWrapper and can be different from basic rlearner 
			if(i %in% c("id", "learner", "predict.type"))
				return(callNextMethod())
      
      return(x@learner[i])
		}
)


#' @rdname trainLearner
setMethod(
  f = "trainLearner",
  signature = signature(
    .learner="BaseWrapper", 
    .task="LearnTask", .subset="integer"
  ),
  
  def = function(.learner, .task, .subset,  ...) {
    trainLearner(.learner@learner, .task, .subset, ...)
  }
)

#' @rdname predictLearner
setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "BaseWrapper", 
				.model = "WrappedModel", 
				.newdata = "data.frame", 
				.type = "ANY" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			predictLearner(.learner@learner, .model, .newdata, .type, ...)
		}
)	

#' @rdname getParameterSet
setMethod(
  f = "getParameterSet",
  signature = signature(learner="BaseWrapper"), 
  def = function(learner) {
    c(learner@par.set, getParameterSet(learner@learner))
  } 
)


#' @rdname setHyperPars 
setMethod(
	f = "setHyperPars",
	
	signature = signature(
			learner="BaseWrapper", 
			par.vals="list" 
	),
	
	def = function(learner, ..., par.vals=list()) {
		ns = names(par.vals)
		pds.n = names(learner@par.set@pars)
		for (i in seq(length=length(par.vals))) {
			if (ns[i] %in% pds.n) {
				learner = callNextMethod(learner, par.vals=par.vals[i])
			} else {	
				learner@learner = setHyperPars(learner@learner, par.vals=par.vals[i])
			}
		}
		return(learner)
	} 
)


#' @rdname to.string
setMethod(f = "to.string",
  signature = signature("BaseWrapper"),
  def = function(x) {
    s = ""
    y = x 
    while (is(y, "BaseWrapper")) {
      s = paste(s, class(y), "->", sep="")
      y = y@learner
    }
    s = paste(s, class(y))
    
    return(paste(
        s, "\n",
        "Hyperparameters: ", getHyperParsString(x), "\n\n",
        sep = ""         
      ))
  })


#' @rdname getHyperPars
setMethod(
  f = "getHyperPars",
  signature = signature(learner="BaseWrapper", for.fun="character"), 
  def = function(learner, for.fun) {
    c(getHyperPars(learner@learner, for.fun), getHyperParsTop(learner, for.fun))
  }
)


