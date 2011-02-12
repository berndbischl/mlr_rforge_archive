#' @include learner.r
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
#' @exportClass base.wrapper

setClass(
		"base.wrapper",
		contains = c("learner"),
		representation = representation(
			learner = "learner"
		)
)


#' Constructor.

setMethod(
		f = "initialize",
		signature = signature("base.wrapper"),
		def = function(.Object, learner, par.set, par.vals=list(), pack=as.character(c())) {
			if (missing(learner))
				return(make.empty(.Object))
      ns = intersect(names(par.set@pars), names(learner@par.set@pars))
      if (length(ns) > 0)
        stop("Hyperparameter names in wrapper clash with base learner names: ", paste(ns, collapse=","))
			.Object@learner = learner
      callNextMethod(.Object, id=learner@id, par.set=par.set, par.vals=par.vals, pack=pack)
		}
)



#' @rdname base.wrapper-class

setMethod(
		f = "[",
		signature = signature("base.wrapper"),
		def = function(x,i,j,...,drop) {
      
      head = list(...)$head
      if (is.null(head)) 
        head = FALSE
      
      # these belong to base.wrapper and can be different from basic rlearner 
			if(i %in% c("id", "learner", "predict.type", "par.vals.string"))
				return(callNextMethod())
      
      if(i == "pack") {
				return(c(x@learner["pack"], x@pack))
			}			
			if(i == "par.set") {
        if (head)
          return(callNextMethod())
        else
          return(c(x@learner@par.set, callNextMethod()))
			}
      if(i == "par.vals") {
        if (head)
          return(callNextMethod())
        else
          return(c(x@learner["par.vals"], callNextMethod()))
      }
      if(i == "par.train") {
        return(c(x@learner["par.train"], callNextMethod()))
      }
      if(i == "par.predict") {
        return(c(x@learner["par.predict"], callNextMethod()))
      }
      if(i == "leaf.learner") {
        y = x@learner 
        while (is(y, "base.wrapper")) 
          y = y@learner
        return(y)  
      }
      return(x@learner[i])
		}
)


#' @rdname trainLearner
setMethod(
  f = "trainLearner",
  signature = signature(
    .learner="base.wrapper", 
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
				.learner = "base.wrapper", 
				.model = "WrappedModel", 
				.newdata = "data.frame", 
				.type = "ANY" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			predictLearner(.learner@learner, .model, .newdata, .type, ...)
		}
)	

#' @rdname setHyperPars 
setMethod(
	f = "setHyperPars",
	
	signature = signature(
			learner="base.wrapper", 
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
  signature = signature("base.wrapper"),
  def = function(x) {
    s = ""
    y = x 
    while (is(y, "base.wrapper")) {
      s = paste(s, class(y), "->", sep="")
      y = y@learner
    }
    s = paste(s, class(y))
    
    return(paste(
        s, "\n",
        "Hyperparameters: ", x["par.vals.string"], "\n\n",
        sep = ""         
      ))
  })



