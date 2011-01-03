#' @include object.r
roxygen()
#' @include learner.r
roxygen()

#' Wraps an already implemented learning method from R to make it accessible to mlr.
#'  
#' @exportClass rlearner
#' @title Base class for inducers. 

setClass(
		"rlearner",
		contains = c("learner")
)

#' Getter.
#' @rdname rlearner-class

setMethod(
		f = "[",
		signature = signature("rlearner"),
		def = function(x,i,j,...,drop) {
			if (i == "is.classif") {
				return(is(x, "rlearner.classif"))
			}
			if (i == "is.regr") {
				return(is(x, "rlearner.regr"))
			}
			callNextMethod()
		}
)

#---------------- constructor---- -----------------------------------------------------

#' Constructor.
#' @title rlearner constructor
setMethod(
		f = "initialize",
		signature = signature("rlearner"),
		def = function(.Object, id, pack, desc, par.descs=list(), par.vals=list()) {
			if (missing(desc))
				return(.Object)
			if (missing(id))
				id = as.character(class(.Object))
			callNextMethod(.Object, id=id, pack=pack, desc=desc, par.desc=par.descs, par.vals=par.vals)
		}
)

#' Base class for classification learners.
#' @exportClass rlearner.classif
#' @title Base class for classification learners. 
setClass("rlearner.classif", contains = c("rlearner"))


#' Base class for regression learners.
#' @exportClass rlearner.regr
#' @title Base class for regression learners. 
setClass("rlearner.regr", contains = c("rlearner"))


#' @rdname to.string
setMethod(f = "to.string",
  signature = signature("rlearner.classif"),
  def = function(x) {
    hps = x["par.vals"]
    hps.ns = names(hps)
    hps = Map(function(n, v) hyper.par.val.to.name(n,v,x), hps.ns, hps)
    hps = paste(hps.ns, hps, sep="=", collapse=" ")
    pack = paste(x["pack"], collapse=",")
    return(paste(
        "Classification learner id=", x["id"], " from package ", pack, "\n",
        "Class: ", class(x), "\n",
        "Predict-Type: ", x["predict.type"], "\n",
        "Hyperparameters: ", hps, "\n\n",
        "Supported features Doubles:", x["doubles"], " Factors:", x["factors"], "\n",
        "Supports missings: ", x["missings"], "\n", 
        "Supports weights: ", x["weights"], "\n", 
        "Supports multiclass: ", x["multiclass"], "\n",
        "Supports probabilities: ", x["probs"], "\n", 
        "Supports decision values: ", x["decision"], "\n", 
        "Supports costs: ", x["costs"], "\n", 
        sep =""					
      ))
})

#' @rdname to.string
setMethod(f = "to.string",
  signature = signature("rlearner.regr"),
  def = function(x) {
    hps = x["par.vals"]
    hps.ns = names(hps)
    hps = Map(function(n, v) hyper.par.val.to.name(n,v,x), hps.ns, hps)
    hps = paste(hps.ns, hps, sep="=", collapse=" ")
    pack = paste(x["pack"], collapse=",")
    return(paste(
        "Regression learner id=", x["id"], " from package ", pack, "\n",
        "Class: ", class(x), "\n",
        "Hyperparameters: ", hps, "\n\n",
        "Supported features Doubles:", x["doubles"], " Factors:", x["factors"], "\n",
        "Supports missings: ", x["missings"], "\n", 
        "Supports weights: ", x["weights"], "\n", 
        sep =""					
      ))
  })



