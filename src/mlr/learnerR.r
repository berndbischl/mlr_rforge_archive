#' @include object.r
roxygen()
#' @include Learner.R
roxygen()

#' Wraps an already implemented learning method from R to make it accessible to mlr.
#'  
#' @exportClass rlearner
#' @title Base class for inducers. 

setClass(
  "rlearner",
  contains = c("Learner"),
  representation = representation(
    missings="logical",
    feat="logical",
    weights="logical"
  )    
)




#---------------- constructor---- -----------------------------------------------------


#' Base class for classification learners.
#' @exportClass rlearner.classif
#' @title Base class for classification learners. 
setClass("rlearner.classif", 
  contains = c("rlearner"),
  representation = representation(
    classes="logical",
    predict="logical",
    costs="logical"
  )  
)

#' Constructor.
#' @title rlearner constructor
setMethod(
  f = "initialize",
  signature = signature("rlearner.classif"),
  def = function(.Object, id, pack, desc, par.set=makeParameterSet(), par.vals=list()) {
    if (missing(desc))
      return(make.empty(.Object))
    if (missing(id))
      id = as.character(class(.Object))
    ns = c("numerics", "factors", "missings", "weights", "oneclass", "twoclass", "multiclass", "decision", "prob", "costs")
    if (!setequal(names(desc), ns))
      stop("Logical description of ", id, " must exactly contain: ", paste(ns, collapse=","))
    .Object = callNextMethod(.Object, id=id, pack=pack, par.set=par.set, par.vals=par.vals)
    .Object@desc@type = "classif"  
    .Object@desc@feat = c(desc["numerics"], desc["factors"])  
    .Object@desc@weights = as.logical(desc["weights"])  
    .Object@desc@missings = as.logical(desc["missings"])
    .Object@desc@classes = c(desc["oneclass"], desc["twoclass"], desc["multiclass"])
    .Object@desc@predict = c(desc["prob"], desc["decision"])
    .Object@desc@costs = as.logical(desc["costs"])
    return(.Object)
  }
)


#' Base class for regression learners.
#' @exportClass rlearner.regr
#' @title Base class for regression learners. 
setClass("rlearner.regr", contains = c("rlearner"))

#' Constructor.
#' @title rlearner constructor
setMethod(
  f = "initialize",
  signature = signature("rlearner.regr"),
  def = function(.Object, id, pack, desc, par.set=makeParameterSet(), par.vals=list()) {
    if (missing(desc))
      return(make.empty(.Object))
    if (missing(id))
      id = as.character(class(.Object))
    ns = c("numerics", "factors", "missings", "weights") 
    if (!setequal(names(desc), ns))
    stop("Logical description of ", id, " must exactly contain: ", paste(ns, collapse=","))
    .Object = callNextMethod(.Object, id=id, pack=pack, par.set=par.set, par.vals=par.vals)
    .Object@desc@type = "regr"  
    .Object@desc@feat = c(desc["numerics"], desc["factors"])  
    .Object@desc@weights = as.logical(desc["weights"])  
    .Object@desc@missings = as.logical(desc["missings"])
    .Object@desc@classes = c(oneclass=FALSE, twoclass=FALSE, multiclass=FALSE)
    .Object@desc@predict = c(prob=FALSE, decision=FALSE)
    .Object@desc@costs = FALSE
    return(.Object)
  }
)




#' @rdname to.string
setMethod(f = "to.string",
  signature = signature("rlearner.classif"),
  def = function(x) {
    pack = paste(x@pack, collapse=",")
    return(paste(
        "Classification learner id=", x@desc@id, " from package ", pack, "\n",
        "Class: ", class(x), "\n",
        "Predict-Type: ", x["predict.type"], "\n",
        "Hyperparameters: ", getParameterValuesString(x), "\n\n",
        "Supported features Doubles:", x@desc@feat["numerics"], " Factors:", x@desc@feat["factors"], "\n",
        "Supports missings: ", x@desc@missings, "\n", 
        "Supports weights: ", x@desc@weights, "\n", 
        "Supports classes: ", paste(c("one", "two", "multi")[x@desc@classes], collapse=","), "\n",
        "Supports probabilities: ", x@desc@predict["prob"], "\n", 
        "Supports decision values: ", x@desc@predict["decision"], "\n", 
        "Supports costs: ", x@desc@costs, "\n", 
        sep =""					
      ))
})

#' @rdname to.string
setMethod(f = "to.string",
  signature = signature("rlearner.regr"),
  def = function(x) {
    pack = paste(x@pack, collapse=",")
    return(paste(
        "Regression learner id=", x@desc@id, " from package ", pack, "\n",
        "Class: ", class(x), "\n",
        "Hyperparameters: ", getParameterValuesString(x), "\n\n",
        "Supported features Doubles:", x@desc@feat["numerics"], " Factors:", x@desc@feat["factors"], "\n",
        "Supports missings: ", x@desc@missings, "\n", 
        "Supports weights: ", x@desc@weights, "\n", 
        sep =""					
      ))
  })



