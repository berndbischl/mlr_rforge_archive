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
  contains = c("Learner")
)




#---------------- constructor---- -----------------------------------------------------


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
    pack = paste(x@pack, collapse=",")
    return(paste(
        "Classification learner id='", x@id, "' from package ", pack, "\n",
        "Class: ", class(x), "\n",
        "Predict-Type: ", x["predict.type"], "\n",
        "Hyperparameters: ", getParameterValuesString(x), "\n\n",
        "Supported features Doubles:", getProperty(x, "numerics"), " Factors:", getProperty(x, "factors"), "\n",
        "Supports missings: ", getProperty(x, "missings"), "\n", 
        "Supports weights: ",getProperty(x, "weights"), "\n", 
        "Supports classes: ", 
          paste(c("one", "two", "multi")
          [c(getProperty(x, "oneclass"), getProperty(x, "twoclass"), getProperty(x, "multiclass"))],  
          collapse=","), "\n",
        "Supports probabilities: ", getProperty(x, "prob"), "\n", 
        "Supports decision values: ", getProperty(x, "decision"), "\n", 
        "Supports costs: ", getProperty(x, "costs"), "\n", 
        sep =""					
      ))
})

#' @rdname to.string
setMethod(f = "to.string",
  signature = signature("rlearner.regr"),
  def = function(x) {
    pack = paste(x@pack, collapse=",")
    return(paste(
        "Regression learner id='", x@id, "' from package ", pack, "\n",
        "Class: ", class(x), "\n",
        "Hyperparameters: ", getParameterValuesString(x), "\n\n",
        "Supported features Doubles:", getProperty(x, "numerics"), " Factors:", getProperty(x, "factors"), "\n",
        "Supports missings: ", getProperty(x, "missings"), "\n", 
        "Supports weights: ", getProperty(x, "weights"), "\n", 
        sep =""					
      ))
  })



