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


setMethod("show", "rlearner.classif", function(object) {
  pack = paste(object@pack, collapse=",")
  cat(
    "Classification learner id='", object@id, "' from package ", pack, "\n",
    "Class: ", class(object), "\n",
    "Predict-Type: ", object@predict.type, "\n",
    "Hyperparameters: ", getHyperParsString(object), "\n\n",
    "Supported features Doubles:", getProperty(object, "numerics"), " Factors:", getProperty(object, "factors"), "\n",
    "Supports missings: ", getProperty(object, "missings"), "\n", 
    "Supports weights: ",getProperty(object, "weights"), "\n", 
    "Supports classes: ", 
    paste(c("one", "two", "multi")
        [c(getProperty(object, "oneclass"), getProperty(object, "twoclass"), getProperty(object, "multiclass"))],  
      collapse=","), "\n",
    "Supports probabilities: ", getProperty(object, "prob"), "\n", 
    sep =""					
  )
})

setMethod("show", "rlearner.regr", function(object) {
  pack = paste(object@pack, collapse=",")
  cat(
    "Regression learner id='", object@id, "' from package ", pack, "\n",
    "Class: ", class(object), "\n",
    "Predict-Type: ", object@predict.type, "\n",
    "Hyperparameters: ", getHyperParsString(object), "\n\n",
    "Supported features Doubles:", getProperty(object, "numerics"), " Factors:", getProperty(object, "factors"), "\n",
    "Supports missings: ", getProperty(object, "missings"), "\n", 
    "Supports weights: ", getProperty(object, "weights"), "\n", 
    "Supports standard errs: ", getProperty(object, "se"), "\n", 
    sep =""
  )
})


