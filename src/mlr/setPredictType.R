#' Set the type of prediction the learner should return.
#'
#' Possible prediction types are labels / numeric response or class probabilities.
#' 
#' @param learner [\code{\linkS4class{Learner}}]\cr 
#'        Learner object.   
#' @param predict.type [\code{character(1)}] \cr
#'        Classification: \dQuote{response} or \dQuote{prob},
#'        specifying the type to predict. Default is \dQuote{response}.
#'        Ignored for regression.
#' 		    
#' @return \code{\linkS4class{Learner}} with changed Prediction behaviour.
#'
#' @seealso \code{\link{setThreshold}} to alter the threshold used for prediction.
#'
#' @example
#' cl <- makeLearner("classif.logreg")
#' cl <- setPredictType(cl, "response")
#'
#' @exportMethod setPredictType
#' @rdname setPredictType 
#' @title Set predict type of learner object.
setGeneric(
  name = "setPredictType",
  def = function(learner, predict.type) {
    standardGeneric("setPredictType")
  }
)

#' @rdname setPredictType 
setMethod(
  f = "setPredictType",
  
  signature = signature(
    learner = "Learner", 
    predict.type = "character" 
  ),
  
  def = function(learner, predict.type) {
    if (predict.type != "response" && learner@properties[["type"]] != "classif") {
      stop("Trying to predict ", predict.type, ", but only classifiers support that!")
    }
    if ("prob" == predict.type && !learner@properties[["prob"]]) {
      stop("Trying to predict probs, but ", learner@id, " does not support that!")
    }
    learner@predict.type = predict.type
    return(learner)
  }
)
