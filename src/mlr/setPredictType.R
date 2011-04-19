#' Set the type of prediction the classification learner object returns.
#'
#' Possible prediction types are classes, class probabilities or decision values.
#' 
#' @param learner [\code{\linkS4class{Learner}}]\cr 
#'        Learner object.   
#' @param type [\code{character(1)}] \cr
#'        Classification: \dQuote{response}, \dQuote{prob} or \dQuote{decision},
#'        specifying the type to predict. Default is \dQuote{response}.
#'        \dQuote{decision} is experimental. Ignored for regression.
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
  def = function(learner, type) {
    standardGeneric("setPredictType")
  }
)

#' @rdname setPredictType 
setMethod(
  f = "setPredictType",
  
  signature = signature(
    learner="Learner", 
    type="character" 
  ),
  
  def = function(learner, type) {
    if (type != "response" && learner@properties[["type"]] != "classif") {
      stop("Trying to predict ", type, ", but only classifiers support that!")
    }
    if ("prob" == type && !learner@properties[["prob"]]) {
      stop("Trying to predict probs, but ", learner@id, " does not support that!")
    }
    if ("decision" == type && !learner@properties[["decision"]]) {
      stop("Trying to predict decision values, but ", learner@id,
           " does not support that!")
    }
    learner@predict.type = type
    return(learner)
  }
)
